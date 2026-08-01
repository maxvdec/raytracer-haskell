{-# LANGUAGE BangPatterns #-}

module Renders where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (modifyMVar_, newMVar)
import Data.ByteString.Builder (Builder, hPutBuilder)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities)
import Geometry.Hit (Hit (info, material), Hittable (hit, pdfObjectValue), hitNormal, hitP, hitT)
import Geometry.HitInfo (HitInfo (p, uv))
import Geometry.Ray (Ray (Ray, direction, origin, time), at)
import Geometry.ScatterRecord (ScatterRecord (ScatterRecord, attenuation, scatPrimitive))
import Geometry.Scene (Camera (backgroundColor, maxDepth, samplesPerPixel), Lights (mainHittable), World, getSPPProperties, makeRayGenerator)
import Graphics.HittablePDF (makeHittablePDF)
import Graphics.Image (putColor, putColorBuilder)
import Graphics.Materials (Material (emit, scatter, scatteringPDF))
import Graphics.PDF (PDF (CosinePDF, HittablePDF, MixturePDF), generate, getPDFValue)
import Math (Color, ImageCoord, RandomGenerator, Resolution, Vector3 (Vector3), dot, getX, getY, getZ, infinity, lengthSquared, makeRandomGenerator, normalizeColor, randomInHemisphere, randomInRange, ratio, unit, (*.), (.*), (/.))
import System.IO (Handle, hFlush, hPutStr, stdout)

formatDuration :: Integer -> String
formatDuration totalSeconds
    | totalSeconds >= 3600 =
        show hours ++ "h " ++ show minutes ++ "m " ++ show seconds ++ "s"
    | totalSeconds >= 60 =
        show minutes ++ "m " ++ show seconds ++ "s"
    | otherwise = show totalSeconds ++ "s"
  where
    (hours, afterHours) = totalSeconds `divMod` 3600
    (minutes, seconds) = afterHours `divMod` 60

-- UV Render
uvForPos :: ImageCoord -> Resolution -> Color
uvForPos (x, y) (w, h) =
    Vector3 (ratio x w) (ratio y h) 0

-- Render everything
uvRow :: Resolution -> Integer -> Integer -> String
uvRow (w, h) x y
    | x < w =
        putColor (uvForPos (x, y) (w, h))
            ++ uvRow (w, h) (x + 1) y
    | otherwise = ""

uvRows :: Resolution -> Integer -> String
uvRows res@(_, h) y
    | y < h = uvRow res 0 y ++ uvRows res (y + 1)
    | otherwise = ""

uvRender :: Resolution -> String
uvRender res = uvRows res 0

-- Computed image
computedSamples ::
    ( RandomGenerator ->
      Resolution ->
      ImageCoord ->
      Integer ->
      Integer ->
      Float ->
      IO Color
    ) ->
    RandomGenerator ->
    Resolution ->
    ImageCoord ->
    Camera ->
    IO Color
computedSamples f generator res coord cam = do
    let (sqrtSamples, invSqrtSamples) = getSPPProperties cam

        strata =
            [ (sampleX, sampleY)
            | sampleY <- [0 .. sqrtSamples - 1]
            , sampleX <- [0 .. sqrtSamples - 1]
            ]

    colors <-
        mapM
            ( \(sampleX, sampleY) ->
                f
                    generator
                    res
                    coord
                    sampleX
                    sampleY
                    invSqrtSamples
            )
            strata

    pure (sum colors)

computedChunk ::
    ( RandomGenerator ->
      Resolution ->
      ImageCoord ->
      Integer ->
      Integer ->
      Float ->
      IO Color
    ) ->
    IO () ->
    Resolution ->
    Integer ->
    Integer ->
    Camera ->
    IO Builder
computedChunk f reportProgress res@(w, _) firstPixel lastPixel cam = do
    generator <- makeRandomGenerator

    let actualSampleCount = (samplesPerPixel cam)

        computePixel pixelIndex = do
            let (y, x) = pixelIndex `divMod` w

            total <-
                computedSamples
                    f
                    generator
                    res
                    (x, y)
                    cam

            reportProgress

            pure $
                putColorBuilder
                    (total /. fromInteger actualSampleCount)

    pixels <-
        mapM
            computePixel
            [firstPixel .. lastPixel]

    pure (mconcat pixels)

computedImage ::
    (RandomGenerator -> Resolution -> ImageCoord -> Integer -> Integer -> Float -> IO Color) ->
    Handle ->
    Resolution ->
    Camera ->
    IO ()
computedImage f handle res@(w, h) cam = do
    capabilities <- getNumCapabilities
    startTime <- getMonotonicTimeNSec
    progress <- newMVar (0, 0)
    let totalPixels = w * h
        reportProgress =
            modifyMVar_ progress $ \(completed, displayedPercentage) -> do
                let completedPixels = completed + 1
                    percentage = completedPixels * 100 `div` totalPixels
                if percentage > displayedPercentage
                    then do
                        currentTime <- getMonotonicTimeNSec
                        let elapsedNanoseconds = currentTime - startTime
                            remainingPixels = totalPixels - completedPixels
                            remainingSeconds =
                                ceiling
                                    ( fromIntegral elapsedNanoseconds
                                        * fromIntegral remainingPixels
                                        / fromIntegral completedPixels
                                        / 1000000000
                                    )
                        putStr
                            ( show percentage
                                ++ "% rendered | ETA "
                                ++ formatDuration remainingSeconds
                                ++ "\r"
                            )
                        hFlush stdout
                    else pure ()
                pure (completedPixels, max displayedPercentage percentage)
        chunkSize = 32
        batchSize = max 1 (fromIntegral capabilities)
        renderChunks firstPixel
            | firstPixel >= totalPixels = pure ()
            | otherwise = do
                let chunkStarts =
                        take
                            (fromIntegral batchSize)
                            [firstPixel, firstPixel + chunkSize .. totalPixels - 1]
                    renderChunk chunkStart =
                        computedChunk
                            f
                            reportProgress
                            res
                            chunkStart
                            (min (totalPixels - 1) (chunkStart + chunkSize - 1))
                            cam
                chunks <-
                    mapConcurrently
                        renderChunk
                        chunkStarts
                mapM_ (hPutBuilder handle) chunks
                renderChunks (firstPixel + fromIntegral (length chunkStarts) * chunkSize)

    putStr "0% rendered\r"
    hFlush stdout

    renderChunks 0
    putStrLn ""

colorNormal :: Hit -> Color
colorNormal h =
    0.5 .* (hitNormal h + Vector3 1 1 1)

colorGradient :: RandomGenerator -> Camera -> Hit -> World -> Lights -> Integer -> IO Color
colorGradient generator cam h world lights depth = do
    dir <- randomInHemisphere generator (hitNormal h)
    let ray =
            Ray
                { origin = hitP h
                , direction = dir + hitNormal h
                , time = 0
                }
    bounces <- rayColor generator cam ray world lights (depth - 1)
    pure (0.5 .* bounces)

colorScattering :: RandomGenerator -> Camera -> Hit -> Ray -> World -> Lights -> Integer -> IO Color
colorScattering generator cam h r world lights depth = do
    let hitMaterial = material h
        hitInformation = info h
        colorFromEmission = emit hitMaterial (uv hitInformation) hitInformation (p hitInformation)

    scatterResult <- scatter hitMaterial generator r hitInformation

    case scatterResult of
        Nothing ->
            pure colorFromEmission
        Just srec -> do
            colorFromScatter <- handleScattering srec
            pure (colorFromScatter + colorFromEmission)
  where
    handleScattering :: ScatterRecord -> IO Color
    handleScattering rec =
        case (scatPrimitive rec) of
            Left pdf -> performPDFScattering rec pdf
            Right ray -> do
                bounced <- rayColor generator cam ray world lights (depth - 1)
                pure (bounced * (attenuation rec))

    performPDFScattering :: ScatterRecord -> PDF -> IO Color
    performPDFScattering srec pdf = do
        let lightPDF = makeHittablePDF (mainHittable lights) (hitP h)
            mixPDF = MixturePDF lightPDF pdf
        pdfGen <- generate mixPDF generator
        let scattered =
                r
                    { origin = (hitP h)
                    , direction = pdfGen
                    }

        pdfVal <- getPDFValue mixPDF generator (direction scattered)
        let scatPdf = scatteringPDF (material h) r (info h) scattered
        bounceColor <- rayColor generator cam scattered world lights (depth - 1)
        pure (((attenuation srec) *. scatPdf * bounceColor) /. pdfVal)

rayColor ::
    RandomGenerator ->
    Camera ->
    Ray ->
    World ->
    Lights ->
    Integer ->
    IO Color
rayColor _ _ _ _ _ depth
    | depth <= 0 =
        pure (Vector3 0 0 0)
rayColor generator camera r world lights depth = do
    res <- hit world generator r (0.001, infinity)
    case res of
        Just hitted ->
            colorScattering
                generator
                camera
                hitted
                r
                world
                lights
                depth
        Nothing ->
            pure (backgroundColor camera)

rayPass :: Camera -> World -> Lights -> RandomGenerator -> Resolution -> ImageCoord -> Integer -> Integer -> Float -> IO Color
rayPass cam world lights =
    let makeRay = makeRayGenerator cam
     in \generator _ (x, y) si sj invSamplesPerPixel -> do
            ray <- makeRay generator x y si sj invSamplesPerPixel
            rayColor generator cam ray world lights (maxDepth cam)
