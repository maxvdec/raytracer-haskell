{-# LANGUAGE BangPatterns #-}

module Renders where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (modifyMVar_, newMVar)
import Data.ByteString.Builder (Builder, hPutBuilder)
import GHC.Conc (getNumCapabilities)
import GHC.Clock (getMonotonicTimeNSec)
import Geometry.Hit (Hit (info, material), Hittable (hit), hitNormal, hitP, hitT)
import Geometry.HitInfo (HitInfo (p, uv))
import Geometry.Ray (Ray (Ray, direction, origin, time), at)
import Geometry.Scene (Camera (backgroundColor, maxDepth, samplesPerPixel), World, makeRayGenerator)
import Graphics.Image (putColor, putColorBuilder)
import Graphics.Materials (Material (emit, scatter))
import Math (Color, ImageCoord, RandomGenerator, Resolution, Vector3 (Vector3), getX, getY, getZ, infinity, makeRandomGenerator, normalizeColor, randomInHemisphere, ratio, unit, (*.), (.*), (/.))
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
    (RandomGenerator -> Resolution -> ImageCoord -> IO Color) ->
    RandomGenerator ->
    Resolution ->
    ImageCoord ->
    Integer ->
    IO Color
computedSamples f generator res coord sampleCount = do
    accumulate sampleCount (Vector3 0 0 0)
  where
    accumulate 0 total = pure total
    accumulate remaining total = do
        color <- f generator res coord
        let !newTotal = total + color
        accumulate (remaining - 1) newTotal

computedChunk ::
    (RandomGenerator -> Resolution -> ImageCoord -> IO Color) ->
    IO () ->
    Resolution ->
    Integer ->
    Integer ->
    Integer ->
    IO Builder
computedChunk f reportProgress res@(w, _) firstPixel lastPixel sampleCount = do
    generator <- makeRandomGenerator
    let computePixel pixelIndex = do
            let (y, x) = pixelIndex `divMod` w
            total <- computedSamples f generator res (x, y) sampleCount
            reportProgress
            pure (putColorBuilder (total /. fromInteger sampleCount))
    pixels <- mapM computePixel [firstPixel .. lastPixel]
    pure (mconcat pixels)

computedImage ::
    (RandomGenerator -> Resolution -> ImageCoord -> IO Color) ->
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
                            (samplesPerPixel cam)
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

colorGradient :: RandomGenerator -> Camera -> Hit -> World -> Integer -> IO Color
colorGradient generator cam h world depth = do
    dir <- randomInHemisphere generator (hitNormal h)
    let ray =
            Ray
                { origin = hitP h
                , direction = dir + hitNormal h
                , time = 0
                }
    bounces <- rayColor generator cam ray world (depth - 1)
    pure (0.5 .* bounces)

colorScattering :: RandomGenerator -> Camera -> Hit -> Ray -> World -> Integer -> IO Color
colorScattering generator cam h r world depth = do
    let hitMaterial = material h
        hitInformation = info h
        colorFromEmission = emit hitMaterial (uv hitInformation) (p hitInformation)

    scatterResult <- scatter hitMaterial generator r hitInformation

    case scatterResult of
        Nothing ->
            pure colorFromEmission
        Just (attenuation, scattered) -> do
            bouncedColor <-
                rayColor generator cam scattered world (depth - 1)

            let colorFromScatter = attenuation * bouncedColor
            pure (colorFromEmission + colorFromScatter)

rayColor ::
    RandomGenerator ->
    Camera ->
    Ray ->
    World ->
    Integer ->
    IO Color
rayColor _ _ _ _ depth
    | depth <= 0 =
        pure (Vector3 0 0 0)
rayColor generator camera r world depth = do
    res <- hit world generator r (0.001, infinity)
    case res of
        Just hitted ->
            colorScattering
                generator
                camera
                hitted
                r
                world
                depth
        Nothing ->
            pure (backgroundColor camera)

rayPass :: Camera -> World -> RandomGenerator -> Resolution -> ImageCoord -> IO Color
rayPass cam world =
    let makeRay = makeRayGenerator cam
     in \generator _ (x, y) -> do
            ray <- makeRay generator x y
            rayColor generator cam ray world (maxDepth cam)
