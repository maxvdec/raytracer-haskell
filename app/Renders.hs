module Renders where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (modifyMVar_, newMVar)
import GHC.Conc (getNumCapabilities)
import Geometry.Hit (Hit (info, material), Hittable (hit), hitNormal, hitP, hitT)
import Geometry.Ray (Ray (Ray, direction, origin), at)
import Geometry.Scene (Camera (maxDepth, samplesPerPixel), World, makeRayForCoordinate)
import Graphics.Image (putColor)
import Graphics.Materials (Material (scatter))
import Math (Color, ImageCoord, RandomGenerator, Resolution, Vector3 (Vector3), getX, getY, getZ, infinity, makeRandomGenerator, normalizeColor, randomInHemisphere, ratio, unit, (*.), (.*), (/.))
import System.IO (Handle, hFlush, hPutStr, stdout)

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
        accumulate (remaining - 1) (total + color)

computedRow ::
    (RandomGenerator -> Resolution -> ImageCoord -> IO Color) ->
    IO () ->
    Resolution ->
    Integer ->
    Integer ->
    IO String
computedRow f reportProgress res@(w, _) y sampleCount = do
    generator <- makeRandomGenerator
    let computePixel x = do
            total <- computedSamples f generator res (x, y) sampleCount
            reportProgress
            pure (putColor (total /. fromInteger sampleCount))
    pixels <- mapM computePixel [0 .. w - 1]
    pure (concat pixels)

computedImage ::
    (RandomGenerator -> Resolution -> ImageCoord -> IO Color) ->
    Handle ->
    Resolution ->
    Camera ->
    IO ()
computedImage f handle res@(w, h) cam = do
    capabilities <- getNumCapabilities
    progress <- newMVar (0, 0)
    let totalPixels = w * h
        reportProgress =
            modifyMVar_ progress $ \(completed, displayedPercentage) -> do
                let completedPixels = completed + 1
                    percentage = completedPixels * 100 `div` totalPixels
                if percentage > displayedPercentage
                    then do
                        putStr (show percentage ++ "% rendered\r")
                        hFlush stdout
                    else pure ()
                pure (completedPixels, max displayedPercentage percentage)
        batchSize = max 1 (fromIntegral capabilities * 2)
        renderRows firstRow
            | firstRow >= h = pure ()
            | otherwise = do
                let lastRow = min (h - 1) (firstRow + batchSize - 1)
                rows <-
                    mapConcurrently
                        (\y -> computedRow f reportProgress res y (samplesPerPixel cam))
                        [firstRow .. lastRow]
                mapM_ (hPutStr handle) rows
                renderRows (lastRow + 1)

    putStr "0% rendered\r"
    hFlush stdout

    renderRows 0
    putStrLn ""

colorNormal :: Hit -> Color
colorNormal h =
    0.5 .* (hitNormal h + Vector3 1 1 1)

colorGradient :: RandomGenerator -> Hit -> World -> Integer -> IO Color
colorGradient generator h world depth = do
    dir <- randomInHemisphere generator (hitNormal h)
    let ray =
            Ray
                { origin = hitP h
                , direction = dir + hitNormal h
                }
    bounces <- rayColor generator ray world (depth - 1)
    pure (0.5 .* bounces)

colorScattering :: RandomGenerator -> Hit -> Ray -> World -> Integer -> IO Color
colorScattering generator h r world depth = do
    let hitMaterial = material h
    result <- scatter hitMaterial generator r (info h)
    case result of
        Nothing -> pure (Vector3 0 0 0)
        Just (attenuation, scattered) -> do
            bounces <- rayColor generator scattered world (depth - 1)
            pure (attenuation * bounces)

rayColor :: RandomGenerator -> Ray -> World -> Integer -> IO Vector3
rayColor _ _ _ 0 = pure (Vector3 0 0 0)
rayColor generator r world depth =
    let unitDirection = unit (direction r)
        a = 0.5 * getY unitDirection + 1.0
        hitResult = hit world r (0.001, infinity)
     in case hitResult of
            Just hitted -> do
                colorScattering generator hitted r world depth
            _ -> pure (((1.0 - a) .* Vector3 1 1 1) + (a .* Vector3 0.5 0.7 1))

rayPass :: Camera -> World -> RandomGenerator -> Resolution -> ImageCoord -> IO Color
rayPass cam world generator _ (x, y) = do
    ray <- makeRayForCoordinate generator cam x y
    rayColor generator ray world (maxDepth cam)
