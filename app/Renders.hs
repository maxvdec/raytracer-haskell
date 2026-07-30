module Renders where

import Geometry.Hit (Hit (info, material), Hittable (hit), hitNormal, hitP, hitT)
import Geometry.Ray (Ray (Ray, direction, origin), at)
import Geometry.Scene (Camera (maxDepth, samplesPerPixel), World, makeRayForCoordinate)
import Image (putColor)
import Materials (Material (scatter))
import Math (Color, ImageCoord, Resolution, Vector3 (Vector3), getX, getY, getZ, infinity, normalizeColor, randomInHemisphere, ratio, unit, (*.), (.*), (/.))
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
    (Resolution -> ImageCoord -> IO Color) ->
    Resolution ->
    Integer ->
    Integer ->
    Integer ->
    IO Color
computedSamples f res@(w, h) x y sampleCount
    | sampleCount == 1 = do f (w, h) (x, y)
    | otherwise = do
        remainingSamples <- computedSamples f res x y (sampleCount - 1)
        color <- f (w, h) (x, y)
        pure (color + remainingSamples)

computedRow ::
    (Resolution -> ImageCoord -> IO Color) ->
    Handle ->
    Resolution ->
    Integer ->
    Integer ->
    Integer ->
    IO ()
computedRow f handle res@(w, _) x y sampleCount
    | x < w = do
        currentSamples <- computedSamples f res x y sampleCount
        hPutStr handle (putColor (currentSamples /. fromInteger sampleCount))
        computedRow f handle res (x + 1) y sampleCount
    | otherwise = pure ()

computedRows ::
    (Resolution -> ImageCoord -> IO Color) ->
    Handle ->
    Resolution ->
    Integer ->
    Integer ->
    IO ()
computedRows f handle res@(_, h) y sampleCount
    | y < h = do
        computedRow f handle res 0 y sampleCount
        putStr ("Row " ++ show (y + 1) ++ " of " ++ show h ++ " completed\r")
        hFlush stdout
        computedRows f handle res (y + 1) sampleCount
    | otherwise = do
        putStrLn ("Rendering finished all " ++ show h ++ " rows\n")

computedImage ::
    (Resolution -> ImageCoord -> IO Color) ->
    Handle ->
    Resolution ->
    Camera ->
    IO ()
computedImage f handle res cam = computedRows f handle res 0 (samplesPerPixel cam)

colorNormal :: Hit -> Color
colorNormal h =
    0.5 .* (hitNormal h + Vector3 1 1 1)

colorGradient :: Hit -> World -> Integer -> IO Color
colorGradient h world depth = do
    dir <- randomInHemisphere (hitNormal h)
    let ray =
            Ray
                { origin = hitP h
                , direction = dir + hitNormal h
                }
    bounces <- rayColor ray world (depth - 1)
    pure (0.5 .* bounces)

colorScattering :: Hit -> Ray -> World -> Integer -> IO Color
colorScattering h r world depth = do
    let hitMaterial = material h
    result <- scatter hitMaterial r (info h)
    case result of
        Nothing -> pure (Vector3 0 0 0)
        Just (attenuation, scattered) -> do
            bounces <- rayColor scattered world (depth - 1)
            pure (attenuation * bounces)

rayColor :: Ray -> World -> Integer -> IO Vector3
rayColor _ _ 0 = pure (Vector3 0 0 0)
rayColor r world depth =
    let unitDirection = unit (direction r)
        a = 0.5 * getY unitDirection + 1.0
        hitResult = hit world r (0.001, infinity)
     in case hitResult of
            Just hitted -> do
                colorScattering hitted r world depth
            _ -> pure (((1.0 - a) .* Vector3 1 1 1) + (a .* Vector3 0.5 0.7 1))

rayPass :: Camera -> World -> Resolution -> ImageCoord -> IO Color
rayPass cam world _ (x, y) = do
    ray <- makeRayForCoordinate cam x y
    rayColor ray world (maxDepth cam)
