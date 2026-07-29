module Renders where

import Geometry.Ray (Ray (Ray, direction, origin), at)
import Geometry.Scene (Camera (maxDepth, samplesPerPixel), World, makeRayForCoordinate)
import Geometry.Shapes (Hit (normal, p, t), Hittable (hit))
import Image (putColor)
import Math (Color, ImageCoord, Resolution, Vector3 (Vector3), getX, getY, getZ, infinity, normalizeColor, randomInHemisphere, ratio, unit, (*.), (.*), (/.))
import System.IO (hFlush, stdout)

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
    Resolution ->
    Integer ->
    Integer ->
    Integer ->
    IO String
computedRow f res@(w, h) x y sampleCount
    | x < w = do
        remaningPixels <- computedRow f res (x + 1) y sampleCount
        currentSamples <- computedSamples f res x y sampleCount
        pure (putColor (currentSamples /. fromInteger sampleCount) ++ remaningPixels)
    | otherwise = do
        putStr ("Row " ++ show y ++ " of " ++ show h ++ " completed\r")
        hFlush stdout
        pure ""

computedRows ::
    (Resolution -> ImageCoord -> IO Color) ->
    Resolution ->
    Integer ->
    Integer ->
    IO String
computedRows f res@(_, h) y sampleCount
    | y < h = do
        currentRow <- computedRow f res 0 y sampleCount
        otherRows <- computedRows f res (y + 1) sampleCount
        pure (currentRow ++ otherRows)
    | otherwise = do
        putStrLn ("Rendering finished all " ++ show h ++ " rows\n")
        pure ""

computedImage ::
    (Resolution -> ImageCoord -> IO Color) ->
    Resolution ->
    Camera ->
    IO String
computedImage f res cam = computedRows f res 0 (samplesPerPixel cam)

colorNormal :: Hit -> Color
colorNormal h =
    0.5 .* (normal h + Vector3 1 1 1)

colorGradient :: Hit -> World -> Integer -> IO Color
colorGradient h world depth = do
    dir <- randomInHemisphere (normal h)
    let ray =
            Ray
                { origin = p h
                , direction = dir
                }
    bounces <- rayColor ray world (depth - 1)
    pure (0.5 .* bounces)

rayColor :: Ray -> World -> Integer -> IO Color
rayColor _ _ 0 = pure (Vector3 0 0 0)
rayColor r world depth =
    let unitDirection = unit (direction r)
        a = 0.5 * getY unitDirection + 1.0
        hitResult = hit world r (0, infinity)
     in case hitResult of
            Just hitted -> do
                colorGradient hitted world depth
            _ -> pure (((1.0 - a) .* Vector3 1 1 1) + (a .* Vector3 0.5 0.7 1))

rayPass :: Camera -> World -> Resolution -> ImageCoord -> IO Color
rayPass cam world _ (x, y) = do
    ray <- makeRayForCoordinate cam x y
    rayColor ray world (maxDepth cam)
