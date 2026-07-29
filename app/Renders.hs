module Renders where

import Geometry.Ray (Ray (direction), at)
import Geometry.Scene (Camera, World, makeRayForCoordinate)
import Geometry.Shapes (Hit (normal, t), Hittable (hit))
import Image (putColor)
import Math (Color, ImageCoord, Resolution, Vector3 (Vector3), getX, getY, getZ, infinity, normalizeColor, ratio, unit, (*.), (.*))
import System.IO (hFlush, stdout)

-- UV Render
uvForPos :: ImageCoord -> Resolution -> Color
uvForPos (x, y) (w, h) =
    (Vector3 (ratio x w) (ratio y h) 0)

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
computedRow ::
    (Resolution -> ImageCoord -> Color) ->
    Resolution ->
    Integer ->
    Integer ->
    IO String
computedRow f res@(w, h) x y
    | x < w = do
        remaningPixels <- computedRow f res (x + 1) y
        pure (putColor (f (w, h) (x, y)) ++ remaningPixels)
    | otherwise = do
        putStr ("Row " ++ show y ++ " of " ++ show h ++ " completed\r")
        hFlush stdout
        pure ""

computedRows ::
    (Resolution -> ImageCoord -> Color) ->
    Resolution ->
    Integer ->
    IO String
computedRows f res@(_, h) y
    | y < h = do
        currentRow <- computedRow f res 0 y
        otherRows <- computedRows f res (y + 1)
        pure (currentRow ++ otherRows)
    | otherwise = do
        putStrLn ("Rendering finished all " ++ show h ++ " rows\n")
        pure ""

computedImage ::
    (Resolution -> ImageCoord -> Color) ->
    Resolution ->
    IO String
computedImage f res = computedRows f res 0

colorNormal :: Hit -> Color
colorNormal h =
    0.5 .* ((normal h) + (Vector3 1 1 1))

rayColor :: Ray -> World -> Color
rayColor r world =
    let unitDirection = unit (direction r)
        a = 0.5 * ((getY unitDirection) + 1.0)
        hitResult = hit world r (0, infinity)
     in case hitResult of
            Just hitted -> colorNormal hitted
            _ -> ((1.0 - a) .* (Vector3 1 1 1)) + (a .* (Vector3 0.5 0.7 1))

rayPass :: Camera -> World -> Resolution -> ImageCoord -> Color
rayPass cam world _ (x, y) =
    let ray = (makeRayForCoordinate cam x y)
     in rayColor ray world