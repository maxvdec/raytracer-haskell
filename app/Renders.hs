module Renders where

import Image (putColor)
import Math (Color, ImageCoord, Resolution, normalizeColor, ratio)

-- UV Render
uvForPos :: ImageCoord -> Resolution -> Color
uvForPos (x, y) (w, h) =
    normalizeColor (ratio x w, ratio y h, 0)

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
    String
computedRow f (w, h) x y
    | x < w =
        putColor (f (x, y) (w, h)) ++ computedRow f (w, h) (x + 1) y
    | otherwise = ""

computedRows ::
    (Resolution -> ImageCoord -> Color) ->
    Resolution ->
    Integer ->
    String
computedRows f res@(_, h) y
    | y < h = computedRow f res 0 y ++ computedRows f res (y + 1)
    | otherwise = ""

computedImage ::
    (Resolution -> ImageCoord -> Color) ->
    Resolution ->
    String
computedImage f res = computedRows f res 0
