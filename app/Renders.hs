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