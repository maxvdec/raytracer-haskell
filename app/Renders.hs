module Renders where
import Math (ImageCoord, Resolution, Color, normalizeColor, ratio)
import Image (putColor)

-- UV Render
uvForPos :: ImageCoord -> Resolution -> Color
uvForPos (x, y) (w, h) = 
    normalizeColor (ratio x w, ratio y h, 0)

-- Render everything
uvColumn :: Resolution -> Integer -> Integer -> String 
uvColumn (w, h) x y 
    | y < h = putColor (uvForPos (x, y) (w, h)) ++ uvColumn (w, h) x (y + 1)
    | otherwise = "" 


uvSwipe :: Resolution -> Integer -> Integer -> String
uvSwipe (w, h) x y
    | x < w = uvColumn (w, h) x y ++ uvSwipe (w, h) (x + 1) y
    | otherwise = "" 


uvRender :: Resolution -> String
uvRender res = uvSwipe res 0 0