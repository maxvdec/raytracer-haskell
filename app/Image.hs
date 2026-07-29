module Image where

import Math

ppmHeader :: Resolution -> String
ppmHeader (width, height) = "P3\n" ++ (show width) ++ " " ++ (show height) ++ "\n255\n"

fillColorData :: NormalizedColor -> String
fillColorData (r, g, b) = (show r) ++ " " ++ (show g) ++ " " ++ (show b)

putColor :: Color -> String
putColor color = fillColorData (normalizeColor color) ++ "\n"

fillColorTotal :: Integer -> Color -> String
fillColorTotal pixels color
    | pixels == 0 = ""
    | otherwise = putColor color ++ fillColorTotal (pixels - 1) color

fillColor :: Resolution -> Color -> String
fillColor (w, h) = fillColorTotal (w * h)
