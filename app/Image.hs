module Image where

import Math

ppmHeader :: Resolution -> String
ppmHeader (width, height) = "P3\n" ++ (show width) ++ " " ++ (show height) ++ "\n255\n"

fillColorData :: Color -> String
fillColorData (r, g, b) = (show r) ++ " " ++ (show g) ++ " " ++ (show b)

putColor :: Color -> String
putColor color = fillColorData color ++ "\n"

fillColorTotal :: Integer -> Color -> String
fillColorTotal pixels color
    | pixels == 0 = ""
    | otherwise = putColor color ++ fillColorTotal (pixels - 1) color

fillColor :: Resolution -> Color -> String
fillColor (w, h) color = fillColorTotal (w * h) color