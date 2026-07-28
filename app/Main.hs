module Main (main) where

import Image
import Math (Color, ImageCoord, Resolution)
import Renders (computedImage)

_normalResolution :: Resolution
_normalResolution = (1920, 1080)

devResolution :: Resolution
devResolution = (400, 255)

redFiller :: Resolution -> ImageCoord -> Color
redFiller _ _ = (1.0, 0, 1.0)

main :: IO ()
main = do
    writeFile "./output.ppm" (Image.ppmHeader devResolution)
    computation <- Renders.computedImage redFiller devResolution
    appendFile "./output.ppm" computation
