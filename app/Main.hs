module Main (main) where

import Image
import Math (Color, ImageCoord, Resolution)
import Renders (computedImage, uvRender)

_normalResolution :: Resolution
_normalResolution = (1920, 1080)

devResolution :: Resolution
devResolution = (400, 255)

redFiller :: Resolution -> ImageCoord -> Color
redFiller _ _ = (255, 0, 255)

main :: IO ()
main = do
    writeFile "./output.ppm" (Image.ppmHeader devResolution)
    appendFile "./output.ppm" (Renders.computedImage redFiller devResolution)
