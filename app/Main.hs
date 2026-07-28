module Main (main) where

import Image
import Math (Resolution)
import Renders (uvRender)

_normalResolution :: Resolution
_normalResolution = (1920, 1080)

devResolution :: Resolution
devResolution = (400, 255)

main :: IO ()
main = do
    writeFile "./output.ppm" (Image.ppmHeader devResolution)
    appendFile "./output.ppm" (Renders.uvRender devResolution)
