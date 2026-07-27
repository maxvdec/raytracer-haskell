module Main (main) where

import Image
import Renders (uvRender)

main :: IO ()
main = do
    writeFile "./output.ppm" (Image.ppmHeader (256, 256))
    appendFile "./output.ppm" (Renders.uvRender (256, 256))
