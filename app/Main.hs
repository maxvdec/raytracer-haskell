module Main (main) where

import Image

main :: IO ()
main = do
    writeFile "./output.ppm" (Image.ppmHeader (256, 256))
    appendFile "./output.ppm" (Image.fillColor (256, 256) (255, 255, 255))
