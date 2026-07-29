module Main (main) where

import Geometry.Scene (Camera (Camera, cameraCenter, focalLength, resolution, viewportResolution), createViewportResolutionFromHeight)
import Image
import Math (Color, ImageCoord, Resolution, Vector3 (Vector3))
import Renders (computedImage, rayPass)

_normalResolution :: Resolution
_normalResolution = (1920, 1080)

devResolution :: Resolution
devResolution = (400, 225)

main :: IO ()
main =
    let camera =
            Camera
                { focalLength = 1.0
                , viewportResolution = (createViewportResolutionFromHeight 2.0 devResolution)
                , cameraCenter = (Vector3 0 0 0)
                , resolution = devResolution
                }
     in do
            writeFile "./output.ppm" (Image.ppmHeader devResolution)
            computation <- Renders.computedImage (rayPass camera) devResolution
            appendFile "./output.ppm" computation
