module Main (main) where

import Geometry.Scene (Camera (Camera, cameraCenter, focalLength, resolution, viewportResolution), World (World, hittables), createViewportResolutionFromHeight)
import Geometry.Shapes
import Image
import Math (Color, ImageCoord, Resolution, Vector3 (Vector3))
import Renders (computedImage, rayPass)

_normalResolution :: Resolution
_normalResolution = (1920, 1080)

devResolution :: Resolution
devResolution = (400, 225)

makeWorld :: World
makeWorld =
    let sphere1 = makeSphere (Vector3 0 0 (-1)) 0.5
        sphere2 = makeSphere (Vector3 0 (-100.5) (-1)) 100
     in World
            { hittables = [SomeHittable sphere1, SomeHittable sphere2]
            }

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
            computation <- Renders.computedImage (rayPass camera makeWorld) devResolution
            appendFile "./output.ppm" computation
