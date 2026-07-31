module Main (main) where

import Geometry.BVH (createBVHTree)
import Geometry.Hit (SomeHittable (SomeHittable))
import Geometry.Scene (Camera (Camera, defocusAngle, defocusDiskU, defocusDiskV, focusDist, fov, lookat, lookfrom, maxDepth, resolution, samplesPerPixel, viewportResolution, vup), World (World, hittables), fillDiskInfo, fillViewportResolution)
import Geometry.Shapes
import Graphics.Image
import Graphics.Materials (SomeMaterial (SomeMaterial), makeDielectric, makeLambertian, makeMetal)
import Graphics.Texture (SomeTexture (SomeTexture), loadImageTexture, makeCheckerFromColors, makeSolidColor)
import Math (RandomGenerator, Resolution, Vector3 (Vector3), makeRandomGenerator, (|>))
import Renders (computedImage, rayPass)
import System.IO (IOMode (WriteMode), hPutStr, withFile)

normalResolution :: Resolution
normalResolution = (1920, 1080)

devResolution :: Resolution
devResolution = (400, 225)

mediumResolution :: Resolution
mediumResolution = (800, 450)

makeWorld :: IO World
makeWorld = do
    worldTexture <- loadImageTexture "./textures/earthmap.jpg"
    let surface = makeLambertian (SomeTexture worldTexture)
        globe = makeSphere (Vector3 0 0 0) 2 (SomeMaterial surface)
        scene = [SomeHittable globe]
        root = createBVHTree scene

    pure (World{hittables = [SomeHittable root]})

main :: IO ()
main =
    let camera =
            Camera
                { viewportResolution = (0, 0)
                , resolution = mediumResolution
                , samplesPerPixel = 100
                , maxDepth = 50
                , fov = 20
                , lookfrom = (Vector3 0 0 12)
                , lookat = (Vector3 0 0 0)
                , vup = (Vector3 0 1 0)
                , defocusAngle = 0
                , focusDist = 3.58
                , defocusDiskU = (Vector3 0 0 0)
                , defocusDiskV = (Vector3 0 0 0)
                }
                |> fillViewportResolution
                |> fillDiskInfo
     in withFile "./output.ppm" WriteMode $ \handle -> do
            world <- makeWorld
            hPutStr handle (ppmHeader mediumResolution)
            computedImage
                (rayPass camera world)
                handle
                mediumResolution
                camera
