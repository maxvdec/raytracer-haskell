module Main (main) where

import Geometry.Hit (SomeHittable (SomeHittable))
import Geometry.Scene (Camera (Camera, maxDepth, resolution, samplesPerPixel, viewportResolution, fov, lookat, lookfrom, vup, defocusAngle, focusDist, defocusDiskU, defocusDiskV), World (World, hittables), fillViewportResolution, fillDiskInfo)
import Geometry.Shapes
import Graphics.Image
import Graphics.Materials (SomeMaterial (SomeMaterial), makeLambertian, makeMetal, makeDielectric)
import Math (Resolution, Vector3 (Vector3), (|>), RandomGenerator, makeRandomGenerator)
import Renders (computedImage, rayPass)
import System.IO (IOMode (WriteMode), hPutStr, withFile)
import Geometry.BVH (createBVHTree)

normalResolution :: Resolution
normalResolution = (1920, 1080)

devResolution :: Resolution
devResolution = (400, 225)

mediumResolution :: Resolution
mediumResolution = (800, 450)

makeWorld :: World
makeWorld = 
    let materialGround = makeLambertian (Vector3 0.8 0.8 0)
        materialCenter = makeLambertian (Vector3 0.1 0.2 0.5)
        materialLeft = makeDielectric (1.0 / 1.33)
        materialRight = makeMetal (Vector3 0.8 0.6 0.2) 1.0
        groundSphere = makeSphere (Vector3 0 (-100.5) (-1)) 100 (SomeMaterial materialGround)
        centerSphere = makeAnimatedSphere (Vector3 0 0 (-1.2)) (Vector3 0 1.5 (-1.2)) 0.5 (SomeMaterial materialCenter)
        leftSphere = makeSphere (Vector3 (-1) 0 (-1)) 0.5 (SomeMaterial materialLeft)
        rightSphere = makeSphere (Vector3 1 0 (-1)) 0.5 (SomeMaterial materialRight)
        scene = [SomeHittable groundSphere, SomeHittable centerSphere, SomeHittable leftSphere, SomeHittable rightSphere] 
        root = createBVHTree scene in
    World { hittables = [SomeHittable root] }
    

main :: IO ()
main =
    let camera =
            Camera
                { viewportResolution = (0, 0)
                , resolution = mediumResolution
                , samplesPerPixel = 100
                , maxDepth = 50
                , fov = 90
                , lookfrom = (Vector3 (-2) 2 1)
                , lookat = (Vector3 0 0 (-1))
                , vup = (Vector3 0 1 0)
                , defocusAngle = 0.6
                , focusDist = 3.58
                , defocusDiskU = (Vector3 0 0 0)
                , defocusDiskV = (Vector3 0 0 0)
                } |> fillViewportResolution |> fillDiskInfo 
     in withFile "./output.ppm" WriteMode $ \handle -> do
            hPutStr handle (ppmHeader mediumResolution)
            computedImage
                (rayPass camera makeWorld)
                handle
                mediumResolution
                camera
