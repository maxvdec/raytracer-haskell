module Main (main) where

import Data.Char (toLower)
import Geometry.BVH (createBVHTree)
import Geometry.Hit (SomeHittable (SomeHittable))
import Geometry.HitInfo (HitInfo (normal))
import Geometry.Scene (Camera (Camera, defocusAngle, defocusDiskU, defocusDiskV, focusDist, fov, lookat, lookfrom, maxDepth, resolution, samplesPerPixel, viewportResolution, vup), Scene, World (World, hittables), fillDiskInfo, fillViewportResolution)
import Geometry.Shapes
import Graphics.Image
import Graphics.Materials (SomeMaterial (SomeMaterial), makeDielectric, makeLambertian, makeMetal)
import Graphics.Texture (SomeTexture (SomeTexture), loadImageTexture, makeCheckerFromColors, makeSolidColor)
import Math (RandomGenerator, Resolution, Vector3 (Vector3), makeRandomGenerator, (|>))
import Renders (computedImage, rayPass)
import Scenes.CornellBox (cornellBoxScene)
import Scenes.PerlinSpheres (perlinSpheresScene)
import Scenes.Quads (quadsScene)
import Scenes.SimpleLight (simpleLightScene)
import Scenes.World (worldScene)
import System.Environment (getArgs)
import System.IO (IOMode (WriteMode), hPutStr, withFile)

normalResolution :: Resolution
normalResolution = (1920, 1080)

devResolution :: Resolution
devResolution = (400, 225)

mediumResolution :: Resolution
mediumResolution = (800, 450)

matchScene :: String -> Resolution -> (Camera, IO World)
matchScene name res = case (map toLower name) of
    "world" -> worldScene res
    "perlin" -> perlinSpheresScene res
    "quads" -> quadsScene res
    "simple_light" -> simpleLightScene res
    "cornell" -> cornellBoxScene res
    _ -> error "Scene does not exist"

currentResolution :: Resolution
currentResolution = devResolution

main :: IO ()
main = do
    args <- getArgs

    let sceneName =
            case args of
                name : _ -> name
                [] -> ""
    let (camera, makeWorld) = matchScene sceneName currentResolution
     in withFile "./output.ppm" WriteMode $ \handle -> do
            world <- makeWorld
            hPutStr handle (ppmHeader currentResolution)
            computedImage
                (rayPass camera world)
                handle
                currentResolution
                camera
