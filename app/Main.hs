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
import Scenes.CornellBoxSmoke (cornellBoxSmokeScene)
import Scenes.FinalScene (finalScene)
import Scenes.PerlinSpheres (perlinSpheresScene)
import Scenes.Quads (quadsScene)
import Scenes.SimpleLight (simpleLightScene)
import Scenes.World (worldScene)
import System.Environment (getArgs)
import System.IO (IOMode (WriteMode), hPutStr, withFile)
import Text.Read (readMaybe)

normalResolution :: Resolution
normalResolution = (1280, 720)

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
    "smoke" -> cornellBoxSmokeScene res
    "final" -> finalScene res
    _ -> error "Scene does not exist"

currentResolution :: Resolution
currentResolution = normalResolution

main :: IO ()
main = do
    args <- getArgs

    let (sceneName, selectedResolution, selectedSamples) =
            case args of
                name : widthText : heightText : sampleText : _ ->
                    case (readMaybe widthText, readMaybe heightText, readMaybe sampleText) of
                        (Just width, Just height, Just sampleCount)
                            | width > 0 && height > 0 && sampleCount > 0 ->
                                (name, (width, height), Just sampleCount)
                        _ -> error "Width, height, and samples must be positive integers"
                name : _ -> (name, currentResolution, Nothing)
                [] -> ("", currentResolution, Nothing)
        (baseCamera, makeWorld) = matchScene sceneName selectedResolution
        camera =
            case selectedSamples of
                Just sampleCount -> baseCamera{samplesPerPixel = sampleCount}
                Nothing -> baseCamera
     in withFile "./output.ppm" WriteMode $ \handle -> do
            world <- makeWorld
            hPutStr handle (ppmHeader selectedResolution)
            computedImage
                (rayPass camera world)
                handle
                selectedResolution
                camera
