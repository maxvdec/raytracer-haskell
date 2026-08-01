module Main (main) where

import Data.Char (toLower)
import Geometry.Scene (Camera (samplesPerPixel), Lights, World)
import Graphics.Image
import Math (Resolution)
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

mediumResolution :: Resolution
mediumResolution = (800, 450)

matchScene :: String -> Resolution -> (Camera, IO (World, Lights))
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
currentResolution = mediumResolution

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
            (world, lights) <- makeWorld
            hPutStr handle (ppmHeader selectedResolution)
            computedImage
                (rayPass camera world lights)
                handle
                selectedResolution
                camera
