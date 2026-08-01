module Scenes.DesertMonoliths where

import Geometry.Scene (Camera, Lights, Scene, World)
import Math (Resolution, Vector3 (Vector3))
import Scenes.Showcase

desertMonolithsCamera :: Resolution -> Camera
desertMonolithsCamera res =
    showcaseCamera
        res
        1300
        36
        (Vector3 960 340 (-1260))
        (Vector3 0 165 390)
        0.12
        1918
        (Vector3 0.11 0.035 0.008)

desertMonolithsWorld :: IO (World, Lights)
desertMonolithsWorld = do
    sand <- noise 0.018
    let sandstone = solid (Vector3 0.48 0.22 0.085)
        blackStone = metal (Vector3 0.055 0.045 0.04) 0.025
        bronze = metal (Vector3 0.68 0.33 0.09) 0.16
        artifactGlass = glass 1.62
        ground = quad (Vector3 (-1300) 0 (-480)) (Vector3 2600 0 0) (Vector3 0 0 2300) sand
        monolithSpecs =
            [ (-740, 40, 130, 520, -8)
            , (-460, 280, 170, 760, 12)
            , (-190, 110, 105, 410, -18)
            , (210, 240, 145, 680, 16)
            , (520, 50, 120, 470, -12)
            , (760, 440, 180, 820, 9)
            , (-690, 900, 160, 620, 21)
            , (-240, 1040, 135, 720, -10)
            , (310, 920, 160, 580, 14)
            , (690, 1120, 125, 710, -16)
            ]
        monoliths =
            [ rotatedBox
                (Vector3 (-width / 2) 0 (-width / 3))
                (Vector3 (width / 2) height (width / 3))
                angle
                (Vector3 x 0 z)
                (if even index then blackStone else sandstone)
            | (index, (x, z, width, height, angle)) <- zip [0 :: Int ..] monolithSpecs
            ]
        crowns =
            [ sphere
                (Vector3 x (height + width * 0.3) z)
                (width * 0.3)
                (if even index then artifactGlass else bronze)
            | (index, (x, z, width, height, _)) <- zip [0 :: Int ..] monolithSpecs
            ]
        centralGate =
            [ box (Vector3 (-285) 0 560) (Vector3 (-195) 520 680) blackStone
            , box (Vector3 195 0 560) (Vector3 285 520 680) blackStone
            , box (Vector3 (-285) 450 560) (Vector3 285 540 680) blackStone
            , sphere (Vector3 0 250 625) 145 artifactGlass
            , sphere (Vector3 0 250 625) 52 bronze
            ]
        portalLight =
            quad
                (Vector3 155 85 558)
                (Vector3 (-310) 0 0)
                (Vector3 0 330 0)
                (light (Vector3 20 5 0.7))
        sun = sphere (Vector3 (-900) 1040 1450) 170 (light (Vector3 22 12 4.5))
        emberLights =
            [ sphere (Vector3 x 24 z) 9 (light (Vector3 18 3 0.25))
            | x <- [-380, -190 .. 380]
            , z <- [160, 390 .. 1080]
            ]
        objects = [ground] ++ monoliths ++ crowns ++ centralGate
    showcaseWorld objects (sun : portalLight : emberLights)

desertMonolithsScene :: Scene
desertMonolithsScene res = (desertMonolithsCamera res, desertMonolithsWorld)
