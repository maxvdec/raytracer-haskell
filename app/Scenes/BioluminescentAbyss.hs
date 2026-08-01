module Scenes.BioluminescentAbyss where

import Geometry.Scene (Camera, Lights, Scene, World)
import Math (Resolution, Vector3 (Vector3))
import Scenes.Showcase

bioluminescentAbyssCamera :: Resolution -> Camera
bioluminescentAbyssCamera res =
    showcaseCamera
        res
        1700
        52
        (Vector3 560 220 (-760))
        (Vector3 0 170 390)
        0.09
        1279
        (Vector3 0 0.001 0.004)

bioluminescentAbyssWorld :: IO (World, Lights)
bioluminescentAbyssWorld = do
    seabed <- noise 0.045
    let shell = solid (Vector3 0.19 0.24 0.25)
        pearl = glass 1.34
        rock = metal (Vector3 0.08 0.12 0.14) 0.72
        ground = quad (Vector3 (-850) 0 (-260)) (Vector3 1700 0 0) (Vector3 0 0 1680) seabed
        canyonWalls =
            [ rotatedBox (Vector3 (-180) 0 (-120)) (Vector3 180 560 120) (-18) (Vector3 (-670) 0 510) rock
            , rotatedBox (Vector3 (-190) 0 (-130)) (Vector3 190 690 130) 24 (Vector3 650 0 620) rock
            , rotatedBox (Vector3 (-150) 0 (-100)) (Vector3 150 430 100) (-32) (Vector3 (-560) 0 1050) rock
            , rotatedBox (Vector3 (-170) 0 (-110)) (Vector3 170 520 110) 35 (Vector3 570 0 1110) rock
            ]
        coralStalks =
            concat
                [ [ box
                        (Vector3 (x - width) 0 (z - width))
                        (Vector3 (x + width) height (z + width))
                        shell
                  , sphere (Vector3 x (height + radius * 0.45) z) radius pearl
                  ]
                | (x, z, width, height, radius) <-
                    [ (-410, 40, 17, 180, 62)
                    , (-210, 180, 21, 280, 78)
                    , (40, 60, 14, 140, 48)
                    , (300, 210, 25, 340, 92)
                    , (480, 20, 16, 220, 58)
                    , (-500, 620, 25, 390, 104)
                    , (-130, 710, 18, 240, 68)
                    , (180, 560, 22, 310, 85)
                    , (520, 760, 20, 270, 75)
                    , (-320, 1100, 20, 300, 82)
                    , (100, 1040, 26, 430, 112)
                    , (440, 1190, 17, 210, 64)
                    ]
                ]
        bubbles =
            [ sphere (Vector3 x y z) radius pearl
            | (x, y, z, radius) <-
                [ (-290, 440, 350, 32)
                , (-235, 530, 410, 21)
                , (-180, 610, 475, 15)
                , (330, 530, 520, 29)
                , (280, 620, 600, 18)
                , (230, 710, 685, 12)
                , (40, 580, 960, 37)
                , (-20, 700, 1020, 23)
                ]
            ]
        waterVolume = fogSphere (Vector3 0 300 550) 1500 0.00055 (Vector3 0.12 0.42 0.52)
        creatureLights =
            [ sphere (Vector3 x y z) radius (light color)
            | (x, y, z, radius, color) <-
                [ (-410, 210, 40, 22, Vector3 1 9 16)
                , (-210, 310, 180, 28, Vector3 2 15 11)
                , (40, 165, 60, 18, Vector3 10 2 18)
                , (300, 375, 210, 32, Vector3 1 12 20)
                , (480, 250, 20, 21, Vector3 18 2 10)
                , (-500, 435, 620, 36, Vector3 2 17 12)
                , (-130, 270, 710, 24, Vector3 12 3 20)
                , (180, 345, 560, 30, Vector3 1 13 21)
                , (520, 300, 760, 26, Vector3 20 4 9)
                , (-320, 335, 1100, 28, Vector3 1 14 18)
                , (100, 475, 1040, 38, Vector3 5 18 12)
                , (440, 240, 1190, 23, Vector3 16 3 19)
                ]
            ]
        objects = [ground, waterVolume] ++ canyonWalls ++ coralStalks ++ bubbles
    showcaseWorld objects creatureLights

bioluminescentAbyssScene :: Scene
bioluminescentAbyssScene res = (bioluminescentAbyssCamera res, bioluminescentAbyssWorld)
