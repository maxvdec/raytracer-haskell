module Scenes.CelestialGarden where

import Geometry.Scene (Camera, Lights, Scene, World)
import Math (Resolution, Vector3 (Vector3))
import Scenes.Showcase

celestialGardenCamera :: Resolution -> Camera
celestialGardenCamera res =
    showcaseCamera
        res
        1300
        43
        (Vector3 780 300 (-1020))
        (Vector3 0 150 400)
        0.1
        1628
        (Vector3 0.002 0.004 0.014)

celestialGardenWorld :: IO (World, Lights)
celestialGardenWorld = do
    moss <- noise 0.07
    marble <- noise 0.16
    let bark = solid (Vector3 0.17 0.075 0.035)
        leaf = solid (Vector3 0.08 0.28 0.15)
        water = metal (Vector3 0.24 0.4 0.52) 0.06
        crystal = glass 1.46
        ground = quad (Vector3 (-900) 0 (-300)) (Vector3 1800 0 0) (Vector3 0 0 1750) moss
        reflectingPool = quad (Vector3 (-280) 5 120) (Vector3 560 0 0) (Vector3 0 0 900) water
        trees =
            concat
                [ [ box
                        (Vector3 (x - 24) 0 (z - 24))
                        (Vector3 (x + 24) height (z + 24))
                        bark
                  , sphere (Vector3 x (height + 85) z) 118 leaf
                  , sphere (Vector3 (x - 72) (height + 45) (z + 18)) 72 leaf
                  , sphere (Vector3 (x + 68) (height + 58) (z - 12)) 78 leaf
                  ]
                | (x, z, height) <-
                    [ (-610, 40, 240)
                    , (610, 70, 280)
                    , (-720, 420, 310)
                    , (710, 470, 250)
                    , (-650, 850, 270)
                    , (650, 940, 330)
                    , (-500, 1260, 290)
                    , (520, 1280, 260)
                    ]
                ]
        steppingStones =
            [ rotatedBox
                (Vector3 (-75) 0 (-48))
                (Vector3 75 22 48)
                angle
                (Vector3 x 4 z)
                marble
            | (x, z, angle) <- [(-115, 70, -12), (90, 210, 14), (-80, 370, -8), (95, 550, 20), (-70, 735, -15), (80, 920, 9)]
            ]
        gardenCrystals =
            [ sphere (Vector3 x y z) radius crystal
            | (x, y, z, radius) <-
                [ (-350, 90, 210, 82)
                , (370, 120, 390, 110)
                , (-420, 145, 690, 128)
                , (390, 100, 910, 90)
                , (-320, 130, 1180, 118)
                ]
            ]
        fireflies =
            [ sphere (Vector3 x y z) radius (light color)
            | (x, y, z, radius, color) <-
                [ (-520, 260, 100, 15, Vector3 9 18 3)
                , (-350, 330, 260, 12, Vector3 2 16 18)
                , (480, 290, 300, 14, Vector3 17 8 2)
                , (260, 380, 520, 11, Vector3 10 3 20)
                , (-470, 410, 640, 16, Vector3 2 19 9)
                , (520, 350, 790, 13, Vector3 2 12 20)
                , (-280, 310, 920, 12, Vector3 18 5 12)
                , (330, 430, 1080, 17, Vector3 5 18 7)
                , (-510, 370, 1240, 13, Vector3 3 10 22)
                , (490, 320, 1300, 15, Vector3 19 9 2)
                ]
            ]
        moon = sphere (Vector3 (-620) 820 1320) 145 (light (Vector3 8 10 15))
        objects = [ground, reflectingPool] ++ trees ++ steppingStones ++ gardenCrystals
    showcaseWorld objects (moon : fireflies)

celestialGardenScene :: Scene
celestialGardenScene res = (celestialGardenCamera res, celestialGardenWorld)
