module Scenes.FrozenCitadel where

import Geometry.Scene (Camera, Lights, Scene, World)
import Math (Resolution, Vector3 (Vector3))
import Scenes.Showcase

frozenCitadelCamera :: Resolution -> Camera
frozenCitadelCamera res =
    showcaseCamera
        res
        1500
        38
        (Vector3 820 310 (-1080))
        (Vector3 0 210 340)
        0.07
        1638
        (Vector3 0.008 0.018 0.038)

frozenCitadelWorld :: IO (World, Lights)
frozenCitadelWorld = do
    snow <- noise 0.038
    let ice = glass 1.31
        blueIce = solid (Vector3 0.22 0.42 0.62)
        silver = metal (Vector3 0.78 0.88 0.98) 0.12
        ground = quad (Vector3 (-850) 0 (-300)) (Vector3 1700 0 0) (Vector3 0 0 1700) snow
        centralKeep =
            [ box (Vector3 (-230) 0 250) (Vector3 230 360 680) blueIce
            , box (Vector3 (-135) 360 340) (Vector3 135 610 590) ice
            , sphere (Vector3 0 650 465) 82 ice
            ]
        towers =
            concat
                [ [ box (Vector3 (x - 75) 0 (z - 75)) (Vector3 (x + 75) height (z + 75)) ice
                  , sphere (Vector3 x (height + 58) z) 70 ice
                  , sphere (Vector3 x (height + 58) z) 26 silver
                  ]
                | (x, z, height) <-
                    [ (-430, 80, 340)
                    , (430, 80, 340)
                    , (-520, 610, 440)
                    , (520, 610, 440)
                    , (-350, 980, 300)
                    , (350, 980, 300)
                    ]
                ]
        crystalForest =
            [ rotatedBox
                (Vector3 (-width) 0 (-width))
                (Vector3 width height width)
                angle
                (Vector3 x 0 z)
                ice
            | (x, z, width, height, angle) <-
                [ (-690, 250, 24, 220, 17)
                , (-610, 430, 34, 320, -24)
                , (-720, 760, 27, 410, 38)
                , (690, 250, 24, 260, -17)
                , (620, 450, 35, 350, 28)
                , (730, 790, 30, 440, -35)
                , (-590, 1030, 38, 280, 12)
                , (590, 1050, 38, 310, -12)
                ]
            ]
        frozenFog = fogSphere (Vector3 0 175 570) 720 0.0012 (Vector3 0.62 0.78 1)
        auroraLights =
            [ quad
                (Vector3 (-520) y z)
                (Vector3 1040 0 0)
                (Vector3 0 0 95)
                (light color)
            | (y, z, color) <-
                [ (770, 90, Vector3 3 14 22)
                , (820, 410, Vector3 2 20 13)
                , (760, 760, Vector3 7 5 24)
                , (850, 1080, Vector3 2 12 20)
                ]
            ]
        objects = [ground, frozenFog] ++ centralKeep ++ towers ++ crystalForest
    showcaseWorld objects auroraLights

frozenCitadelScene :: Scene
frozenCitadelScene res = (frozenCitadelCamera res, frozenCitadelWorld)
