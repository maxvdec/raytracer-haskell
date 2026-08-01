module Scenes.NeonMetropolis where

import Geometry.Scene (Camera, Lights, Scene, World)
import Math (Resolution, Vector3 (Vector3))
import Scenes.Showcase

neonMetropolisCamera :: Resolution -> Camera
neonMetropolisCamera res =
    showcaseCamera
        res
        1400
        48
        (Vector3 610 235 (-920))
        (Vector3 0 190 360)
        0.08
        1419
        (Vector3 0.001 0.002 0.008)

neonMetropolisWorld :: IO (World, Lights)
neonMetropolisWorld = do
    let wetAsphalt = metal (Vector3 0.22 0.25 0.32) 0.14
        blackGlass = metal (Vector3 0.08 0.11 0.18) 0.04
        concrete = solid (Vector3 0.16 0.17 0.2)
        windowGlass = glass 1.48
        ground = quad (Vector3 (-720) 0 (-240)) (Vector3 1440 0 0) (Vector3 0 0 1540) wetAsphalt
        towerSpecs =
            [ (-610, 70, 180, 470)
            , (-430, 160, 150, 650)
            , (-245, 20, 170, 390)
            , (65, 130, 190, 730)
            , (300, 10, 175, 520)
            , (500, 190, 160, 610)
            , (-650, 590, 210, 340)
            , (-360, 690, 180, 520)
            , (240, 690, 210, 450)
            , (495, 570, 175, 690)
            ]
        towers =
            [ box
                (Vector3 x 0 z)
                (Vector3 (x + width) height (z + width))
                (if even index then blackGlass else concrete)
            | (index, (x, z, width, height)) <- zip [0 :: Int ..] towerSpecs
            ]
        rooftopGlass =
            [ sphere (Vector3 (x + width / 2) (height + 38) (z + width / 2)) 38 windowGlass
            | (x, z, width, height) <- towerSpecs
            ]
        cyanSigns =
            [ quad
                (Vector3 (x + width - 8) (height * 0.2) (z - 1))
                (Vector3 (-(width - 16)) 0 0)
                (Vector3 0 (height * 0.56) 0)
                (light (Vector3 1.5 14 22))
            | (x, z, width, height) <- take 5 towerSpecs
            ]
        magentaSigns =
            [ quad
                (Vector3 (x + width - 12) (height * 0.32) (z - 1))
                (Vector3 (-(width - 24)) 0 0)
                (Vector3 0 (height * 0.22) 0)
                (light (Vector3 20 1.2 12))
            | (x, z, width, height) <- drop 5 towerSpecs
            ]
        streetLights =
            [ sphere
                (Vector3 x 58 z)
                13
                (light (if even index then Vector3 18 5 1 else Vector3 2 8 20))
            | (index, (x, z)) <- zip [0 :: Int ..] [(x, z) | x <- [-210, 0, 210], z <- [40, 260 .. 920]]
            ]
        laneDividers =
            [ box (Vector3 (-8) 1 z) (Vector3 8 3 (z + 72)) (light (Vector3 7 4 1.2))
            | z <- [-100, 45 .. 1080]
            ]
        objects = [ground] ++ towers ++ rooftopGlass ++ laneDividers
    showcaseWorld objects (cyanSigns ++ magentaSigns ++ streetLights)

neonMetropolisScene :: Scene
neonMetropolisScene res = (neonMetropolisCamera res, neonMetropolisWorld)
