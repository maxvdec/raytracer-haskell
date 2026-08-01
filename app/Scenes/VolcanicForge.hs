module Scenes.VolcanicForge where

import Geometry.Scene (Camera, Lights, Scene, World)
import Math (Resolution, Vector3 (Vector3))
import Scenes.Showcase

volcanicForgeCamera :: Resolution -> Camera
volcanicForgeCamera res =
    showcaseCamera
        res
        1400
        46
        (Vector3 850 420 (-980))
        (Vector3 0 125 390)
        0.05
        1637
        (Vector3 0.008 0.001 0)

volcanicForgeWorld :: IO (World, Lights)
volcanicForgeWorld = do
    basalt <- noise 0.085
    let obsidian = metal (Vector3 0.09 0.055 0.045) 0.18
        iron = metal (Vector3 0.48 0.27 0.12) 0.28
        ground = quad (Vector3 (-820) 0 (-240)) (Vector3 1640 0 0) (Vector3 0 0 1580) basalt
        forge =
            [ box (Vector3 (-250) 0 320) (Vector3 250 160 760) obsidian
            , box (Vector3 (-180) 160 390) (Vector3 180 330 690) iron
            , sphere (Vector3 0 330 540) 150 (glass 1.52)
            ]
        basaltColumns =
            [ box
                (Vector3 (x - 42) 0 (z - 42))
                (Vector3 (x + 42) height (z + 42))
                basalt
            | (x, z, height) <-
                [ (-650, 70, 270)
                , (-540, 145, 410)
                , (-690, 300, 520)
                , (-590, 510, 350)
                , (-720, 720, 460)
                , (-570, 940, 310)
                , (650, 70, 330)
                , (540, 180, 460)
                , (700, 350, 280)
                , (580, 560, 520)
                , (720, 780, 390)
                , (560, 1010, 440)
                ]
            ]
        chains =
            [ sphere (Vector3 x y z) 19 iron
            | x <- [-340, 340]
            , (y, z) <- zip [90, 135 .. 585] [180, 220 .. 620]
            ]
        smokePlumes =
            [ fogSphere (Vector3 x y z) radius density color
            | (x, y, z, radius, density, color) <-
                [ (-260, 420, 520, 190, 0.012, Vector3 0.11 0.095 0.09)
                , (180, 520, 610, 230, 0.009, Vector3 0.15 0.11 0.08)
                , (0, 690, 650, 270, 0.006, Vector3 0.12 0.09 0.07)
                ]
            ]
        lavaLights =
            [ quad
                (Vector3 x 5 z)
                (Vector3 0 0 length)
                (Vector3 width 0 0)
                (light color)
            | (x, z, width, length, color) <-
                [ (-430, -120, 105, 1350, Vector3 24 3.2 0.25)
                , (260, -80, 130, 1290, Vector3 20 1.8 0.1)
                , (-130, 720, 260, 510, Vector3 18 5 0.35)
                ]
            ]
        furnaceLight = sphere (Vector3 0 250 475) 72 (light (Vector3 28 7 0.5))
        objects = [ground] ++ forge ++ basaltColumns ++ chains ++ smokePlumes
    showcaseWorld objects (furnaceLight : lavaLights)

volcanicForgeScene :: Scene
volcanicForgeScene res = (volcanicForgeCamera res, volcanicForgeWorld)
