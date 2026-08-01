module Scenes.Chronosphere where

import Geometry.Scene (Camera, Lights, Scene, World)
import Math (Resolution, Vector3 (Vector3))
import Scenes.Showcase

chronosphereCamera :: Resolution -> Camera
chronosphereCamera res =
    showcaseCamera
        res
        1800
        39
        (Vector3 920 430 (-1280))
        (Vector3 0 275 420)
        0.055
        1938
        (Vector3 0.001 0.001 0.004)

chronosphereWorld :: IO (World, Lights)
chronosphereWorld = do
    let brass = metal (Vector3 0.82 0.52 0.16) 0.055
        silver = metal (Vector3 0.72 0.79 0.9) 0.025
        blackMetal = metal (Vector3 0.045 0.05 0.07) 0.015
        glassShell = glass 1.52
        stone = checker 95 (Vector3 0.19 0.18 0.2) (Vector3 0.055 0.055 0.07)
        floorPlane = quad (Vector3 (-900) 0 (-300)) (Vector3 1800 0 0) (Vector3 0 0 1800) stone
        dais =
            [ box (Vector3 (-360) 0 260) (Vector3 360 70 820) blackMetal
            , box (Vector3 (-285) 70 325) (Vector3 285 125 755) brass
            , box (Vector3 (-210) 125 390) (Vector3 210 170 690) blackMetal
            ]
        centralMachine =
            [ sphere (Vector3 0 410 540) 255 glassShell
            , sphere (Vector3 0 410 540) 176 brass
            , sphere (Vector3 0 410 540) 112 glassShell
            , sphere (Vector3 0 410 540) 48 silver
            ]
        squareOrbits =
            concat
                [ [ rotatedBox (Vector3 (-radius) 0 (-10)) (Vector3 radius 20 10) angle (Vector3 0 y 540) material
                  , rotatedBox (Vector3 (-10) 0 (-radius)) (Vector3 10 20 radius) angle (Vector3 0 y 540) material
                  ]
                | (radius, y, angle, material) <-
                    [ (335, 305, 0, brass)
                    , (390, 390, 24, silver)
                    , (445, 475, 53, brass)
                    , (500, 560, 78, silver)
                    ]
                ]
        timePillars =
            concat
                [ [ box (Vector3 (x - 48) 0 (z - 48)) (Vector3 (x + 48) height (z + 48)) blackMetal
                  , sphere (Vector3 x (height + 45) z) 52 glassShell
                  ]
                | (x, z, height) <-
                    [ (-650, 20, 390)
                    , (650, 20, 390)
                    , (-720, 740, 520)
                    , (720, 740, 520)
                    , (-580, 1260, 350)
                    , (580, 1260, 350)
                    ]
                ]
        orbitLights =
            [ sphere (Vector3 x y z) radius (light color)
            | (x, y, z, radius, color) <-
                [ (-330, 410, 540, 28, Vector3 22 5 1)
                , (330, 410, 540, 28, Vector3 2 11 24)
                , (0, 735, 540, 31, Vector3 12 4 22)
                , (0, 85, 540, 31, Vector3 2 20 12)
                , (-410, 560, 420, 24, Vector3 18 9 2)
                , (410, 280, 660, 24, Vector3 3 10 22)
                ]
            ]
        pillarLights =
            [ sphere (Vector3 x (height + 45) z) 19 (light color)
            | (index, (x, z, height)) <-
                zip
                    [0 :: Int ..]
                    [ (-650, 20, 390)
                    , (650, 20, 390)
                    , (-720, 740, 520)
                    , (720, 740, 520)
                    , (-580, 1260, 350)
                    , (580, 1260, 350)
                    ]
            , let color = if even index then Vector3 16 7 1.5 else Vector3 2 8 18
            ]
        coreLight = sphere (Vector3 0 410 540) 34 (light (Vector3 24 15 5))
        objects = [floorPlane] ++ dais ++ centralMachine ++ squareOrbits ++ timePillars
    showcaseWorld objects (coreLight : orbitLights ++ pillarLights)

chronosphereScene :: Scene
chronosphereScene res = (chronosphereCamera res, chronosphereWorld)
