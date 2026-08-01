module Scenes.MirrorTemple where

import Geometry.Scene (Camera, Lights, Scene, World)
import Math (Resolution, Vector3 (Vector3))
import Scenes.Showcase

mirrorTempleCamera :: Resolution -> Camera
mirrorTempleCamera res =
    showcaseCamera
        res
        1800
        50
        (Vector3 0 195 (-350)) -- -930 original
        (Vector3 0 190 440)
        0.035
        1370
        (Vector3 0 0 0)

mirrorTempleWorld :: IO (World, Lights)
mirrorTempleWorld = do
    let mirror = metal (Vector3 0.94 0.96 1) 0.005
        darkMirror = metal (Vector3 0.17 0.19 0.24) 0.015
        ivory = solid (Vector3 0.72 0.7 0.66)
        crystal = glass 1.5
        floorPlane = quad (Vector3 (-500) 0 (-100)) (Vector3 1000 0 0) (Vector3 0 0 1500) mirror
        ceiling = quad (Vector3 (-500) 570 1400) (Vector3 1000 0 0) (Vector3 0 0 (-1500)) darkMirror
        leftWall = quad (Vector3 (-500) 0 1400) (Vector3 0 0 (-1500)) (Vector3 0 570 0) mirror
        rightWall = quad (Vector3 500 0 (-100)) (Vector3 0 0 1500) (Vector3 0 570 0) mirror
        rearWall = quad (Vector3 500 0 1400) (Vector3 (-1000) 0 0) (Vector3 0 570 0) darkMirror
        portalFrames =
            concat
                [ [ box (Vector3 (-360) 0 z) (Vector3 (-310) 430 (z + 46)) ivory
                  , box (Vector3 310 0 z) (Vector3 360 430 (z + 46)) ivory
                  , box (Vector3 (-360) 395 z) (Vector3 360 445 (z + 46)) ivory
                  ]
                | z <- [40, 260 .. 1140]
                ]
        floatingSculptures =
            [ sphere (Vector3 x y z) radius material
            | (x, y, z, radius, material) <-
                [ (-190, 125, 150, 82, crystal)
                , (185, 165, 340, 105, metal (Vector3 0.86 0.62 0.22) 0.03)
                , (-140, 210, 545, 125, crystal)
                , (180, 145, 760, 92, metal (Vector3 0.72 0.78 0.9) 0.01)
                , (-170, 175, 970, 110, crystal)
                , (0, 230, 1240, 155, metal (Vector3 0.9 0.72 0.3) 0.02)
                ]
            ]
        prismCores =
            [ sphere (Vector3 x y z) radius (light color)
            | (x, y, z, radius, color) <-
                [ (-190, 125, 150, 23, Vector3 20 2 5)
                , (185, 165, 340, 20, Vector3 2 9 22)
                , (-140, 210, 545, 26, Vector3 2 20 12)
                , (180, 145, 760, 22, Vector3 18 3 20)
                , (-170, 175, 970, 25, Vector3 2 12 24)
                , (0, 230, 1240, 34, Vector3 24 8 1)
                ]
            ]
        ceilingStrips =
            [ quad
                (Vector3 (-250) 565 z)
                (Vector3 500 0 0)
                (Vector3 0 0 65)
                (light (Vector3 9 10 13))
            | z <- [20, 230 .. 1280]
            ]
        objects = [floorPlane, ceiling, leftWall, rightWall, rearWall] ++ portalFrames ++ floatingSculptures
    showcaseWorld objects (prismCores ++ ceilingStrips)

mirrorTempleScene :: Scene
mirrorTempleScene res = (mirrorTempleCamera res, mirrorTempleWorld)
