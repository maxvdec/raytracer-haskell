module Scenes.CrystalCathedral where

import Geometry.Scene (Camera, Lights, Scene, World)
import Math (Resolution, Vector3 (Vector3))
import Scenes.Showcase

crystalCathedralCamera :: Resolution -> Camera
crystalCathedralCamera res =
    showcaseCamera
        res
        1200
        42
        (Vector3 0 245 (-980))
        (Vector3 0 175 310)
        0.06
        1292
        (Vector3 0.002 0.004 0.012)

crystalCathedralWorld :: IO (World, Lights)
crystalCathedralWorld = do
    marble <- noise 0.055
    let crystal = glass 1.52
        gold = metal (Vector3 0.92 0.68 0.24) 0.08
        darkStone = solid (Vector3 0.055 0.065 0.09)
        floorMaterial = checker 90 (Vector3 0.72 0.76 0.84) (Vector3 0.12 0.14 0.2)
        floorPlane = quad (Vector3 (-460) 0 (-120)) (Vector3 920 0 0) (Vector3 0 0 1180) floorMaterial
        rearWall = quad (Vector3 (-460) 0 1040) (Vector3 920 0 0) (Vector3 0 520 0) darkStone
        columns =
            concat
                [ [ box (Vector3 (-390) 0 z) (Vector3 (-330) 330 (z + 60)) marble
                  , box (Vector3 330 0 z) (Vector3 390 330 (z + 60)) marble
                  , sphere (Vector3 (-360) 350 (z + 30)) 58 crystal
                  , sphere (Vector3 360 350 (z + 30)) 58 crystal
                  ]
                | z <- [20, 230 .. 860]
                ]
        naveCrystals =
            [ sphere (Vector3 x radius z) radius crystal
            | (x, radius, z) <-
                [ (-150, 72, 95)
                , (145, 96, 210)
                , (-110, 118, 370)
                , (125, 82, 540)
                , (-145, 102, 700)
                , (80, 132, 880)
                ]
            ]
        altar =
            [ box (Vector3 (-190) 0 880) (Vector3 190 70 1020) darkStone
            , sphere (Vector3 0 205 965) 130 crystal
            , sphere (Vector3 0 205 965) 54 gold
            ]
        goldRails =
            concat
                [ [ box (Vector3 (-258) 8 z) (Vector3 (-246) 50 (z + 135)) gold
                  , box (Vector3 246 8 z) (Vector3 258 50 (z + 135)) gold
                  ]
                | z <- [40, 215 .. 740]
                ]
        ceilingLights =
            [ quad
                (Vector3 (-105) 470 z)
                (Vector3 210 0 0)
                (Vector3 0 0 95)
                (light (Vector3 17 19 24))
            | z <- [70, 270 .. 870]
            ]
        roseLight = sphere (Vector3 0 325 1018) 76 (light (Vector3 14 5 2.5))
        objects = [floorPlane, rearWall] ++ columns ++ naveCrystals ++ altar ++ goldRails
    showcaseWorld objects (roseLight : ceilingLights)

crystalCathedralScene :: Scene
crystalCathedralScene res = (crystalCathedralCamera res, crystalCathedralWorld)
