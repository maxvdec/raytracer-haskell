module Scenes.OrbitalObservatory where

import Geometry.Scene (Camera, Lights, Scene, World)
import Graphics.Materials (SomeMaterial (SomeMaterial), makeLambertian)
import Graphics.Texture (SomeTexture (SomeTexture), loadImageTexture)
import Math (Resolution, Vector3 (Vector3))
import Scenes.Showcase

orbitalObservatoryCamera :: Resolution -> Camera
orbitalObservatoryCamera res =
    showcaseCamera
        res
        1200
        44
        (Vector3 760 360 (-980))
        (Vector3 0 230 230)
        0.04
        1434
        (Vector3 0.001 0.002 0.009)

orbitalObservatoryWorld :: IO (World, Lights)
orbitalObservatoryWorld = do
    earthTexture <- loadImageTexture "./textures/earthmap.jpg"
    let earth = SomeMaterial (makeLambertian (SomeTexture earthTexture))
        hull = metal (Vector3 0.62 0.68 0.76) 0.16
        darkHull = metal (Vector3 0.09 0.12 0.18) 0.05
        glazing = glass 1.5
        floorMaterial = checker 84 (Vector3 0.32 0.36 0.42) (Vector3 0.055 0.065 0.085)
        floorPlane = quad (Vector3 (-620) 0 (-180)) (Vector3 1240 0 0) (Vector3 0 0 1260) floorMaterial
        planet = sphere (Vector3 0 300 390) 210 earth
        atmosphere = sphere (Vector3 0 300 390) 224 glazing
        pedestal = box (Vector3 (-125) 0 270) (Vector3 125 85 510) darkHull
        ringFrames =
            concat
                [ [ rotatedBox (Vector3 (-330) 0 (-12)) (Vector3 330 18 12) angle (Vector3 0 y 390) hull
                  , rotatedBox (Vector3 (-12) 0 (-330)) (Vector3 12 18 330) angle (Vector3 0 y 390) hull
                  ]
                | (angle, y) <- [(0, 70), (28, 105), (56, 140)]
                ]
        consoles =
            [ rotatedBox
                (Vector3 (-65) 0 (-35))
                (Vector3 65 72 35)
                angle
                (Vector3 x 0 z)
                darkHull
            | (x, z, angle) <- [(-390, 120, -22), (390, 120, 22), (-430, 620, -150), (430, 620, 150)]
            ]
        holograms =
            [ sphere (Vector3 x y z) radius glazing
            | (x, y, z, radius) <-
                [ (-380, 142, 112, 58)
                , (380, 142, 112, 58)
                , (-420, 150, 615, 72)
                , (420, 150, 615, 72)
                ]
            ]
        starLights =
            [ sphere
                (Vector3 x y z)
                radius
                (light color)
            | (x, y, z, radius, color) <-
                [ (-460, 560, 20, 28, Vector3 12 16 25)
                , (430, 610, 180, 34, Vector3 18 10 4)
                , (-520, 470, 780, 24, Vector3 8 13 25)
                , (510, 520, 860, 31, Vector3 20 8 3)
                , (0, 700, 520, 40, Vector3 12 14 20)
                ]
            ]
        objects = [floorPlane, planet, atmosphere, pedestal] ++ ringFrames ++ consoles ++ holograms
    showcaseWorld objects starLights

orbitalObservatoryScene :: Scene
orbitalObservatoryScene res = (orbitalObservatoryCamera res, orbitalObservatoryWorld)
