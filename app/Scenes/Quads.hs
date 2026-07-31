module Scenes.Quads where

import Geometry.BVH (createBVHTree)
import Geometry.Hit (SomeHittable (SomeHittable))
import Geometry.Scene
import Geometry.Shapes (makeQuad, makeSphere)
import Graphics.Materials (SomeMaterial (SomeMaterial), makeLambertian, makeSolidLambertian)
import Graphics.Texture (SomeTexture (SomeTexture), loadImageTexture)
import Math (Resolution, Vector3 (Vector3), (|>))

quadsSceneCamera :: Resolution -> Camera
quadsSceneCamera res =
    Camera
        { viewportResolution = (0, 0)
        , resolution = res
        , samplesPerPixel = 100
        , maxDepth = 50
        , fov = 80
        , lookfrom = (Vector3 0 0 9)
        , lookat = (Vector3 0 0 0)
        , vup = (Vector3 0 1 0)
        , defocusAngle = 0
        , focusDist = 3.58
        , defocusDiskU = (Vector3 0 0 0)
        , defocusDiskV = (Vector3 0 0 0)
        }
        |> fillViewportResolution
        |> fillDiskInfo

quadsSceneWorld :: IO World
quadsSceneWorld = do
    let leftRed = makeSolidLambertian (Vector3 1 0.2 0.2)
        backGreen = makeSolidLambertian (Vector3 0.2 1 0.2)
        rightBlue = makeSolidLambertian (Vector3 0.2 0.2 1)
        upperOrange = makeSolidLambertian (Vector3 1 0.5 0)
        lowerTeal = makeSolidLambertian (Vector3 0.2 0.8 0.8)

        leftQuad = makeQuad (Vector3 (-3) (-2) 5) (Vector3 0 0 (-4)) (Vector3 0 4 0) (SomeMaterial leftRed)
        backQuad = makeQuad (Vector3 (-2) (-2) 0) (Vector3 4 0 0) (Vector3 0 4 0) (SomeMaterial backGreen)
        rightQuad = makeQuad (Vector3 3 (-2) 1) (Vector3 0 0 4) (Vector3 0 4 0) (SomeMaterial rightBlue)
        upperQuad = makeQuad (Vector3 (-2) 3 1) (Vector3 4 0 0) (Vector3 0 0 4) (SomeMaterial upperOrange)
        lowerQuad = makeQuad (Vector3 (-2) (-3) 5) (Vector3 4 0 0) (Vector3 0 0 (-4)) (SomeMaterial lowerTeal)
        scene = [SomeHittable leftQuad, SomeHittable backQuad, SomeHittable rightQuad, SomeHittable upperQuad, SomeHittable lowerQuad]
        root = createBVHTree scene

    pure (World{hittables = [SomeHittable root]})

quadsScene :: Scene
quadsScene res = (quadsSceneCamera res, quadsSceneWorld)
