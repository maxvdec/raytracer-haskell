module Scenes.World where

import Geometry.BVH (createBVHTree)
import Geometry.Hit (SomeHittable (SomeHittable))
import Geometry.Scene
import Geometry.Shapes (makeSphere)
import Graphics.Materials (SomeMaterial (SomeMaterial), makeLambertian)
import Graphics.Texture (SomeTexture (SomeTexture), loadImageTexture)
import Math (Resolution, Vector3 (Vector3), (|>))

worldSceneCamera :: Resolution -> Camera
worldSceneCamera res =
    Camera
        { viewportResolution = (0, 0)
        , resolution = res
        , samplesPerPixel = 100
        , maxDepth = 50
        , fov = 20
        , lookfrom = (Vector3 0 0 12)
        , lookat = (Vector3 0 0 0)
        , vup = (Vector3 0 1 0)
        , defocusAngle = 0
        , focusDist = 3.58
        , defocusDiskU = (Vector3 0 0 0)
        , defocusDiskV = (Vector3 0 0 0)
        }
        |> fillViewportResolution
        |> fillDiskInfo

worldSceneWorld :: IO World
worldSceneWorld = do
    worldTexture <- loadImageTexture "./textures/earthmap.jpg"
    let surface = makeLambertian (SomeTexture worldTexture)
        globe = makeSphere (Vector3 0 0 0) 2 (SomeMaterial surface)
        scene = [SomeHittable globe]
        root = createBVHTree scene

    pure (World{hittables = [SomeHittable root]})

worldScene :: Scene
worldScene res = (worldSceneCamera res, worldSceneWorld)
