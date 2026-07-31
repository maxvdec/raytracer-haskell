module Scenes.PerlinSpheres where

import Geometry.BVH (createBVHTree)
import Geometry.Hit (SomeHittable (SomeHittable))
import Geometry.Scene
import Geometry.Shapes (makeSphere)
import Graphics.Materials (SomeMaterial (SomeMaterial), makeLambertian)
import Graphics.Texture (SomeTexture (SomeTexture), loadImageTexture, makeNoiseTexture)
import Math (Resolution, Vector3 (Vector3), (|>))

perlinSpheresCamera :: Resolution -> Camera
perlinSpheresCamera res =
    Camera
        { viewportResolution = (0, 0)
        , resolution = res
        , samplesPerPixel = 100
        , maxDepth = 50
        , fov = 20
        , lookfrom = (Vector3 12 2 3)
        , lookat = (Vector3 0 0 0)
        , vup = (Vector3 0 1 0)
        , defocusAngle = 0
        , focusDist = 3.58
        , defocusDiskU = (Vector3 0 0 0)
        , defocusDiskV = (Vector3 0 0 0)
        }
        |> fillViewportResolution
        |> fillDiskInfo

perlinSpheresWorld :: IO World
perlinSpheresWorld = do
    noiseTexture <- makeNoiseTexture
    let surface = makeLambertian (SomeTexture noiseTexture)
        ground = makeSphere (Vector3 0 (-1000) 0) 1000 (SomeMaterial surface)
        sphere = makeSphere (Vector3 0 2 0) 2 (SomeMaterial surface)
        scene = [SomeHittable ground, SomeHittable sphere]
        root = createBVHTree scene

    pure (World{hittables = [SomeHittable root]})

perlinSpheresScene :: Scene
perlinSpheresScene res = (perlinSpheresCamera res, perlinSpheresWorld)
