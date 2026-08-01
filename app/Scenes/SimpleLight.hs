module Scenes.SimpleLight where

import Geometry.BVH (createBVHTree)
import Geometry.Hit (SomeHittable (SomeHittable))
import Geometry.Scene
import Geometry.Shapes (makeQuad, makeSphere)
import Graphics.Materials (SomeMaterial (SomeMaterial), makeLambertian, makeLightFromColor)
import Graphics.Texture (SomeTexture (SomeTexture), loadImageTexture, makeNoiseTexture)
import Math (Resolution, Vector3 (Vector3), (|>))

simpleLightCamera :: Resolution -> Camera
simpleLightCamera res =
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
        , backgroundColor = (Vector3 0.0 0.0 0)
        }
        |> fillViewportResolution
        |> fillDiskInfo

simpleLightWorld :: IO (World, Lights)
simpleLightWorld = do
    noiseTexture <- makeNoiseTexture 4
    let surface = makeLambertian (SomeTexture noiseTexture)
        diffuse = makeLightFromColor (Vector3 4 4 4)
        ground = makeSphere (Vector3 0 (-1000) 0) 1000 (SomeMaterial surface)
        sphere = makeSphere (Vector3 0 2 0) 2 (SomeMaterial surface)
        light = makeQuad (Vector3 3 1 (-2)) (Vector3 2 0 0) (Vector3 0 2 0) (SomeMaterial diffuse)
        sphereLight = makeSphere (Vector3 0 7 0) 2 (SomeMaterial diffuse)
        scene = [SomeHittable ground, SomeHittable sphere, SomeHittable sphereLight, SomeHittable light]
        root = createBVHTree scene

    pure (World{hittables = [SomeHittable root]}, makeLightsForSingle (SomeHittable light))

simpleLightScene :: Scene
simpleLightScene res = (simpleLightCamera res, simpleLightWorld)
