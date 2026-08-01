module Scenes.CornellBox where

import Geometry.BVH (createBVHTree)
import Geometry.Hit (SomeHittable (SomeHittable))
import Geometry.Scene
import Geometry.Shapes (makeBox, makeQuad, makeSphere, rotateBy, translateBy)
import Graphics.Materials (Lambertian (lambertianTexture), SomeMaterial (SomeMaterial), makeLambertian, makeLightFromColor, makeSolidLambertian)
import Graphics.Texture (SomeTexture (SomeTexture), loadImageTexture, makeNoiseTexture)
import Math (Resolution, Vector3 (Vector3), (|>), (||>))

cornellBoxCamera :: Resolution -> Camera
cornellBoxCamera res =
    Camera
        { viewportResolution = (0, 0)
        , resolution = res
        , samplesPerPixel = 30
        , maxDepth = 50
        , fov = 40
        , lookfrom = (Vector3 278 278 (-800))
        , lookat = (Vector3 278 278 0)
        , vup = (Vector3 0 1 0)
        , defocusAngle = 0
        , focusDist = 3.58
        , defocusDiskU = (Vector3 0 0 0)
        , defocusDiskV = (Vector3 0 0 0)
        , backgroundColor = (Vector3 0 0 0)
        }
        |> fillViewportResolution
        |> fillDiskInfo

cornellBoxWorld :: IO World
cornellBoxWorld = do
    let red = makeSolidLambertian (Vector3 0.65 0.05 0.05)
        white = makeSolidLambertian (Vector3 0.73 0.73 0.73)
        green = makeSolidLambertian (Vector3 0.12 0.45 0.15)
        light = makeLightFromColor (Vector3 15 15 15)

        left = makeQuad (Vector3 555 0 0) (Vector3 0 555 0) (Vector3 0 0 555) (SomeMaterial green)
        right = makeQuad (Vector3 0 0 0) (Vector3 0 555 0) (Vector3 0 0 555) (SomeMaterial red)
        lightPrim = makeQuad (Vector3 343 554 332) (Vector3 (-130) 0 0) (Vector3 0 0 (-105)) (SomeMaterial light)
        wall = makeQuad (Vector3 0 0 0) (Vector3 555 0 0) (Vector3 0 0 555) (SomeMaterial white)
        wall' = makeQuad (Vector3 555 555 555) (Vector3 (-555) 0 0) (Vector3 0 0 (-555)) (SomeMaterial white)
        wall'' = makeQuad (Vector3 0 0 555) (Vector3 555 0 0) (Vector3 0 555 0) (SomeMaterial white)

        box1 = makeBox (Vector3 0 0 0) (Vector3 165 330 165) (SomeMaterial white)
        box2 = makeBox (Vector3 0 0 0) (Vector3 165 165 165) (SomeMaterial white)

        scene =
            [ SomeHittable left
            , SomeHittable right
            , SomeHittable lightPrim
            , SomeHittable wall
            , SomeHittable wall'
            , SomeHittable wall''
            , (box1 |> rotateBy 15 |> translateBy (Vector3 265 0 295))
            , (box2 |> rotateBy (-18) |> translateBy (Vector3 130 0 65))
            ]

        root = createBVHTree scene

    pure (World{hittables = [SomeHittable root]})

cornellBoxScene :: Scene
cornellBoxScene res = (cornellBoxCamera res, cornellBoxWorld)
