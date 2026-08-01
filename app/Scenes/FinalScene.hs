module Scenes.FinalScene where

import Control.Monad (replicateM)
import Geometry.BVH (createBVHTree)
import Geometry.ConstantMedium (makeMedium)
import Geometry.Hit (SomeHittable (SomeHittable))
import Geometry.Scene
import Geometry.Shapes (
    makeAnimatedSphere,
    makeBox,
    makeQuad,
    makeSphere,
    rotateBy,
    translateBy,
 )
import Graphics.Materials (
    SomeMaterial (SomeMaterial),
    makeDielectric,
    makeLambertian,
    makeLightFromColor,
    makeMetal,
    makeSolidLambertian,
 )
import Graphics.Texture (
    SomeTexture (SomeTexture),
    loadImageTexture,
    makeNoiseTexture,
 )
import Math (
    Resolution,
    Vector3 (Vector3),
    (|>),
 )
import System.Random (randomRIO)

finalSceneCamera :: Resolution -> Camera
finalSceneCamera res =
    Camera
        { viewportResolution = (0, 0)
        , resolution = res
        , samplesPerPixel = 1000
        , maxDepth = 50
        , fov = 40
        , lookfrom = Vector3 478 278 (-600)
        , lookat = Vector3 278 278 0
        , vup = Vector3 0 1 0
        , defocusAngle = 0
        , focusDist = 10
        , defocusDiskU = Vector3 0 0 0
        , defocusDiskV = Vector3 0 0 0
        , backgroundColor = Vector3 0 0 0
        }
        |> fillViewportResolution
        |> fillDiskInfo

finalSceneWorld :: IO World
finalSceneWorld = do
    earthTexture <- loadImageTexture "./textures/earthmap.jpg"
    noiseTexture <- makeNoiseTexture 0.2

    let groundMaterial =
            SomeMaterial $
                makeSolidLambertian (Vector3 0.48 0.83 0.53)

        whiteMaterial =
            SomeMaterial $
                makeSolidLambertian (Vector3 0.73 0.73 0.73)

        lightMaterial =
            SomeMaterial $
                makeLightFromColor (Vector3 7 7 7)

        movingSphereMaterial =
            SomeMaterial $
                makeSolidLambertian (Vector3 0.7 0.3 0.1)

        glassMaterial =
            SomeMaterial $
                makeDielectric 1.5

        metalMaterial =
            SomeMaterial $
                makeMetal (Vector3 0.8 0.8 0.9) 1.0

        earthMaterial =
            SomeMaterial $
                makeLambertian (SomeTexture earthTexture)

        noiseMaterial =
            SomeMaterial $
                makeLambertian (SomeTexture noiseTexture)

    groundBoxes <-
        mapM
            (uncurry (makeGroundBox groundMaterial))
            [ (i, j)
            | i <- [0 .. 19]
            , j <- [0 .. 19]
            ]

    smallSpheres <-
        replicateM 1000 $
            makeRandomSphere whiteMaterial

    let groundBVH =
            createBVHTree groundBoxes

        light =
            makeQuad
                (Vector3 123 554 147)
                (Vector3 300 0 0)
                (Vector3 0 0 265)
                lightMaterial

        center1 =
            Vector3 400 400 200

        center2 =
            Vector3 430 400 200

        movingSphere =
            makeAnimatedSphere
                center1
                center2
                50
                movingSphereMaterial

        glassSphere =
            makeSphere
                (Vector3 260 150 45)
                50
                glassMaterial

        metalSphere =
            makeSphere
                (Vector3 0 150 145)
                50
                metalMaterial

        smokeBoundary =
            makeSphere
                (Vector3 360 150 145)
                70
                glassMaterial

        coloredMedium =
            makeMedium
                (SomeHittable smokeBoundary)
                0.2
                (Vector3 0.2 0.4 0.9)

        atmosphereBoundary =
            makeSphere
                (Vector3 0 0 0)
                5000
                glassMaterial

        atmosphere =
            makeMedium
                (SomeHittable atmosphereBoundary)
                0.0001
                (Vector3 1 1 1)

        earthSphere =
            makeSphere
                (Vector3 400 200 400)
                100
                earthMaterial

        perlinSphere =
            makeSphere
                (Vector3 220 280 300)
                80
                noiseMaterial

        sphereClusterBVH =
            createBVHTree smallSpheres

        sphereCluster =
            SomeHittable sphereClusterBVH
                |> rotateBy 15
                |> translateBy (Vector3 (-100) 270 395)

        scene =
            [ SomeHittable groundBVH
            , SomeHittable light
            , SomeHittable movingSphere
            , SomeHittable glassSphere
            , SomeHittable metalSphere
            , SomeHittable smokeBoundary
            , SomeHittable coloredMedium
            , SomeHittable atmosphere
            , SomeHittable earthSphere
            , SomeHittable perlinSphere
            , sphereCluster
            ]

        root =
            createBVHTree scene

    pure
        World
            { hittables = [SomeHittable root]
            }

makeGroundBox ::
    SomeMaterial ->
    Int ->
    Int ->
    IO SomeHittable
makeGroundBox material i j = do
    height <- randomRIO (1, 101)

    let width = 100

        x0 =
            -1000 + fromIntegral i * width

        z0 =
            -1000 + fromIntegral j * width

        x1 =
            x0 + width

        y1 =
            height

        z1 =
            z0 + width

    pure $
        makeBox
            (Vector3 x0 0 z0)
            (Vector3 x1 y1 z1)
            material

makeRandomSphere ::
    SomeMaterial ->
    IO SomeHittable
makeRandomSphere material = do
    x <- randomRIO (0, 165)
    y <- randomRIO (0, 165)
    z <- randomRIO (0, 165)

    pure $
        SomeHittable $
            makeSphere
                (Vector3 x y z)
                10
                material

finalScene :: Scene
finalScene res =
    (finalSceneCamera res, finalSceneWorld)
