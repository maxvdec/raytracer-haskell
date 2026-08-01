module Scenes.Showcase where

import Geometry.BVH (createBVHTree)
import Geometry.ConstantMedium (makeMedium)
import Geometry.Hit (SomeHittable (SomeHittable))
import Geometry.Scene
import Geometry.Shapes (makeBox, makeQuad, makeSphere, rotateBy, translateBy)
import Graphics.Materials (SomeMaterial (SomeMaterial), makeDielectric, makeLambertian, makeLightFromColor, makeMetal, makeSolidLambertian)
import Graphics.Texture (SomeTexture (SomeTexture), makeCheckerFromColors, makeNoiseTexture)
import Math (Color, Point3, Resolution, Vector3 (Vector3), (|>))

showcaseCamera :: Resolution -> Integer -> Float -> Point3 -> Point3 -> Float -> Float -> Color -> Camera
showcaseCamera res samples fieldOfView origin target aperture focalDistance background =
    Camera
        { viewportResolution = (0, 0)
        , resolution = res
        , samplesPerPixel = samples
        , maxDepth = 50
        , fov = fieldOfView
        , lookfrom = origin
        , lookat = target
        , vup = Vector3 0 1 0
        , defocusAngle = aperture
        , focusDist = focalDistance
        , defocusDiskU = Vector3 0 0 0
        , defocusDiskV = Vector3 0 0 0
        , backgroundColor = background
        }
        |> fillViewportResolution
        |> fillDiskInfo

showcaseWorld :: [SomeHittable] -> [SomeHittable] -> IO (World, Lights)
showcaseWorld objects lights =
    let root = createBVHTree (objects ++ lights)
     in pure
            ( World{hittables = [SomeHittable root]}
            , makeLights lights
            )

solid :: Color -> SomeMaterial
solid = SomeMaterial . makeSolidLambertian

metal :: Color -> Float -> SomeMaterial
metal color roughness = SomeMaterial (makeMetal color roughness)

glass :: Float -> SomeMaterial
glass = SomeMaterial . makeDielectric

light :: Color -> SomeMaterial
light = SomeMaterial . makeLightFromColor

checker :: Float -> Color -> Color -> SomeMaterial
checker scale first second =
    SomeMaterial $
        makeLambertian $
            SomeTexture $
                makeCheckerFromColors scale first second

noise :: Float -> IO SomeMaterial
noise frequency = do
    texture <- makeNoiseTexture frequency
    pure (SomeMaterial (makeLambertian (SomeTexture texture)))

sphere :: Point3 -> Float -> SomeMaterial -> SomeHittable
sphere center radius material = SomeHittable (makeSphere center radius material)

quad :: Point3 -> Vector3 -> Vector3 -> SomeMaterial -> SomeHittable
quad origin u v material = SomeHittable (makeQuad origin u v material)

box :: Point3 -> Point3 -> SomeMaterial -> SomeHittable
box = makeBox

rotatedBox :: Point3 -> Point3 -> Float -> Vector3 -> SomeMaterial -> SomeHittable
rotatedBox low high angle offset material =
    makeBox low high material
        |> rotateBy angle
        |> translateBy offset

fogSphere :: Point3 -> Float -> Float -> Color -> SomeHittable
fogSphere center radius density color =
    SomeHittable $
        makeMedium
            (sphere center radius (glass 1.5))
            density
            color
