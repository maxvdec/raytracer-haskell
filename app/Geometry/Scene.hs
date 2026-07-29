{-# LANGUAGE InstanceSigs #-}

module Geometry.Scene where

import Geometry.Ray (Ray (Ray, direction, origin))
import Geometry.Shapes (Hit (t), Hittable (hit), SomeHittable)
import Math (Interval, Point3, Resolution, TextureCoord, Vector3 (Vector3), (+.), (.*), (/.))

type ViewportResolution = (Float, Float)

createViewportResolutionFromHeight :: Float -> Resolution -> ViewportResolution
createViewportResolutionFromHeight viewH (w, h) =
    (viewH * ((fromInteger w) / (fromInteger h)), viewH)

data Camera = Camera
    { focalLength :: Float
    , viewportResolution :: ViewportResolution
    , cameraCenter :: Point3
    , resolution :: Resolution
    }

calculateUV :: Camera -> (Vector3, Vector3)
calculateUV cam =
    ( (Vector3 (fst (viewportResolution cam)) 0 0)
    , (Vector3 0 (-snd (viewportResolution cam)) 0)
    )

calculateDeltaUV :: Camera -> (Vector3, Vector3)
calculateDeltaUV cam =
    let (viewportU, viewportV) = calculateUV cam
        (width, height) = resolution cam
     in ( viewportU /. fromInteger width
        , viewportV /. fromInteger height
        )

calculateTopLeftPos :: Camera -> Vector3
calculateTopLeftPos cam =
    let camCenter = cameraCenter cam
        (viewportU, viewportV) = calculateUV cam
        (deltaU, deltaV) = calculateDeltaUV cam
        focalVector = Vector3 0 0 (focalLength cam)

        viewportUpperLeft =
            camCenter
                - focalVector
                - viewportU /. 2
                - viewportV /. 2
     in viewportUpperLeft + 0.5 .* (deltaU + deltaV)

makeRayForCoordinate :: Camera -> Integer -> Integer -> Ray
makeRayForCoordinate cam x y =
    let (deltaU, deltaV) = calculateDeltaUV cam
        pixel0Pos = calculateTopLeftPos cam
        camCenter = cameraCenter cam
        pixelCenter =
            pixel0Pos
                + fromInteger x .* deltaU
                + fromInteger y .* deltaV
        rayDir = pixelCenter - camCenter
     in Ray
            { origin = camCenter
            , direction = rayDir
            }

data World = World
    { hittables :: [SomeHittable]
    }

addObject :: World -> SomeHittable -> World
addObject w obj =
    World
        { hittables = (hittables w) ++ [obj]
        }

getClosestHit :: World -> Ray -> Interval -> Maybe Hit -> Float -> Maybe Hit
getClosestHit (World []) _ _ closestRay _ =
    closestRay
getClosestHit (World (object : rest)) r interval closestRay closestSoFar =
    case hit object r (fst interval, closestSoFar) of
        Nothing ->
            getClosestHit (World rest) r interval closestRay closestSoFar
        Just result ->
            getClosestHit (World rest) r interval (Just result) (t result)

instance Hittable World where
    hit :: World -> Ray -> Interval -> Maybe Hit
    hit world r interval =
        getClosestHit world r interval Nothing (snd interval)