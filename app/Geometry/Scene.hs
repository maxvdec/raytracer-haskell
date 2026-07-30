{-# LANGUAGE InstanceSigs #-}

module Geometry.Scene where

import Geometry.Hit (Hit, Hittable (hit), SomeHittable, hitT)
import Geometry.Ray (Ray (Ray, direction, origin))
import Math (Interval, Point3, RandomGenerator, Resolution, Vector3 (Vector3), getX, getY, randomInRange,  (.*), (/.), degreesToRadians, vecLength, unit, cross, (*.))

type ViewportResolution = (Float, Float)

data Camera = Camera
    { viewportResolution :: ViewportResolution
    , resolution :: Resolution
    , samplesPerPixel :: Integer
    , maxDepth :: Integer
    , fov :: Float 
    , lookfrom :: Vector3
    , lookat :: Vector3
    , vup :: Vector3
    }

fillViewportResolution :: Camera -> Camera
fillViewportResolution cam =
    let flength = vecLength ((lookfrom cam) - (lookat cam)) 
        theta = degreesToRadians flength 
        (resX, resY) = resolution cam
        h = tan (theta / 2)
        viewportHeight = 2 * h * (fov cam)
        viewportWidth = viewportHeight * ((fromInteger resX) / (fromInteger resY))
    in
        Camera {
            viewportResolution = (viewportWidth, viewportHeight)
            , resolution = (resX, resY)
            , samplesPerPixel = (samplesPerPixel cam)
            , maxDepth = (maxDepth cam)
            , fov = (fov cam)
            , lookfrom = (lookfrom cam)
            , lookat = (lookat cam)
            , vup = (vup cam)
        }

calculateUV :: Camera -> (Vector3, Vector3)
calculateUV cam =
    let w = unit ((lookfrom cam) - (lookat cam)) 
        u = unit (cross (vup cam) w)
        v = cross w u
        (viewportWidth, viewportHeight) = viewportResolution cam in
    (u *. viewportWidth, (-v) *. viewportHeight)

calculateDeltaUV :: Camera -> (Vector3, Vector3)
calculateDeltaUV cam =
    let (viewportU, viewportV) = calculateUV cam
        (width, height) = resolution cam
     in ( viewportU /. fromInteger width
        , viewportV /. fromInteger height
        )

calculateTopLeftPos :: Camera -> Vector3
calculateTopLeftPos cam =
    let 
        (viewportU, viewportV) = calculateUV cam
        (deltaU, deltaV) = calculateDeltaUV cam
        focalLength = vecLength ((lookfrom cam) - (lookat cam)) 
        w = unit ((lookfrom cam) - (lookat cam)) 
        focalVector = focalLength .* w 

        viewportUpperLeft =
            (lookfrom cam)
                - focalVector
                - viewportU /. 2
                - viewportV /. 2
     in viewportUpperLeft + 0.5 .* (deltaU + deltaV)

makeRayForCoordinate :: RandomGenerator -> Camera -> Integer -> Integer -> IO Ray
makeRayForCoordinate generator cam x y =
    let (deltaU, deltaV) = calculateDeltaUV cam
        pixel0Pos = calculateTopLeftPos cam
        camCenter = lookfrom cam 
     in do
            offset <- sampleSquare
            let sampleLoc = pixel0Pos + ((fromInteger x + getX offset) .* deltaU) + ((fromInteger y + getY offset) .* deltaV)
            pure
                ( Ray
                    { origin = camCenter
                    , direction = sampleLoc - camCenter
                    }
                )
  where
    sampleSquare :: IO Vector3
    sampleSquare = do
        xOffset <- randomInRange generator (-0.5, 0.5)
        yOffset <- randomInRange generator (-0.5, 0.5)

        pure (Vector3 xOffset yOffset 0)

newtype World = World
    { hittables :: [SomeHittable]
    }

addObject :: World -> SomeHittable -> World
addObject w obj =
    World
        { hittables = hittables w ++ [obj]
        }

getClosestHit :: World -> Ray -> Interval -> Maybe Hit -> Float -> Maybe Hit
getClosestHit (World []) _ _ closestRay _ =
    closestRay
getClosestHit (World (object : rest)) r interval closestRay closestSoFar =
    case hit object r (fst interval, closestSoFar) of
        Nothing ->
            getClosestHit (World rest) r interval closestRay closestSoFar
        Just result ->
            getClosestHit (World rest) r interval (Just result) (hitT result)

instance Hittable World where
    hit :: World -> Ray -> Interval -> Maybe Hit
    hit world r interval =
        getClosestHit world r interval Nothing (snd interval)
