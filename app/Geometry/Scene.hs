{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Geometry.Scene where

import Geometry.AABB (AABB (AABB, axisX, axisY, axisZ), aabbFromAABBs)
import Geometry.BVH (createBVHTree)
import Geometry.Hit (Hit, Hittable (boundingBox, hit), SomeHittable (SomeHittable), hitT)
import Geometry.Ray (Ray (Ray, direction, origin, time))
import Math (Color, Interval, RandomGenerator, Resolution, Vector3 (Vector3), cross, degreesToRadians, getX, getY, infinity, randomFloat, randomInUnitDisk, unit, (*.), (.*), (/.))

type ViewportResolution = (Float, Float)

type Scene = Resolution -> (Camera, IO (World, Lights))

data Camera = Camera
    { viewportResolution :: ViewportResolution
    , resolution :: Resolution
    , samplesPerPixel :: Integer
    , maxDepth :: Integer
    , fov :: Float
    , lookfrom :: Vector3
    , lookat :: Vector3
    , vup :: Vector3
    , defocusAngle :: Float
    , focusDist :: Float
    , defocusDiskU :: Vector3
    , defocusDiskV :: Vector3
    , backgroundColor :: Color
    }

fillViewportResolution :: Camera -> Camera
fillViewportResolution cam =
    let theta = degreesToRadians (fov cam)
        (resX, resY) = resolution cam
        h = tan (theta / 2)
        viewportHeight = 2 * h * (focusDist cam)
        viewportWidth = viewportHeight * ((fromInteger resX) / (fromInteger resY))
     in cam
            { viewportResolution = (viewportWidth, viewportHeight)
            , resolution = (resX, resY)
            }

fillDiskInfo :: Camera -> Camera
fillDiskInfo cam =
    let defocusRadius = (focusDist cam) * tan (degreesToRadians ((defocusAngle cam) / 2))
        w = unit ((lookfrom cam) - (lookat cam))
        u = unit (cross (vup cam) w)
        v = cross w u
     in cam
            { defocusDiskU = u *. defocusRadius
            , defocusDiskV = v *. defocusRadius
            }

calculateUV :: Camera -> (Vector3, Vector3)
calculateUV cam =
    let w = unit ((lookfrom cam) - (lookat cam))
        u = unit (cross (vup cam) w)
        v = cross w u
        (viewportWidth, viewportHeight) = viewportResolution cam
     in (u *. viewportWidth, (-v) *. viewportHeight)

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
        w = unit ((lookfrom cam) - (lookat cam))
        focalVector = (focusDist cam) .* w

        viewportUpperLeft =
            (lookfrom cam)
                - focalVector
                - viewportU /. 2
                - viewportV /. 2
     in
        viewportUpperLeft + 0.5 .* (deltaU + deltaV)

getSPPProperties :: Camera -> (Integer, Float)
getSPPProperties cam =
    let sqrtSpp = ceiling (sqrt (fromInteger (samplesPerPixel cam))) :: Integer
     in (sqrtSpp, 1 / (fromInteger sqrtSpp))

makeRayGenerator :: Camera -> RandomGenerator -> Integer -> Integer -> Integer -> Integer -> Float -> IO Ray
makeRayGenerator cam =
    let (deltaU, deltaV) = calculateDeltaUV cam
        pixel0Pos = calculateTopLeftPos cam
        camCenter = lookfrom cam
     in \generator x y di dj invSamplesPerPix -> do
            offset <- sampleSquareStratified generator di dj invSamplesPerPix
            rayOrigin <-
                if defocusAngle cam <= 0
                    then pure camCenter
                    else defocusDiskSample generator
            rayTime <- randomFloat generator
            let sampleLoc = pixel0Pos + ((fromInteger x + getX offset) .* deltaU) + ((fromInteger y + getY offset) .* deltaV)
            pure
                ( Ray
                    { origin = rayOrigin
                    , direction = sampleLoc - rayOrigin
                    , time = rayTime
                    }
                )
  where
    sampleSquareStratified :: RandomGenerator -> Integer -> Integer -> Float -> IO Vector3
    sampleSquareStratified gen si sj invSqrtSamples = do
        randX <- randomFloat gen
        randY <- randomFloat gen

        let px = ((fromInteger si + randX) * invSqrtSamples) - 0.5
            py = ((fromInteger sj + randY) * invSqrtSamples) - 0.5

        pure (Vector3 px py 0)

    defocusDiskSample :: RandomGenerator -> IO Vector3
    defocusDiskSample rand = do
        p <- randomInUnitDisk rand
        let result = lookfrom cam + ((getX p) .* (defocusDiskU cam)) + ((getY p) .* (defocusDiskV cam))
        pure result

makeRayForCoordinate :: RandomGenerator -> Camera -> Integer -> Integer -> Integer -> Integer -> Float -> IO Ray
makeRayForCoordinate generator cam = makeRayGenerator cam generator

newtype World = World
    { hittables :: [SomeHittable]
    }

newtype Lights = Lights
    {mainHittable :: SomeHittable}

makeLights :: [SomeHittable] -> Lights
makeLights objs =
    Lights
        { mainHittable = SomeHittable (createBVHTree objs)
        }

makeLightsForSingle :: SomeHittable -> Lights
makeLightsForSingle obj =
    Lights
        { mainHittable = obj
        }

addObject :: World -> SomeHittable -> World
addObject w obj =
    World
        { hittables = hittables w ++ [obj]
        }

getClosestHit :: World -> RandomGenerator -> Ray -> Interval -> Maybe Hit -> Float -> IO (Maybe Hit)
getClosestHit (World []) _ _ _ closestRay _ =
    pure closestRay
getClosestHit (World (object : rest)) gen r interval closestRay closestSoFar = do
    hitRes <- hit object gen r (fst interval, closestSoFar)
    case hitRes of
        Nothing ->
            getClosestHit (World rest) gen r interval closestRay closestSoFar
        Just result ->
            getClosestHit (World rest) gen r interval (Just result) (hitT result)

instance Hittable World where
    hit :: World -> RandomGenerator -> Ray -> Interval -> IO (Maybe Hit)
    hit world gen r interval =
        getClosestHit world gen r interval Nothing (snd interval)

    boundingBox :: World -> AABB
    boundingBox (World []) =
        AABB
            { axisX = (infinity, -infinity)
            , axisY = (infinity, -infinity)
            , axisZ = (infinity, -infinity)
            }
    boundingBox (World [obj]) = boundingBox obj
    boundingBox (World (obj : objs)) =
        let newWorld = World{hittables = objs}
         in aabbFromAABBs (boundingBox obj) (boundingBox newWorld)

newtype HittableList = HittableList
    { objects :: [SomeHittable]
    }

instance Hittable HittableList where
    hit :: HittableList -> RandomGenerator -> Ray -> Interval -> IO (Maybe Hit)
    hit (HittableList objs) gen ray interval =
        go objs Nothing (snd interval)
      where
        go [] closestHit _ =
            pure closestHit
        go (object : rest) closestHit closestT = do
            result <-
                hit
                    object
                    gen
                    ray
                    (fst interval, closestT)

            case result of
                Nothing ->
                    go rest closestHit closestT
                Just h ->
                    go rest (Just h) (hitT h)

    boundingBox :: HittableList -> AABB
    boundingBox (HittableList []) =
        error "No Objects can make an empty HittableList"
    boundingBox (HittableList (object : rest)) =
        foldl
            ( \box current ->
                aabbFromAABBs box (boundingBox current)
            )
            (boundingBox object)
            rest

packHittables :: [SomeHittable] -> SomeHittable
packHittables = SomeHittable . HittableList
