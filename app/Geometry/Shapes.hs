{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Geometry.Shapes where

import Control.Applicative
import Control.Exception.Base (NoMatchingContinuationPrompt)
import Control.Monad (when)
import GHC.Float (roundFloat)
import Geometry.AABB (AABB (axisX, axisY, axisZ), aabbFromAABBs, aabbFromPoints)
import Geometry.Hit (Hit (..), Hittable (boundingBox, hit, pdfObjectValue, randomPdf), SomeHittable (SomeHittable), hitNormal, hitP, hitT, makeHit, setFaceNormal)
import Geometry.HitInfo (HitInfo (normal, p, uv))
import Geometry.Ray (Ray (Ray, direction, origin, time), at, makeRay)
import Geometry.Scene (packHittables)
import Graphics.Materials (SomeMaterial (SomeMaterial))
import Math (Addable (..), Interval, Point3, RandomGenerator, TextureCoord, Vector3 (Vector3), contains, cross, degreesToRadians, dot, getX, getY, getZ, infinity, lengthSquared, randomFloat, unit, vecLength, (.*), (.-), (/.))

data Sphere = Sphere
    { center :: Ray
    , radius :: Float
    , sphereMaterial :: SomeMaterial
    }

makeSphere :: Point3 -> Float -> SomeMaterial -> Sphere
makeSphere c r mat =
    Sphere
        { center = makeRay c (Vector3 0 0 0)
        , radius = r
        , sphereMaterial = mat
        }

makeAnimatedSphere :: Point3 -> Point3 -> Float -> SomeMaterial -> Sphere
makeAnimatedSphere a b r mat =
    Sphere
        { center = makeRay a (b - a)
        , radius = r
        , sphereMaterial = mat
        }

getSphereUV :: Point3 -> TextureCoord
getSphereUV p =
    let theta = acos (-(getY p))
        phi = (atan2 (-(getZ p)) (getX p)) + pi
        u = phi / (2 * pi)
        v = theta / pi
     in (u, v)

instance Hittable Sphere where
    hit :: Sphere -> RandomGenerator -> Ray -> Interval -> IO (Maybe Hit)
    hit sphere _ ray interval
        | discriminant < 0 = pure Nothing
        | otherwise =
            pure
                ( findHit firstRoot
                    <|> findHit secondRoot
                )
      where
        currentCenter = at (center sphere) (time ray)
        oc = currentCenter - origin ray
        directionLengthSquared = lengthSquared (direction ray)
        halfB = dot (direction ray) oc
        c = lengthSquared oc - radius sphere * radius sphere

        discriminant =
            halfB * halfB - directionLengthSquared * c

        sqrtDiscriminant = sqrt discriminant

        firstRoot =
            (halfB - sqrtDiscriminant) / directionLengthSquared

        secondRoot =
            (halfB + sqrtDiscriminant) / directionLengthSquared

        findHit root
            | not (contains interval root) = Nothing
            | otherwise =
                let hitPoint = at ray root
                    outwardNormal =
                        (hitPoint - currentCenter) /. radius sphere
                    sphereUV = getSphereUV outwardNormal

                    initialHit =
                        makeHit
                            hitPoint
                            outwardNormal
                            root
                            False
                            (sphereMaterial sphere)
                            sphereUV
                 in Just (setFaceNormal initialHit ray outwardNormal)

    boundingBox :: Sphere -> AABB
    boundingBox obj
        | direction (center obj) == (Vector3 0 0 0) = makeStaticAABB
        | otherwise = makeAnimatedAABB
      where
        makeStaticAABB :: AABB
        makeStaticAABB =
            let staticCenter = origin (center obj)
                rad = radius obj
                rvec = (Vector3 rad rad rad)
             in aabbFromPoints (staticCenter - rvec) (staticCenter + rvec)

        makeAnimatedAABB :: AABB
        makeAnimatedAABB =
            let rad = radius obj
                rvec = (Vector3 rad rad rad)
                cent = center obj
                box1 = aabbFromPoints ((at cent 0) - rvec) ((at cent 0) + rvec)
                box2 = aabbFromPoints ((at cent 1) - rvec) ((at cent 1) + rvec)
             in aabbFromAABBs box1 box2

data Quad = Quad
    { quadOrigin :: Point3
    , quadU :: Vector3
    , quadV :: Vector3
    , quadMaterial :: SomeMaterial
    }

instance Hittable Quad where
    hit :: Quad -> RandomGenerator -> Ray -> Interval -> IO (Maybe Hit)
    hit quad _ ray interval =
        let denom = dot normal (direction ray)
            t = (d - dot normal (origin ray)) / denom
            intersection = at ray t

            initialHit = makeHit intersection normal t False (quadMaterial quad) (0, 0)

            isParallel = abs denom < 1e-8
            notContained = not (contains interval t)
         in if isParallel || notContained
                then
                    pure Nothing
                else
                    pure (isInterior intersection (setFaceNormal initialHit ray normal))
      where
        normal :: Vector3
        normal =
            let n = cross (quadU quad) (quadV quad)
             in unit n

        d :: Float
        d =
            dot normal (quadOrigin quad)

        w :: Vector3
        w =
            let n = cross (quadU quad) (quadV quad)
             in n /. dot n n

        getAlphaBeta :: Vector3 -> (Float, Float)
        getAlphaBeta intersection =
            let planarHitPtVector = intersection - (quadOrigin quad)
                alpha = dot w (cross planarHitPtVector (quadV quad))
                beta = dot w (cross (quadU quad) planarHitPtVector)
             in (alpha, beta)

        isInterior :: Vector3 -> Hit -> Maybe Hit
        isInterior intersection hitted =
            let unitInterval = (0, 1) :: Interval
                (a, b) = getAlphaBeta intersection
                missingA = not (contains unitInterval a)
                missingB = not (contains unitInterval b)
             in if missingA || missingB
                    then Nothing
                    else
                        Just
                            ( hitted
                                { info =
                                    (info hitted)
                                        { uv = (a, b)
                                        }
                                }
                            )

    boundingBox :: Quad -> AABB
    boundingBox quad =
        let diagonal1 = aabbFromPoints (quadOrigin quad) (quadOrigin quad + quadU quad + quadV quad)
            diagonal2 = aabbFromPoints (quadOrigin quad + quadU quad) (quadOrigin quad + quadV quad)
         in aabbFromAABBs diagonal1 diagonal2

    pdfObjectValue :: Quad -> RandomGenerator -> Point3 -> Vector3 -> IO Float
    pdfObjectValue obj gen org dir = do
        let interval = (0.001, infinity) :: Interval
            pdfRay =
                Ray
                    { origin = org
                    , direction = dir
                    , time = 0
                    }
        hitSelf <- hit obj gen pdfRay interval
        case hitSelf of
            Nothing -> pure 0
            Just h ->
                let sqrtDist = hitT h * hitT h * lengthSquared dir
                    cost = abs ((dot dir (hitNormal h)) / vecLength dir)
                 in pure (sqrtDist / (cost * area))
      where
        area :: Float
        area =
            let n = cross (quadU obj) (quadV obj)
             in vecLength n

    randomPdf :: Quad -> RandomGenerator -> Point3 -> IO Vector3
    randomPdf obj gen org = do
        randomU <- randomFloat gen
        randomV <- randomFloat gen
        let randP = (quadOrigin obj) + (randomU .* (quadU obj)) + (randomV .* (quadV obj))
        pure (randP - org)

makeQuad :: Point3 -> Vector3 -> Vector3 -> SomeMaterial -> Quad
makeQuad org u v mat =
    Quad
        { quadOrigin = org
        , quadU = u
        , quadV = v
        , quadMaterial = mat
        }

makeBox :: Point3 -> Point3 -> SomeMaterial -> SomeHittable
makeBox a b mat =
    let boxMin = pickVec min
        boxMax = pickVec max

        dx = Vector3 ((getX boxMax) - (getX boxMin)) 0 0
        dy = Vector3 0 ((getY boxMax) - (getY boxMin)) 0
        dz = Vector3 0 0 ((getZ boxMax) - (getZ boxMin))
     in packHittables
            ( map
                (\x -> SomeHittable x)
                [ makeSide boxMin boxMin boxMax dx dy
                , makeSide boxMax boxMin boxMax (-dz) dy
                , makeSide boxMax boxMin boxMin (-dx) dy
                , makeSide boxMin boxMin boxMin dz dy
                , makeSide boxMin boxMax boxMax dx (-dz)
                , makeSide boxMin boxMin boxMin dx dz
                ]
            )
  where
    pickVec :: (Float -> Float -> Float) -> Vector3
    pickVec f =
        Vector3 (f (getX a) (getX b)) (f (getY a) (getY b)) (f (getZ a) (getZ b))

    makeSide :: Vector3 -> Vector3 -> Vector3 -> Vector3 -> Vector3 -> Quad
    makeSide a' b' c' d d' =
        makeQuad (Vector3 (getX a') (getY b') (getZ c')) d d' mat

data Translation
    = Translation
    { translatedObject :: SomeHittable
    , translationOffset :: Vector3
    }

instance Hittable Translation where
    hit :: Translation -> RandomGenerator -> Ray -> Interval -> IO (Maybe Hit)
    hit trans gen r interval = do
        let offset = r{origin = (origin r) - (translationOffset trans)}
        hitResult <-
            hit
                (translatedObject trans)
                gen
                offset
                interval
        case hitResult of
            Nothing -> pure Nothing
            Just h ->
                pure
                    ( Just
                        ( h
                            { info =
                                (info h)
                                    { p = (p (info h)) + (translationOffset trans)
                                    }
                            }
                        )
                    )

    boundingBox :: Translation -> AABB
    boundingBox trans =
        (boundingBox (translatedObject trans)) +. (translationOffset trans)

translateBy :: Vector3 -> SomeHittable -> SomeHittable
translateBy offset obj =
    SomeHittable
        ( Translation
            { translatedObject = obj
            , translationOffset = offset
            }
        )

data RotateY = RotateY
    { rotatedObject :: SomeHittable
    , sinTheta :: Float
    , cosTheta :: Float
    }

instance Hittable RotateY where
    hit :: RotateY -> RandomGenerator -> Ray -> Interval -> IO (Maybe Hit)
    hit rotation gen r interval = do
        let rotatedRay =
                r
                    { origin = org
                    , direction = dir
                    }
        result <- hit (rotatedObject rotation) gen rotatedRay interval
        case result of
            Nothing -> pure Nothing
            Just h ->
                pure
                    ( Just
                        ( h
                            { info =
                                (info h)
                                    { p = transformToWorldSpace hitP h
                                    , normal = transformToWorldSpace hitNormal h
                                    }
                            }
                        )
                    )
      where
        matrixWithRayProperty :: (Ray -> Vector3) -> Point3
        matrixWithRayProperty f =
            let cost = (cosTheta rotation)
                sint = (sinTheta rotation)
                x = (cost * (getX (f r))) - (sint * (getZ (f r)))
                y = (getY (f r))
                z = (sint * (getX (f r))) + (cost * (getZ (f r)))
             in Vector3 x y z

        org :: Point3
        org = matrixWithRayProperty origin

        dir :: Vector3
        dir = matrixWithRayProperty direction

        transformToWorldSpace :: (Hit -> Vector3) -> Hit -> Vector3
        transformToWorldSpace f h =
            let cost = (cosTheta rotation)
                sint = (sinTheta rotation)
                x = (cost * (getX (f h))) + (sint * (getZ (f h)))
                y = (getY (f h))
                z = (-sint * (getX (f h))) + (cost * (getZ (f h)))
             in Vector3 x y z

    boundingBox :: RotateY -> AABB
    boundingBox rotation =
        let originalBox = boundingBox (rotatedObject rotation)

            initialMin = Vector3 infinity infinity infinity
            initialMax = Vector3 (-infinity) (-infinity) (-infinity)

            (finalMin, finalMax) =
                foldl'
                    (\acc (i, j, k) -> getMaxMinCorner i j k originalBox acc)
                    (initialMin, initialMax)
                    cornerIndices
         in aabbFromPoints finalMin finalMax
      where
        filterVector :: (Float -> Float -> Float) -> Vector3 -> Vector3 -> Vector3
        filterVector f a b =
            let x = f (getX a) (getX b)
                y = f (getY a) (getY b)
                z = f (getZ a) (getZ b)
             in Vector3 x y z

        transformXZ :: (Float, Float) -> (Float, Float)
        transformXZ (x, z) =
            let cost = (cosTheta rotation)
                sint = (sinTheta rotation)
             in (cost * x + sint * z, -sint * x + cost * z)

        getMaxMinCorner :: Int -> Int -> Int -> AABB -> (Vector3, Vector3) -> (Vector3, Vector3)
        getMaxMinCorner i j k aabb (currentMin, currentMax) =
            let x = fromIntegral i * snd (axisX aabb) + (1 - fromIntegral i) * fst (axisX aabb)
                y = fromIntegral j * snd (axisY aabb) + (1 - fromIntegral j) * fst (axisY aabb)
                z = fromIntegral k * snd (axisZ aabb) + (1 - fromIntegral k) * fst (axisZ aabb)

                (newx, newz) = transformXZ (x, z)

                tester = Vector3 newx y newz
             in (filterVector min currentMin tester, filterVector max currentMax tester)

        cornerIndices :: [(Int, Int, Int)]
        cornerIndices = [(i, j, k) | i <- [0, 1], j <- [0, 1], k <- [0, 1]]

rotateBy :: Float -> SomeHittable -> SomeHittable
rotateBy angle obj =
    let radians = degreesToRadians angle
        sint = sin radians
        cost = cos radians
     in SomeHittable
            ( RotateY
                { rotatedObject = obj
                , cosTheta = cost
                , sinTheta = sint
                }
            )
