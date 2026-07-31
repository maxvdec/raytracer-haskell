{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Geometry.Shapes where

import Control.Applicative
import Control.Monad (when)
import GHC.Float (roundFloat)
import Geometry.AABB (AABB, aabbFromAABBs, aabbFromPoints)
import Geometry.Hit (Hit (..), Hittable (boundingBox, hit), SomeHittable (SomeHittable), makeHit, setFaceNormal)
import Geometry.HitInfo (HitInfo (uv))
import Geometry.Ray (Ray (direction, origin, time), at, makeRay)
import Graphics.Materials (SomeMaterial (SomeMaterial))
import Math (Interval, Point3, TextureCoord, Vector3 (Vector3), contains, cross, dot, getX, getY, getZ, lengthSquared, unit, (.*), (.-), (/.))

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
    hit :: Sphere -> Ray -> Interval -> Maybe Hit
    hit sphere ray interval
        | discriminant < 0 = Nothing
        | otherwise =
            findHit firstRoot
                <|> findHit secondRoot
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
    hit :: Quad -> Ray -> Interval -> Maybe Hit
    hit quad ray interval =
        let denom = dot normal (direction ray)
            t = (d - dot normal (origin ray)) / denom
            intersection = at ray t

            initialHit = makeHit intersection normal t False (quadMaterial quad) (0, 0)

            isParallel = abs denom < 1e-8
            notContained = not (contains interval t)
         in if isParallel || notContained
                then
                    Nothing
                else
                    isInterior intersection (setFaceNormal initialHit ray normal)
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

makeQuad :: Point3 -> Vector3 -> Vector3 -> SomeMaterial -> Quad
makeQuad org u v mat =
    Quad
        { quadOrigin = org
        , quadU = u
        , quadV = v
        , quadMaterial = mat
        }

makeBox :: Point3 -> Point3 -> SomeMaterial -> [SomeHittable]
makeBox a b mat =
    let boxMin = pickVec min
        boxMax = pickVec max

        dx = Vector3 ((getX boxMax) - (getX boxMin)) 0 0
        dy = Vector3 0 ((getY boxMax) - (getY boxMin)) 0
        dz = Vector3 0 0 ((getZ boxMax) - (getZ boxMin))
     in map
            (\x -> SomeHittable x)
            [ makeSide boxMin boxMin boxMax dx dy
            , makeSide boxMax boxMin boxMax (-dz) dy
            , makeSide boxMax boxMin boxMin (-dx) dy
            , makeSide boxMin boxMin boxMin dz dy
            , makeSide boxMin boxMax boxMax dx (-dz)
            , makeSide boxMin boxMin boxMin dx dz
            ]
  where
    pickVec :: (Float -> Float -> Float) -> Vector3
    pickVec f =
        Vector3 (f (getX a) (getX b)) (f (getY a) (getY b)) (f (getZ a) (getZ b))

    makeSide :: Vector3 -> Vector3 -> Vector3 -> Vector3 -> Vector3 -> Quad
    makeSide a' b' c' d d' =
        makeQuad (Vector3 (getX a') (getY b') (getZ c')) d d' mat
