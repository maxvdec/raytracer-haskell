{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Geometry.Shapes where

import Control.Applicative
import GHC.Float (roundFloat)
import Geometry.Hit (Hit (..), Hittable (hit, boundingBox), makeHit, setFaceNormal)
import Geometry.Ray (Ray (direction, origin, time), at, makeRay)
import Materials (SomeMaterial)
import Math (Interval, Point3, Vector3 (Vector3), contains, dot, lengthSquared, (.*), (.-), (/.))
import Geometry.AABB (AABB, aabbFromPoints, aabbFromAABBs)

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
    Sphere {
        center = makeRay a (b - a)
        , radius = r
        , sphereMaterial = mat
    }

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

                    initialHit =
                        makeHit
                            hitPoint
                            outwardNormal
                            root
                            False
                            (sphereMaterial sphere)
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
                    rvec = (Vector3 rad rad rad) in
                    aabbFromPoints (staticCenter - rvec) (staticCenter + rvec)

            makeAnimatedAABB :: AABB
            makeAnimatedAABB =
                let rad = radius obj
                    rvec = (Vector3 rad rad rad) 
                    cent = center obj
                    box1 = aabbFromPoints ((at cent 0) - rvec) ((at cent 0) + rvec)
                    box2 = aabbFromPoints ((at cent 1) - rvec) ((at cent 1) + rvec)
                in aabbFromAABBs box1 box2
