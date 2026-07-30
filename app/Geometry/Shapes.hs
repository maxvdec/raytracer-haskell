{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Geometry.Shapes where

import Control.Applicative
import GHC.Float (roundFloat)
import Geometry.Hit (Hit (..), Hittable (hit), makeHit, setFaceNormal)
import Geometry.Ray (Ray (direction, origin, time), at, makeRay)
import Materials (SomeMaterial)
import Math (Interval, Point3, Vector3 (Vector3), contains, dot, lengthSquared, (.*), (.-), (/.))

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
