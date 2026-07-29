{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Geometry.Shapes where

import Control.Applicative
import Geometry.Ray (Ray (direction, origin), at)
import Math (Interval, Point3, Vector3, contains, dot, lengthSquared, (.*), (.-), (/.))

data Hit = Hit
    { p :: Point3
    , normal :: Vector3
    , t :: Float
    , isFront :: Bool
    }

setFaceNormal :: Hit -> Ray -> Vector3 -> Hit
setFaceNormal h r outward =
    let frontFace = dot (direction r) outward < 0
        normalResult = if frontFace then outward else -outward
     in Hit
            { p = p h
            , normal = normalResult
            , t = t h
            , isFront = frontFace
            }

class Hittable a where
    hit :: a -> Ray -> Interval -> Maybe Hit

data SomeHittable = forall a. (Hittable a) => SomeHittable a

instance Hittable SomeHittable where
    hit :: SomeHittable -> Ray -> Interval -> Maybe Hit
    hit (SomeHittable obj) = hit obj

data Sphere = Sphere
    { center :: Point3
    , radius :: Float
    }
    deriving (Eq)

makeSphere :: Point3 -> Float -> Sphere
makeSphere c r =
    Sphere
        { center = c
        , radius = r
        }

instance Hittable Sphere where
    hit :: Sphere -> Ray -> Interval -> Maybe Hit
    hit sphere ray interval
        | discriminant < 0 = Nothing
        | otherwise =
            findHit firstRoot
                <|> findHit secondRoot
      where
        oc = center sphere - origin ray
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
                        (hitPoint - center sphere) /. radius sphere

                    initialHit =
                        Hit
                            { p = hitPoint
                            , normal = outwardNormal
                            , t = root
                            , isFront = False
                            }
                 in Just (setFaceNormal initialHit ray outwardNormal)
