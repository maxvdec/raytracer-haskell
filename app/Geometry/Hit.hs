{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Geometry.Hit where

import Geometry.HitInfo (HitInfo (HitInfo, isFront, normal, p, t))
import Geometry.Ray (Ray (direction))
import Materials (SomeMaterial)
import Math (Interval, Point3, Vector3, dot)

data Hit = Hit
    { info :: HitInfo
    , material :: SomeMaterial
    }

hitP :: Hit -> Point3
hitP = p . info

hitNormal :: Hit -> Vector3
hitNormal = normal . info

hitT :: Hit -> Float
hitT = t . info

isHitFront :: Hit -> Bool
isHitFront = isFront . info

makeHit :: Point3 -> Vector3 -> Float -> Bool -> SomeMaterial -> Hit
makeHit point normalVec tVal frontFace mat =
    Hit
        { info = HitInfo{p = point, normal = normalVec, t = tVal, isFront = frontFace}
        , material = mat
        }

setFaceNormal :: Hit -> Ray -> Vector3 -> Hit
setFaceNormal h r outward =
    let frontFace = dot (direction r) outward < 0
        normalResult = if frontFace then outward else -outward
     in makeHit
            (hitP h)
            normalResult
            (hitT h)
            frontFace
            (material h)

class Hittable a where
    hit :: a -> Ray -> Interval -> Maybe Hit

data SomeHittable = forall a. (Hittable a) => SomeHittable a

instance Hittable SomeHittable where
    hit :: SomeHittable -> Ray -> Interval -> Maybe Hit
    hit (SomeHittable obj) = hit obj
