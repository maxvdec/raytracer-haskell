{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Geometry.Hit where

import Geometry.AABB (AABB)
import Geometry.HitInfo (HitInfo (HitInfo, isFront, normal, p, t, uv))
import Geometry.Ray (Ray (direction))
import Graphics.Materials (SomeMaterial)
import Math (Interval, Point3, RandomGenerator, TextureCoord, Vector3 (Vector3), dot)

data Hit = Hit
    { info :: !HitInfo
    , material :: !SomeMaterial
    }

hitP :: Hit -> Point3
hitP = p . info

hitTexCoords :: Hit -> TextureCoord
hitTexCoords = uv . info

hitNormal :: Hit -> Vector3
hitNormal = normal . info

hitT :: Hit -> Float
hitT = t . info

isHitFront :: Hit -> Bool
isHitFront = isFront . info

makeHit :: Point3 -> Vector3 -> Float -> Bool -> SomeMaterial -> TextureCoord -> Hit
makeHit point normalVec tVal frontFace mat coords =
    Hit
        { info = HitInfo{p = point, normal = normalVec, t = tVal, uv = coords, isFront = frontFace}
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
            (hitTexCoords h)

class Hittable a where
    hit :: a -> RandomGenerator -> Ray -> Interval -> IO (Maybe Hit)
    boundingBox :: a -> AABB
    pdfObjectValue :: a -> RandomGenerator -> Point3 -> Vector3 -> IO Float
    pdfObjectValue _ _ _ _ = pure 0
    randomPdf :: a -> RandomGenerator -> Vector3 -> IO Vector3
    randomPdf _ _ _ = pure (Vector3 1 0 0)

data SomeHittable = forall a. (Hittable a) => SomeHittable !a

instance Hittable SomeHittable where
    hit :: SomeHittable -> RandomGenerator -> Ray -> Interval -> IO (Maybe Hit)
    hit (SomeHittable obj) = hit obj

    boundingBox :: SomeHittable -> AABB
    boundingBox (SomeHittable obj) = boundingBox obj

    pdfObjectValue (SomeHittable obj) = pdfObjectValue obj
    randomPdf (SomeHittable obj) = randomPdf obj
