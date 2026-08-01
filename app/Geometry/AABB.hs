{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Geometry.AABB where

import Geometry.Ray (Ray (direction, origin))
import Math (Addable ((+.), (.+)), Interval, Point3, Vector3 (Vector3), enclose, expand, getX, getY, getZ, sizeOfInterval, (|>))

data AABB = AABB
    { axisX :: !Interval
    , axisY :: !Interval
    , axisZ :: !Interval
    }

aabbFromPoints :: Point3 -> Point3 -> AABB
aabbFromPoints a b =
    let x = makeAxis a b getX
        y = makeAxis a b getY
        z = makeAxis a b getZ
     in ( AABB
            { axisX = x
            , axisY = y
            , axisZ = z
            }
        )
            |> padAABBToMinimums
  where
    makeAxis :: Point3 -> Point3 -> (Point3 -> Float) -> Interval
    makeAxis a' b' get =
        if (get a') <= (get b') then (get a', get b') else (get b', get a')

aabbFromAABBs :: AABB -> AABB -> AABB
aabbFromAABBs boxA boxB =
    let x = enclose (axisX boxA) (axisX boxB)
        y = enclose (axisY boxA) (axisY boxB)
        z = enclose (axisZ boxA) (axisZ boxB)
     in ( AABB
            { axisX = x
            , axisY = y
            , axisZ = z
            }
        )
            |> padAABBToMinimums

getAxisFromAABB :: AABB -> Integer -> Interval
getAxisFromAABB aabb i
    | i == 1 = axisY aabb
    | i == 2 = axisZ aabb
    | otherwise = axisX aabb

getLongestAxisFromAABB :: AABB -> Integer
getLongestAxisFromAABB aabb =
    let lengthX = sizeOfInterval (axisX aabb)
        lengthY = sizeOfInterval (axisY aabb)
        lengthZ = sizeOfInterval (axisZ aabb)
     in if lengthX > lengthY
            then
                if lengthX > lengthZ then 0 else 2
            else
                if lengthY > lengthZ then 1 else 2

hitAABB :: AABB -> Ray -> Interval -> Maybe Interval
hitAABB aabb r rayT =
    hitAxis (axisX aabb) ox dx rayT
        >>= hitAxis (axisY aabb) oy dy
        >>= hitAxis (axisZ aabb) oz dz
  where
    Vector3 ox oy oz = origin r
    Vector3 dx dy dz = direction r

    hitAxis :: Interval -> Float -> Float -> Interval -> Maybe Interval
    hitAxis axisInterval rayOrigin rayDirection inT =
        let adinv = 1.0 / rayDirection
            t0 = (fst axisInterval - rayOrigin) * adinv
            t1 = (snd axisInterval - rayOrigin) * adinv
            nearT = min t0 t1
            farT = max t0 t1
            rayMin = max nearT (fst inT)
            rayMax = min farT (snd inT)
         in if rayMax < rayMin
                then Nothing
                else Just (rayMin, rayMax)

padAABBToMinimums :: AABB -> AABB
padAABBToMinimums aabb =
    let delta = 0.0001
        newX = if sizeOfInterval (axisX aabb) < delta then expand (axisX aabb) delta else axisX aabb
        newY = if sizeOfInterval (axisY aabb) < delta then expand (axisY aabb) delta else axisY aabb
        newZ = if sizeOfInterval (axisZ aabb) < delta then expand (axisZ aabb) delta else axisZ aabb
     in aabb
            { axisX = newX
            , axisY = newY
            , axisZ = newZ
            }

instance Addable AABB Vector3 where
    (+.) :: AABB -> Vector3 -> AABB
    aabb +. vec =
        AABB
            { axisX = (axisX aabb) +. (getX vec)
            , axisY = (axisY aabb) +. (getY vec)
            , axisZ = (axisZ aabb) +. (getZ vec)
            }

    (.+) :: Vector3 -> AABB -> AABB
    vec .+ aabb = aabb +. vec
