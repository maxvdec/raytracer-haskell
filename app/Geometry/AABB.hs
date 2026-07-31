module Geometry.AABB where

import Control.Monad (foldM)
import Geometry.Ray (Ray (direction, origin))
import Math (Interval, Point3, enclose, expand, getAxisFromVec3, getX, getY, getZ, sizeOfInterval, surrounds, (|>))

data AABB = AABB
    { axisX :: Interval
    , axisY :: Interval
    , axisZ :: Interval
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
hitAABB aabb r rayT = do
    foldM hitAxis rayT [0, 1, 2]
  where
    hitAxis :: Interval -> Integer -> Maybe Interval
    hitAxis inT i =
        let rayOrigin = origin r
            rayDir = direction r
            axisInterval = getAxisFromAABB aabb i

            adinv = 1.0 / getAxisFromVec3 rayDir i

            t0 =
                (fst axisInterval - getAxisFromVec3 rayOrigin i)
                    * adinv

            t1 =
                (snd axisInterval - getAxisFromVec3 rayOrigin i)
                    * adinv

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
