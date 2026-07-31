module Geometry.AABB where
import Math (Interval, Point3, getX, getY, getZ, getAxisFromVec3, surrounds, enclose)
import Geometry.Ray (Ray (origin, direction))
import Control.Monad (foldM)

data AABB = AABB {
    axisX :: Interval
    , axisY :: Interval
    , axisZ :: Interval
}

aabbFromPoints :: Point3 -> Point3 -> AABB
aabbFromPoints a b =
    let x = makeAxis a b getX 
        y = makeAxis a b getY 
        z = makeAxis a b getZ 
    in
    AABB {
        axisX = x
        , axisY = y
        , axisZ = z
    }

    where
        makeAxis :: Point3 -> Point3 -> (Point3 -> Float) -> Interval
        makeAxis a' b' get =
            if (get a') <= (get b') then (get a', get b') else (get b', get a')

aabbFromAABBs :: AABB -> AABB -> AABB
aabbFromAABBs boxA boxB =
    let x = enclose (axisX boxA) (axisX boxB)
        y = enclose (axisY boxA) (axisY boxB)
        z = enclose (axisZ boxA) (axisZ boxB) in
       AABB {
           axisX = x
           , axisY = y
           , axisZ = z
       } 
    

getAxisFromAABB :: AABB -> Integer -> Interval
getAxisFromAABB aabb i 
    | i == 1 = axisY aabb
    | i == 2 = axisZ aabb
    | otherwise = axisX aabb

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
             in if rayMax <= rayMin
                    then Nothing
                    else Just (rayMin, rayMax)
