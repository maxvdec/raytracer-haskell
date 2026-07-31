{-# LANGUAGE InstanceSigs #-}
module Geometry.BVH where
import Geometry.AABB (AABB, hitAABB, getAxisFromAABB, aabbFromAABBs)
import Geometry.Hit (SomeHittable (SomeHittable), Hittable (hit, boundingBox), Hit (info))
import Geometry.Ray (Ray)
import Math (Interval, RandomGenerator, randomInt)
import Geometry.HitInfo (HitInfo(t))
import Control.Applicative
import Data.List (sort, sortBy)

data BVH
    = Leaf !AABB !SomeHittable
    | Branch !AABB !BVH !BVH

instance Hittable BVH where
    hit :: BVH -> Ray -> Interval -> Maybe Hit
    hit bvh r rayT =
        case bvh of
        Leaf aabb obj -> hitLeaf aabb obj
        Branch aabb node1 node2 -> hitBranch aabb node1 node2

        where
            hitLeaf :: AABB -> SomeHittable -> Maybe Hit
            hitLeaf aabb obj = do
                narrowedInterval <- hitAABB aabb r rayT
                hit obj r narrowedInterval

            hitBranch :: AABB -> BVH -> BVH -> Maybe Hit
            hitBranch aabb node1 node2 = do
                narrowedInterval <- hitAABB aabb r rayT
                let leftResult = hit node1 r narrowedInterval
                    newInterval =
                        case leftResult of
                            Nothing ->
                                narrowedInterval
            
                            Just h ->
                                (fst narrowedInterval, t (info h))
                    rightResult = hit node2 r newInterval
            
                rightResult <|> leftResult

    boundingBox :: BVH -> AABB
    boundingBox (Leaf aabb _) = aabb
    boundingBox (Branch aabb _ _) = aabb

createBVHTree :: RandomGenerator -> [SomeHittable] -> IO BVH
createBVHTree _ [] = error "Cannot construct BVH from empty list"
createBVHTree _ [obj] = pure (Leaf (boundingBox obj) obj)
createBVHTree _ [obj1, obj2] = pure (Branch 
    (aabbFromAABBs (boundingBox obj1) (boundingBox obj2)) 
    (Leaf (boundingBox obj1) obj1) 
    (Leaf (boundingBox obj2) obj2))
createBVHTree gen objects = do
    axis <- randomInt gen (0, 2)
    let comparator = 
            case axis of
            0 -> boxXCompare
            1 -> boxYCompare
            _ -> boxZCompare
        sorted = sortBy comparator objects
        middle = length sorted `div` 2
        (leftObjects, rightObjects) = splitAt middle sorted
    leftBVH <- createBVHTree gen leftObjects
    rightBVH <- createBVHTree gen rightObjects
    pure (Branch 
        (aabbFromAABBs (boundingBox leftBVH) (boundingBox rightBVH)) 
        leftBVH
        rightBVH)
    

compareBox :: SomeHittable -> SomeHittable -> Integer -> Ordering
compareBox a b axisIndex =
    let aAxisInterval = getAxisFromAABB (boundingBox a) axisIndex
        bAxisInterval = getAxisFromAABB (boundingBox b) axisIndex in
        compare (fst aAxisInterval) (fst bAxisInterval) 

boxXCompare :: SomeHittable -> SomeHittable -> Ordering
boxXCompare a b = compareBox a b 0

boxYCompare :: SomeHittable -> SomeHittable -> Ordering 
boxYCompare a b = compareBox a b 1

boxZCompare :: SomeHittable -> SomeHittable -> Ordering 
boxZCompare a b = compareBox a b 2
