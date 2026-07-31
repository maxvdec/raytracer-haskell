{-# LANGUAGE InstanceSigs #-}

module Geometry.BVH where

import Control.Applicative
import Data.List (sort, sortBy)
import Geometry.AABB (AABB (AABB), aabbFromAABBs, getAxisFromAABB, getLongestAxisFromAABB, hitAABB)
import Geometry.Hit (Hit (info), Hittable (boundingBox, hit), SomeHittable (SomeHittable))
import Geometry.HitInfo (HitInfo (t))
import Geometry.Ray (Ray)
import Math (Interval, RandomGenerator, randomInt)

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

createBVHTree :: [SomeHittable] -> BVH
createBVHTree [] = error "Cannot construct BVH from empty list"
createBVHTree [obj] = Leaf (boundingBox obj) obj
createBVHTree [obj1, obj2] =
    Branch
        (aabbFromAABBs (boundingBox obj1) (boundingBox obj2))
        (Leaf (boundingBox obj1) obj1)
        (Leaf (boundingBox obj2) obj2)
createBVHTree objects =
    let axis = getLongestAxis
        comparator =
            case axis of
                0 -> boxXCompare
                1 -> boxYCompare
                _ -> boxZCompare
        sorted = sortBy comparator objects
        middle = length sorted `div` 2
        (leftObjects, rightObjects) = splitAt middle sorted
        leftBVH = createBVHTree leftObjects
        rightBVH = createBVHTree rightObjects
     in Branch
            (aabbFromAABBs (boundingBox leftBVH) (boundingBox rightBVH))
            leftBVH
            rightBVH
  where
    getLongestAxis :: Integer
    getLongestAxis = getLongestAxisFromAABB (buildObjectsAABB objects)

    buildObjectsAABB :: [SomeHittable] -> AABB
    buildObjectsAABB [] = error "Cannot build box with no objects"
    buildObjectsAABB [obj] = boundingBox obj
    buildObjectsAABB (obj : os) = aabbFromAABBs (boundingBox obj) (buildObjectsAABB os)

compareBox :: SomeHittable -> SomeHittable -> Integer -> Ordering
compareBox a b axisIndex =
    let aAxisInterval = getAxisFromAABB (boundingBox a) axisIndex
        bAxisInterval = getAxisFromAABB (boundingBox b) axisIndex
     in compare (fst aAxisInterval) (fst bAxisInterval)

boxXCompare :: SomeHittable -> SomeHittable -> Ordering
boxXCompare a b = compareBox a b 0

boxYCompare :: SomeHittable -> SomeHittable -> Ordering
boxYCompare a b = compareBox a b 1

boxZCompare :: SomeHittable -> SomeHittable -> Ordering
boxZCompare a b = compareBox a b 2
