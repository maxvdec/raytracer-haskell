{-# LANGUAGE InstanceSigs #-}

module Geometry.BVH where

import Data.List (sortBy)
import Geometry.AABB (AABB (AABB), aabbFromAABBs, getAxisFromAABB, getLongestAxisFromAABB, hitAABB)
import Geometry.Hit (Hit (info), Hittable (boundingBox, hit, pdfObjectValue, randomPdf), SomeHittable (SomeHittable))
import Geometry.HitInfo (HitInfo (t))
import Geometry.Ray (Ray)
import Math (Interval, Point3, RandomGenerator, Vector3, randomFloat, randomInt)

data BVH
    = Leaf !AABB !SomeHittable
    | Branch !AABB !Int !BVH !BVH

instance Hittable BVH where
    hit :: BVH -> RandomGenerator -> Ray -> Interval -> IO (Maybe Hit)
    hit bvh gen r rayT = hitNode bvh rayT
      where
        hitNode :: BVH -> Interval -> IO (Maybe Hit)
        hitNode node interval =
            case hitAABB (boundingBox node) r interval of
                Nothing -> pure Nothing
                Just _ -> hitKnownNode node interval

        hitKnownNode :: BVH -> Interval -> IO (Maybe Hit)
        hitKnownNode (Leaf _ obj) interval = hit obj gen r interval
        hitKnownNode (Branch _ _ node1 node2) interval = hitChildren node1 node2 interval

        hitChildren :: BVH -> BVH -> Interval -> IO (Maybe Hit)
        hitChildren node1 node2 interval =
            case (hitAABB (boundingBox node1) r interval, hitAABB (boundingBox node2) r interval) of
                (Nothing, Nothing) -> pure Nothing
                (Just _, Nothing) -> hitKnownNode node1 interval
                (Nothing, Just _) -> hitKnownNode node2 interval
                (Just interval1, Just interval2) ->
                    if fst interval1 <= fst interval2
                        then hitInOrder node1 node2 interval
                        else hitInOrder node2 node1 interval

        hitInOrder :: BVH -> BVH -> Interval -> IO (Maybe Hit)
        hitInOrder firstNode secondNode interval = do
            firstResult <- hitKnownNode firstNode interval
            let closest = maybe (snd interval) (t . info) firstResult
            secondResult <- hitNode secondNode (fst interval, closest)
            pure $
                case secondResult of
                    Just result -> Just result
                    Nothing -> firstResult

    boundingBox :: BVH -> AABB
    boundingBox (Leaf aabb _) = aabb
    boundingBox (Branch aabb _ _ _) = aabb

    pdfObjectValue ::
        BVH ->
        RandomGenerator ->
        Point3 ->
        Vector3 ->
        IO Float
    pdfObjectValue (Leaf _ obj) gen org dir =
        pdfObjectValue obj gen org dir
    pdfObjectValue (Branch _ _ left right) gen org dir = do
        let leftCount = bvhObjectCount left
            rightCount = bvhObjectCount right
            totalCount = leftCount + rightCount

            leftWeight =
                fromIntegral leftCount / fromIntegral totalCount

            rightWeight =
                fromIntegral rightCount / fromIntegral totalCount

        leftPdf <-
            pdfObjectValue left gen org dir

        rightPdf <-
            pdfObjectValue right gen org dir

        pure $
            leftWeight * leftPdf
                + rightWeight * rightPdf

    randomPdf ::
        BVH ->
        RandomGenerator ->
        Point3 ->
        IO Vector3
    randomPdf (Leaf _ obj) gen org =
        randomPdf obj gen org
    randomPdf (Branch _ _ left right) gen org = do
        let leftCount = bvhObjectCount left
            rightCount = bvhObjectCount right
            totalCount = leftCount + rightCount

            leftProbability =
                fromIntegral leftCount / fromIntegral totalCount

        choice <- randomFloat gen

        if choice < leftProbability
            then randomPdf left gen org
            else randomPdf right gen org

createBVHTree :: [SomeHittable] -> BVH
createBVHTree [] = error "Cannot construct BVH from empty list"
createBVHTree [obj] = Leaf (boundingBox obj) obj
createBVHTree [obj1, obj2] =
    Branch
        (aabbFromAABBs (boundingBox obj1) (boundingBox obj2))
        2
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
            (bvhObjectCount leftBVH + bvhObjectCount rightBVH)
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

bvhObjectCount :: BVH -> Int
bvhObjectCount (Leaf _ _) = 1
bvhObjectCount (Branch _ count _ _) = count
