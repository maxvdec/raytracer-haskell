{-# LANGUAGE InstanceSigs #-}
module Geometry.BVH where
import Geometry.AABB (AABB, hitAABB)
import Geometry.Hit (SomeHittable, Hittable (hit, boundingBox), Hit (info))
import Geometry.Ray (Ray)
import Math (Interval)
import Geometry.HitInfo (HitInfo(t))
import Control.Applicative

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
