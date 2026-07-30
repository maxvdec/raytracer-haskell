module Geometry.HitInfo where

import Math (Point3, Vector3)

data HitInfo = HitInfo
    { p :: Point3
    , normal :: Vector3
    , t :: Float
    , isFront :: Bool
    }
