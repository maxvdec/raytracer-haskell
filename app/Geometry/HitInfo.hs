module Geometry.HitInfo where

import Math (Point3, TextureCoord, Vector3)

data HitInfo = HitInfo
    { p :: Point3
    , normal :: Vector3
    , t :: Float
    , uv :: TextureCoord
    , isFront :: Bool
    }
