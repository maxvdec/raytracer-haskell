module Geometry.Ray where

import Math (Point3, Vector3, (.*))

data Ray = Ray
    { origin :: Point3
    , direction :: Vector3
    }
    deriving (Show, Eq)

at :: Ray -> Float -> Point3
at ray t =
    origin ray + t .* direction ray