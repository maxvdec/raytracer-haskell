module Geometry.Ray where

import Math (Point3, Vector3, (.*))

data Ray = Ray
    { origin :: Point3
    , direction :: Vector3
    , time :: Float
    }
    deriving (Show, Eq)

at :: Ray -> Float -> Point3
at ray t =
    origin ray + t .* direction ray

makeRay :: Point3 -> Vector3 -> Ray
makeRay orig dir =
    Ray {
        origin = orig
        , direction = dir
        , time = 0
    }
