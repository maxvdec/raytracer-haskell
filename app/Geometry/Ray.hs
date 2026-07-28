module Geometry.Ray where

import Geometry.Scene (Camera (cameraCenter), calculateDeltaUV, calculateTopLeftPos)
import Math (Point3, Vector3 (Vector3), (*.), (.*))

data Ray = Ray
    { origin :: Point3
    , direction :: Vector3
    }
    deriving (Show, Eq)

at :: Ray -> Float -> Point3
at ray t =
    (origin ray) + (t .* (direction ray))

makeRayForCoordinate :: Camera -> Integer -> Integer -> Ray
makeRayForCoordinate cam x y =
    let (deltaU, deltaV) = calculateDeltaUV cam
        pixel0Pos = calculateTopLeftPos cam
        camCenter = cameraCenter cam
        pixelCenter = pixel0Pos + ((fromInteger x) .* deltaU) + ((fromInteger y) .* deltaV)
        rayDir = pixelCenter - camCenter
     in Ray
            { origin = camCenter
            , direction = rayDir
            }