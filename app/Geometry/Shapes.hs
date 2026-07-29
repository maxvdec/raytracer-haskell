module Geometry.Shapes where

import Geometry.Ray (Ray (direction, origin))
import Math (Point3, dot, lengthSquared, (.*))

hitSphere :: Point3 -> Float -> Ray -> Float
hitSphere center radius r =
    let oc = center - (origin r)
        a = lengthSquared (direction r)
        h = dot (direction r) oc
        c = (lengthSquared oc) - radius * radius
        disc = h * h - c * a
     in if disc < 0
            then
                (-1)
            else
                (h - (sqrt disc)) / a
