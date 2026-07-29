module Geometry.Shapes where

import Geometry.Ray (Ray (direction, origin))
import Math (Point3, dot, (.*))

hitSphere :: Point3 -> Float -> Ray -> Float
hitSphere center radius r =
    let oc = center - (origin r)
        a = dot (direction r) (direction r)
        b = -2 * (dot (direction r) oc)
        c = (dot oc oc) - radius * radius
        disc = b * b - 4 * a * c
     in if disc < 0
            then
                (-1)
            else
                ((-b) - (sqrt disc)) / (2.0 * a)
