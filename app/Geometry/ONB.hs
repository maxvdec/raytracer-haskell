module Geometry.ONB where

import Math (Vector3 (Vector3), cross, getX, getY, getZ, unit, (.*))

data ONB = ONB
    { onbU :: !Vector3
    , onbV :: !Vector3
    , onbW :: !Vector3
    }

makeONB :: Vector3 -> ONB
makeONB n =
    let w = unit n
        param = abs (getX w)
        a = if param > 0.9 then Vector3 0 1 0 else Vector3 1 0 0
        v = unit (cross w a)
        u = cross w v
     in ONB
            { onbU = u
            , onbV = v
            , onbW = w
            }

emptyONB :: ONB
emptyONB =
    ONB
        { onbU = 0
        , onbV = 0
        , onbW = 0
        }

transformVectorBasedOnONB :: ONB -> Vector3 -> Vector3
transformVectorBasedOnONB onb v =
    let x = (getX v) .* (onbU onb)
        y = (getY v) .* (onbV onb)
        z = (getZ v) .* (onbW onb)
     in x + y + z
