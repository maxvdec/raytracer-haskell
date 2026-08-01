module Geometry.ScatterRecord where

import Geometry.Ray (Ray)
import Graphics.PDF (PDF)
import Math (Color)

data ScatterRecord = ScatterRecord
    { attenuation :: Color
    , scatPrimitive :: Either PDF Ray
    }

makeScatterRecord :: Color -> Either PDF Ray -> ScatterRecord
makeScatterRecord col scat =
    ScatterRecord
        { attenuation = col
        , scatPrimitive = scat
        }
