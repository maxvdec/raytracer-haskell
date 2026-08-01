module Graphics.HittablePDF where

import Geometry.Hit (Hittable (pdfObjectValue, randomPdf), SomeHittable)
import Graphics.PDF (PDF (HittablePDF))
import Math (Point3)

makeHittablePDF :: SomeHittable -> Point3 -> PDF
makeHittablePDF object origin =
    HittablePDF
        (\generator direction -> pdfObjectValue object generator origin direction)
        (\generator -> randomPdf object generator origin)
