module Graphics.PDF where

import Geometry.ONB (ONB (onbW), emptyONB, makeONB, transformVectorBasedOnONB)
import Math (RandomGenerator, Vector3, dot, randomCosineDirection, randomUnitVector, unit)

data PDF = SpherePDF | CosinePDF Vector3

onbFromPdf :: PDF -> ONB
onbFromPdf (SpherePDF) = emptyONB
onbFromPdf (CosinePDF w) = makeONB w

getPDFValue :: PDF -> Vector3 -> Float
getPDFValue (SpherePDF) _ = 1 / (4 * pi)
getPDFValue cpdf@(CosinePDF _) dir =
    let onb = onbFromPdf cpdf
        cost = dot (unit dir) (onbW onb)
     in max 0 (cost / pi)

generate :: PDF -> RandomGenerator -> IO Vector3
generate (SpherePDF) gen = do randomUnitVector gen
generate cpdf@(CosinePDF _) gen = do
    let onb = onbFromPdf cpdf
    randDir <- randomCosineDirection gen
    pure (transformVectorBasedOnONB onb randDir)
