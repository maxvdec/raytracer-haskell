module Graphics.PDF where

import Geometry.ONB (ONB (onbW), emptyONB, makeONB, transformVectorBasedOnONB)
import Math (RandomGenerator, Vector3, dot, randomCosineDirection, randomFloat, randomUnitVector, unit)

data PDF = SpherePDF | CosinePDF Vector3 | HittablePDF (RandomGenerator -> Vector3 -> IO Float) (RandomGenerator -> IO Vector3) | MixturePDF PDF PDF

onbFromPdf :: PDF -> ONB
onbFromPdf (SpherePDF) = emptyONB
onbFromPdf (HittablePDF _ _) = emptyONB
onbFromPdf (CosinePDF w) = makeONB w
onbFromPdf (MixturePDF _ _) = emptyONB

getPDFValue :: PDF -> RandomGenerator -> Vector3 -> IO Float
getPDFValue (SpherePDF) _ _ = pure (1 / (4 * pi))
getPDFValue cpdf@(CosinePDF _) _ dir =
    let onb = onbFromPdf cpdf
        cost = dot (unit dir) (onbW onb)
     in pure (max 0 (cost / pi))
getPDFValue (HittablePDF value _) gen dir = value gen dir
getPDFValue (MixturePDF p0 p1) gen dir = do
    value0 <- getPDFValue p0 gen dir
    value1 <- getPDFValue p1 gen dir
    pure (0.5 * value0 + 0.5 * value1)

generate :: PDF -> RandomGenerator -> IO Vector3
generate (SpherePDF) gen = do randomUnitVector gen
generate cpdf@(CosinePDF _) gen = do
    let onb = onbFromPdf cpdf
    randDir <- randomCosineDirection gen
    pure (transformVectorBasedOnONB onb randDir)
generate (HittablePDF _ generateDirection) gen = generateDirection gen
generate (MixturePDF p0 p1) gen = do
    value0 <- generate p0 gen
    value1 <- generate p1 gen
    decider <- randomFloat gen
    if decider < 0.5 then pure value0 else pure value1
