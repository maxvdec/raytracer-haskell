module Graphics.PDF where

import Geometry.ONB (ONB (onbW), transformVectorBasedOnONB)
import Math (RandomGenerator, Vector3, dot, randomCosineDirection, randomFloat, randomUnitVector, unit)

data PDF = SpherePDF | CosinePDF !ONB | HittablePDF (RandomGenerator -> Vector3 -> IO Float) (RandomGenerator -> IO Vector3) | MixturePDF !PDF !PDF

getPDFValue :: PDF -> RandomGenerator -> Vector3 -> IO Float
getPDFValue (SpherePDF) _ _ = pure (1 / (4 * pi))
getPDFValue (CosinePDF onb) _ dir =
    let cost = dot (unit dir) (onbW onb)
     in pure (max 0 (cost / pi))
getPDFValue (HittablePDF value _) gen dir = value gen dir
getPDFValue (MixturePDF p0 p1) gen dir = do
    value0 <- getPDFValue p0 gen dir
    value1 <- getPDFValue p1 gen dir
    pure (0.5 * value0 + 0.5 * value1)

generate :: PDF -> RandomGenerator -> IO Vector3
generate (SpherePDF) gen = do randomUnitVector gen
generate (CosinePDF onb) gen = do
    randDir <- randomCosineDirection gen
    pure (transformVectorBasedOnONB onb randDir)
generate (HittablePDF _ generateDirection) gen = generateDirection gen
generate (MixturePDF p0 p1) gen = do
    decider <- randomFloat gen
    if decider < 0.5 then generate p0 gen else generate p1 gen
