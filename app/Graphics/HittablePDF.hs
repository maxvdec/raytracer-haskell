module Graphics.HittablePDF where

import Control.Monad (foldM)
import GHC.Num.Integer (integerFromInt)
import Geometry.Hit (Hittable (pdfObjectValue, randomPdf), SomeHittable)
import Graphics.PDF (PDF (HittablePDF))
import Math (Point3, RandomGenerator, Vector3, randomInt)

makeHittablePDF :: SomeHittable -> Point3 -> PDF
makeHittablePDF object origin =
    HittablePDF
        (\generator direction -> pdfObjectValue object generator origin direction)
        (\generator -> randomPdf object generator origin)

pdfValueForMultipleObjects ::
    [SomeHittable] ->
    RandomGenerator ->
    Point3 ->
    Vector3 ->
    IO Float
pdfValueForMultipleObjects [] _ _ _ =
    pure 0
pdfValueForMultipleObjects objs gen org dir = do
    let weight =
            1 / fromIntegral (length objs)

    foldM
        ( \total obj -> do
            value <- pdfObjectValue obj gen org dir
            pure $! total + weight * value
        )
        0
        objs

pdfRandomForMultipleObjects :: [SomeHittable] -> RandomGenerator -> Point3 -> IO Vector3
pdfRandomForMultipleObjects objs gen org = do
    let size = integerFromInt (length objs)
    randomIndex <- randomInt gen (0, size - 1)
    let obj = objs !! (fromIntegral randomIndex)
    randomPdf obj gen org
