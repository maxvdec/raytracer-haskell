module Graphics.Noise where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Control.Monad (forM_)
import Data.Bits (Bits (xor), (.&.))
import Math (Point3, RandomGenerator, Vector3 (Vector3), dot, getX, getY, getZ, randomVectorInRange, (*.))
import System.Random (randomRIO)

data Perlin = Perlin
    { noiseRandvec :: V.Vector Vector3
    , permX :: V.Vector Int
    , permY :: V.Vector Int
    , permZ :: V.Vector Int
    }

shuffleVector :: V.Vector Int -> IO (V.Vector Int)
shuffleVector vec = do
    mutableVector <- V.thaw vec

    let finalIndex = V.length vec - 1

    forM_ [finalIndex, finalIndex - 1 .. 1] $ \index -> do
        target <- randomRIO (0, index)
        MV.swap mutableVector index target

    V.freeze mutableVector

makePermutation :: Int -> IO (V.Vector Int)
makePermutation size =
    shuffleVector (V.generate size id)

noise :: Perlin -> Point3 -> Float
noise perlin p =
    let u' = (getX p) - fromInteger (floor (getX p))
        v' = (getY p) - fromInteger (floor (getY p))
        w' = (getZ p) - fromInteger (floor (getZ p))
        u = u' * u' * (3 - 2 * u')
        v = v' * v' * (3 - 2 * v')
        w = w' * w' * (3 - 2 * w')

        i = floor (getX p) :: Int
        j = floor (getY p) :: Int
        k = floor (getZ p) :: Int

        corners = makeCorners i j k
     in perlinInterpolation corners u v w
  where
    hashCorner :: Int -> Int -> Int -> Int -> Int -> Int -> Corner
    hashCorner i j k di dj dk =
        let elems = V.length (noiseRandvec perlin)
            wrappedX = (i + di) .&. (elems - 1)
            wrappedY = (j + dj) .&. (elems - 1)
            wrappedZ = (k + dk) .&. (elems - 1)
            permutedX = (permX perlin) V.! wrappedX
            permutedY = (permY perlin) V.! wrappedY
            permutedZ = (permZ perlin) V.! wrappedZ

            noiseIndex = permutedX `xor` permutedY `xor` permutedZ
         in (noiseRandvec perlin) V.! noiseIndex

    makeCorners :: Int -> Int -> Int -> [Corner]
    makeCorners i j k =
        [hashCorner i j k di dj dk | di <- [0, 1], dj <- [0, 1], dk <- [0, 1]]

turb :: Perlin -> Point3 -> Int -> Float
turb perlin p depth =
    abs (go depth 1 p 0)
  where
    go :: Int -> Float -> Point3 -> Float -> Float
    go remaining weight currentPoint accum
        | remaining <= 0 = accum
        | otherwise =
            go
                (remaining - 1)
                (weight * 0.5)
                (currentPoint *. 2)
                (accum + weight * noise perlin currentPoint)

makePerlinNoise :: RandomGenerator -> Int -> IO Perlin
makePerlinNoise gen size = do
    randomNumbers <- V.replicateM size (randomVectorInRange gen (-1, 1))
    permutationX <- makePermutation size
    permutationY <- makePermutation size
    permutationZ <- makePermutation size
    pure
        ( Perlin
            { noiseRandvec = randomNumbers
            , permX = permutationX
            , permY = permutationY
            , permZ = permutationZ
            }
        )

type Corner = Vector3

perlinInterpolation :: [Corner] -> Float -> Float -> Float -> Float
perlinInterpolation [] _ _ _ = 0
perlinInterpolation (corner : corners) u v w =
    let
        (u', v', w') = (hermiteSmoothing u, hermiteSmoothing v, hermiteSmoothing w)
        (x, y, z) = getCornerCoordinatesForIndex (7 - (length corners))
        weight = getWeightForCorner x y z u' v' w'
        offset = getOffset x y z
     in
        (weight * dot corner offset) + perlinInterpolation corners u v w
  where
    getWeightForCorner :: Int -> Int -> Int -> Float -> Float -> Float -> Float
    getWeightForCorner x y z u' v' w' =
        let weightX = contrib x u'
            weightY = contrib y v'
            weightZ = contrib z w'
         in (weightX * weightY * weightZ)
      where
        contrib :: Int -> Float -> Float
        contrib n coord =
            if n == 1 then coord else 1 - coord

    getOffset :: Int -> Int -> Int -> Vector3
    getOffset x y z =
        Vector3 (u - (fromIntegral x)) (v - (fromIntegral y)) (w - (fromIntegral z))

    getCornerCoordinatesForIndex :: Int -> (Int, Int, Int)
    getCornerCoordinatesForIndex i =
        (i `div` 4, (i `div` 2) `mod` 2, i `mod` 2)

    hermiteSmoothing :: Float -> Float
    hermiteSmoothing n = n * n * (3 - 2 * n)
