module Graphics.Noise where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Control.Monad (forM_, replicateM)
import Data.Bits (Bits (xor), (.&.))
import Math (Point3, RandomGenerator, getX, getY, getZ, randomFloat)
import System.Random (randomRIO)

data Perlin = Perlin
    { noiseRandfloat :: V.Vector Float
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
    let u = (getX p) - fromInteger (floor (getX p))
        v = (getY p) - fromInteger (floor (getY p))
        w = (getZ p) - fromInteger (floor (getZ p))

        i = floor (getX p) :: Int
        j = floor (getY p) :: Int
        k = floor (getZ p) :: Int

        corners = makeCorners i j k
     in trilinearInterpolation corners u v w
  where
    hashCorner :: Int -> Int -> Int -> Int -> Int -> Int -> Corner
    hashCorner i j k di dj dk =
        let elems = V.length (noiseRandfloat perlin)
            wrappedX = (i + di) .&. (elems - 1)
            wrappedY = (j + dj) .&. (elems - 1)
            wrappedZ = (k + dk) .&. (elems - 1)
            permutedX = (permX perlin) V.! wrappedX
            permutedY = (permY perlin) V.! wrappedY
            permutedZ = (permZ perlin) V.! wrappedZ

            noiseIndex = permutedX `xor` permutedY `xor` permutedZ
         in (noiseRandfloat perlin) V.! noiseIndex

    makeCorners :: Int -> Int -> Int -> [Corner]
    makeCorners i j k =
        [hashCorner i j k di dj dk | di <- [0, 1], dj <- [0, 1], dk <- [0, 1]]

makePerlinNoise :: RandomGenerator -> Int -> IO Perlin
makePerlinNoise gen size = do
    randomNumbers <- V.replicateM size (randomFloat gen)
    permutationX <- makePermutation size
    permutationY <- makePermutation size
    permutationZ <- makePermutation size
    pure
        ( Perlin
            { noiseRandfloat = randomNumbers
            , permX = permutationX
            , permY = permutationY
            , permZ = permutationZ
            }
        )

type Corner = Float

trilinearInterpolation :: [Corner] -> Float -> Float -> Float -> Float
trilinearInterpolation [] _ _ _ = 0
trilinearInterpolation (corner : corners) u v w =
    let (x, y, z) = getCornerCoordinatesForIndex (7 - (length corners))
        weight = getWeightForCorner x y z
     in (weight * corner) + trilinearInterpolation corners u v w
  where
    getWeightForCorner :: Int -> Int -> Int -> Float
    getWeightForCorner x y z =
        let weightX = contrib x u
            weightY = contrib y v
            weightZ = contrib z w
         in weightX * weightY * weightZ
      where
        contrib :: Int -> Float -> Float
        contrib n coord =
            if n == 1 then coord else 1 - coord

    getCornerCoordinatesForIndex :: Int -> (Int, Int, Int)
    getCornerCoordinatesForIndex i =
        (i `div` 4, (i `div` 2) `mod` 2, i `mod` 2)
