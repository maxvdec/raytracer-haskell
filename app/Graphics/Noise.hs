module Graphics.Noise where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Control.Monad (forM_, replicateM)
import Data.Bits ((.&.))
import Math (Point3, RandomGenerator, getX, getY, getZ, randomFloat)
import System.Random (randomRIO)

data Perlin = Perlin
    { noiseRandfloat :: [Float]
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
    let i = ((floor (4 * getX p)) :: Int) .&. 255
        j = ((floor (4 * getY p)) :: Int) .&. 255
        k = ((floor (4 * getZ p)) :: Int) .&. 255
        noiseX = (permX perlin) V.! i
        noiseY = (permY perlin) V.! j
        noiseZ = (permZ perlin) V.! k
     in (noiseRandfloat perlin) !! (noiseX ^ noiseY ^ noiseZ)

makePerlinNoise :: RandomGenerator -> Int -> IO Perlin
makePerlinNoise gen size = do
    randomNumbers <- replicateM size (randomFloat gen)
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
