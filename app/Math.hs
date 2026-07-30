{-# LANGUAGE InstanceSigs #-}

module Math where

import Data.Ord (clamp)
import System.Random (StdGen, newStdGen)
import System.Random.Stateful (IOGenM, newIOGenM, uniformRM)

(|>) :: a -> (a -> b) -> b
x |> f = f x

degreesToRadians :: Float -> Float
degreesToRadians d =
    d * (pi / 180)

type Resolution = (Integer, Integer)
type ImageCoord = (Integer, Integer)
type TextureCoord = (Float, Float)
type NormalizedColor = (Integer, Integer, Integer)
type Interval = (Float, Float)
type RandomGenerator = IOGenM StdGen

makeRandomGenerator :: IO RandomGenerator
makeRandomGenerator = newStdGen >>= newIOGenM

contains :: Interval -> Float -> Bool
contains (minimumValue, maximumValue) value =
    value > minimumValue && value < maximumValue

sizeOfInterval :: Interval -> Float
sizeOfInterval (min, max) =
    max - min

surrounds :: Interval -> Float -> Bool
surrounds (min, max) val =
    min < val && max > val

normalizeColor :: Color -> NormalizedColor
normalizeColor (Vector3 r g b) =
    ( floor (255.999 * clamp (0, 0.999) r)
    , floor (255.999 * clamp (0, 0.999) g)
    , floor (255.999 * clamp (0, 0.999) b)
    )

linearToGamma :: Color -> Color
linearToGamma (Vector3 r g b) =
    Vector3 (mapToGamma r) (mapToGamma g) (mapToGamma b)
  where
    mapToGamma :: Float -> Float
    mapToGamma linear =
        if linear > 0
            then
                sqrt linear
            else
                0

ratio :: Integer -> Integer -> Float
ratio a b = fromIntegral a / (fromIntegral b - 1)

-- Vec 3 class
data Vector3 = Vector3 !Float !Float !Float deriving (Show, Eq)
type Point3 = Vector3
type Color = Vector3

getX :: Vector3 -> Float
getX (Vector3 a _ _) = a

getY :: Vector3 -> Float
getY (Vector3 _ a _) = a

getZ :: Vector3 -> Float
getZ (Vector3 _ _ a) = a

instance Num Vector3 where
    (+) :: Vector3 -> Vector3 -> Vector3
    Vector3 ax ay az + Vector3 bx by bz =
        Vector3 (ax + bx) (ay + by) (az + bz)

    (-) :: Vector3 -> Vector3 -> Vector3
    Vector3 ax ay az - Vector3 bx by bz =
        Vector3 (ax - bx) (ay - by) (az - bz)

    (*) :: Vector3 -> Vector3 -> Vector3
    Vector3 ax ay az * Vector3 bx by bz =
        Vector3 (ax * bx) (ay * by) (az * bz)

    abs :: Vector3 -> Vector3
    abs (Vector3 ax ay az) =
        Vector3 (abs ax) (abs ay) (abs az)

    signum :: Vector3 -> Vector3
    signum (Vector3 ax ay az) =
        Vector3 (signum ax) (signum ay) (signum az)

    fromInteger :: Integer -> Vector3
    fromInteger n =
        let value = fromInteger n
         in Vector3 value value value

(.*) :: Float -> Vector3 -> Vector3
scalar .* Vector3 x y z =
    Vector3 (scalar * x) (scalar * y) (scalar * z)

(*.) :: Vector3 -> Float -> Vector3
Vector3 x y z *. scalar =
    Vector3 (scalar * x) (scalar * y) (scalar * z)

(./) :: Float -> Vector3 -> Vector3
scalar ./ Vector3 x y z =
    Vector3 (x / scalar) (y / scalar) (z / scalar)

(/.) :: Vector3 -> Float -> Vector3
Vector3 x y z /. scalar =
    Vector3 (x / scalar) (y / scalar) (z / scalar)

(.+) :: Float -> Vector3 -> Vector3
scalar .+ Vector3 x y z =
    Vector3 (scalar + x) (scalar + y) (scalar + z)

(+.) :: Vector3 -> Float -> Vector3
Vector3 x y z +. scalar =
    Vector3 (scalar + x) (scalar + y) (scalar + z)

(.-) :: Float -> Vector3 -> Vector3
scalar .- Vector3 x y z =
    Vector3 (scalar - x) (scalar - y) (scalar - z)

(-.) :: Vector3 -> Float -> Vector3
Vector3 x y z -. scalar =
    Vector3 (scalar - x) (scalar - y) (scalar - z)

nearZero :: Vector3 -> Bool
nearZero (Vector3 x y z) =
    isNearZero x && isNearZero y && isNearZero z
  where
    isNearZero :: Float -> Bool
    isNearZero val =
        let e = 1e-8
         in abs val < e

reflect :: Vector3 -> Vector3 -> Vector3
reflect v n =
    v - 2 * dot v n .* n

refract :: Vector3 -> Vector3 -> Float -> Vector3
refract uv n etaiOverEtat =
    let cosTheta = min (dot (-uv) n) 1.0
        rOutPrep = etaiOverEtat .* (uv + (cosTheta .* n))
        rOutParallel = (-sqrt (abs (1.0 - (lengthSquared rOutPrep)))) .* n in
    rOutPrep + rOutParallel

dot :: Vector3 -> Vector3 -> Float
dot (Vector3 ax ay az) (Vector3 bx by bz) =
    (ax * bx) + (ay * by) + (az * bz)

cross :: Vector3 -> Vector3 -> Vector3
cross (Vector3 ax ay az) (Vector3 bx by bz) =
    let x = ay * bz - az * by
        y = az * bx - ax * bz
        z = ax * by - ay * bx
     in Vector3 x y z

lengthSquared :: Vector3 -> Float
lengthSquared (Vector3 x y z) = (x * x) + (y * y) + (z * z)

vecLength :: Vector3 -> Float
vecLength vec = sqrt (lengthSquared vec)

unit :: Vector3 -> Vector3
unit vec = vec /. vecLength vec

randomVector :: RandomGenerator -> IO Vector3
randomVector generator = do
    Vector3 <$> randomFloat generator <*> randomFloat generator <*> randomFloat generator

randomVectorInRange :: RandomGenerator -> Interval -> IO Vector3
randomVectorInRange generator interval = do
    Vector3 <$> randomInRange generator interval <*> randomInRange generator interval <*> randomInRange generator interval

randomUnitVector :: RandomGenerator -> IO Vector3
randomUnitVector generator = do
    p <- randomVectorInRange generator (-1, 1)
    let lensq = lengthSquared p
    if lensq >= 1 || lensq < 1e-126
        then
            randomUnitVector generator
        else
            pure (unit p)

randomInHemisphere :: RandomGenerator -> Vector3 -> IO Vector3
randomInHemisphere generator normal = do
    onUnitSphere <- randomUnitVector generator
    let dotProduct = dot onUnitSphere normal
    if dotProduct > 0.0
        then
            pure onUnitSphere
        else
            pure (negate onUnitSphere)

infinity :: Float
infinity = 1 / 0

randomFloat :: RandomGenerator -> IO Float
randomFloat generator = uniformRM (0.0, 1.0) generator

randomInRange :: RandomGenerator -> Interval -> IO Float
randomInRange generator interval = uniformRM interval generator

randomInUnitDisk :: RandomGenerator -> IO Vector3
randomInUnitDisk gen = do
    randomx <- randomInRange gen (-1, 1)
    randomy <- randomInRange gen (-1, 1)
    let p = (Vector3 randomx randomy 0)
    if (lengthSquared p) < 1 then
        pure p
    else
        randomInUnitDisk gen
