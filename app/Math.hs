{-# LANGUAGE InstanceSigs #-}

module Math where

import Data.Ord (clamp)

type Resolution = (Integer, Integer)
type ImageCoord = (Integer, Integer)
type TextureCoord = (Float, Float)
type NormalizedColor = (Integer, Integer, Integer)

normalizeColor :: Color -> NormalizedColor
normalizeColor (Vector3 r g b) =
    ( floor (255.999 * clamp (0, 0.999) r)
    , floor (255.999 * clamp (0, 0.999) g)
    , floor (255.999 * clamp (0, 0.999) b)
    )

ratio :: Integer -> Integer -> Float
ratio a b = fromIntegral a / ((fromIntegral b) - 1)

-- Vec 3 class
data Vector3 = Vector3 Float Float Float deriving (Show, Eq)
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

dot :: Vector3 -> Vector3 -> Float
dot (Vector3 ax ay az) (Vector3 bx by bz) =
    (ax * bx) + (ay * by) + (az * bz)

cross :: Vector3 -> Vector3 -> Vector3
cross (Vector3 ax ay az) (Vector3 bx by bz) =
    let x = ay * bz - az * by
        y = az * bx - ax * bz
        z = ax * by - ay * bx
     in Vector3 x y z

length_squared :: Vector3 -> Float
length_squared (Vector3 x y z) = (x * x) + (y * y) + (z * z)

vecLength :: Vector3 -> Float
vecLength vec = sqrt (length_squared vec)

unit :: Vector3 -> Vector3
unit vec = vec /. (vecLength vec)
