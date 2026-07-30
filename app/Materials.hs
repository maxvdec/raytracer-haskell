{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Materials where

import GHC.Generics (Meta)
import Geometry.HitInfo (HitInfo (normal, p))
import Geometry.Ray (Ray (Ray, direction, origin))
import Math (Color, nearZero, randomUnitVector, reflect, unit, (.*), dot)
import Data.Ord (clamp)

class Material a where
    scatter :: a -> Ray -> HitInfo -> IO (Maybe (Color, Ray))

data SomeMaterial = forall a. (Material a) => SomeMaterial a

instance Material SomeMaterial where
    scatter :: SomeMaterial -> Ray -> HitInfo -> IO (Maybe (Color, Ray))
    scatter (SomeMaterial mat) = scatter mat

newtype Lambertian = Lambertian {lambertianAlbedo :: Color}

instance Material Lambertian where
    scatter :: Lambertian -> Ray -> HitInfo -> IO (Maybe (Color, Ray))
    scatter mat _ hitted = do
        randomUnit <- randomUnitVector
        let scatterDirection = normal hitted + randomUnit
        if nearZero scatterDirection
            then do
                let scattered =
                        Ray
                            { origin = p hitted
                            , direction = normal hitted
                            }
                pure (Just (lambertianAlbedo mat, scattered))
            else do
                let scattered =
                        Ray
                            { origin = p hitted
                            , direction = scatterDirection
                            }
                pure (Just (lambertianAlbedo mat, scattered))

makeLambertian :: Color -> Lambertian
makeLambertian col =
    Lambertian
        { lambertianAlbedo = col
        }

data Metal = Metal {metalAlbedo :: Color, fuzz :: Float}

instance Material Metal where
    scatter :: Metal -> Ray -> HitInfo -> IO (Maybe (Color, Ray))
    scatter mat ray hitted = do
        let reflected = reflect (direction ray) (normal hitted)
        randomFuzzUnitVector <- randomUnitVector
        let fuzzedReflected = (unit reflected) + ((fuzz mat) .* randomFuzzUnitVector)
        let scattered =
                Ray
                    { origin = p hitted
                    , direction = fuzzedReflected 
                    }
        let check = (dot (direction scattered) (normal hitted)) > 0
        if check then
            pure (Just (metalAlbedo mat, scattered))
        else
            pure Nothing

makeMetal :: Color -> Float -> Metal
makeMetal col fuz =
    let clampedFuzz = clamp (0, 1) fuz in 
    Metal
        { metalAlbedo = col
        , fuzz = clampedFuzz
        }
