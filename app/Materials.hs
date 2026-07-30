{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Materials where

import GHC.Generics (Meta)
import Geometry.HitInfo (HitInfo (normal, p, isFront))
import Geometry.Ray (Ray (Ray, direction, origin))
import Math (Color, RandomGenerator, nearZero, randomUnitVector, reflect, unit, (.*), dot, Vector3(..), refract)
import Data.Ord (clamp)

class Material a where
    scatter :: a -> RandomGenerator -> Ray -> HitInfo -> IO (Maybe (Color, Ray))

data SomeMaterial = forall a. (Material a) => SomeMaterial a

instance Material SomeMaterial where
    scatter :: SomeMaterial -> RandomGenerator -> Ray -> HitInfo -> IO (Maybe (Color, Ray))
    scatter (SomeMaterial mat) = scatter mat

newtype Lambertian = Lambertian {lambertianAlbedo :: Color}

instance Material Lambertian where
    scatter :: Lambertian -> RandomGenerator -> Ray -> HitInfo -> IO (Maybe (Color, Ray))
    scatter mat generator _ hitted = do
        randomUnit <- randomUnitVector generator
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
    scatter :: Metal -> RandomGenerator -> Ray -> HitInfo -> IO (Maybe (Color, Ray))
    scatter mat generator ray hitted = do
        let reflected = reflect (direction ray) (normal hitted)
        randomFuzzUnitVector <- randomUnitVector generator
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

data Dielectric = Dielectric { refractionIndex :: Float }

instance Material Dielectric where
    scatter :: Dielectric -> RandomGenerator -> Ray -> HitInfo -> IO (Maybe (Color, Ray))
    scatter mat _ ray hitted = do
        let attenuation = (Vector3 1 1 1)
        let ri = if (isFront hitted) then 1.0 / (refractionIndex mat) else (refractionIndex mat)

        let unitDirection = unit (direction ray)
        let cosTheta = min (dot (-unitDirection) (normal hitted)) 1.0
        let sinTheta = sqrt (1.0 - cosTheta * cosTheta)
        let cannotRefract = (ri * sinTheta) > 1.0
        let scatterDirection = if cannotRefract then reflect unitDirection (normal hitted) else refract unitDirection (normal hitted) ri
        let scattered = Ray {
            origin = (p hitted)
            , direction = scatterDirection 
        }
        pure (Just (attenuation, scattered))

makeDielectric :: Float -> Dielectric
makeDielectric refr =
    Dielectric {
        refractionIndex = refr
    }
