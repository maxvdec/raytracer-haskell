{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.Materials where

import Data.Ord (clamp)
import GHC.Generics (Meta)
import Geometry.HitInfo (HitInfo (isFront, normal, p, uv))
import Geometry.Ray (Ray (Ray, direction, origin, time))
import Graphics.Texture (SomeTexture (SomeTexture), Texture (sample), makeSolidColor)
import Math (Color, Point3, RandomGenerator, TextureCoord, Vector3 (..), dot, nearZero, randomFloat, randomInHemisphere, randomUnitVector, reflect, refract, unit, (.*))

class Material a where
    scatter :: a -> RandomGenerator -> Ray -> HitInfo -> IO (Maybe (Color, Ray))
    scatter _ _ _ _ = pure Nothing

    emit :: a -> TextureCoord -> Point3 -> Color
    emit _ _ _ = (Vector3 0 0 0)

    scatteringPDF :: a -> Ray -> HitInfo -> Ray -> Float
    scatteringPDF _ _ _ _ = 0

data SomeMaterial = forall a. (Material a) => SomeMaterial a

instance Material SomeMaterial where
    scatter :: SomeMaterial -> RandomGenerator -> Ray -> HitInfo -> IO (Maybe (Color, Ray))
    scatter (SomeMaterial mat) = scatter mat
    emit (SomeMaterial mat) = emit mat
    scatteringPDF (SomeMaterial mat) = scatteringPDF mat

newtype Lambertian = Lambertian {lambertianTexture :: SomeTexture}

instance Material Lambertian where
    scatter :: Lambertian -> RandomGenerator -> Ray -> HitInfo -> IO (Maybe (Color, Ray))
    scatter mat generator r hitted = do
        randomUnit <- randomInHemisphere generator (normal hitted)
        let scatterDirection = normal hitted + randomUnit
        if nearZero scatterDirection
            then do
                let scattered =
                        Ray
                            { origin = p hitted
                            , direction = normal hitted
                            , time = (time r)
                            }
                pure (Just (sampleTexture, scattered))
            else do
                let scattered =
                        Ray
                            { origin = p hitted
                            , direction = scatterDirection
                            , time = (time r)
                            }
                pure (Just (sampleTexture, scattered))
      where
        sampleTexture :: Color
        sampleTexture =
            let tex = (lambertianTexture mat)
             in sample tex (uv hitted) (p hitted)

    scatteringPDF :: Lambertian -> Ray -> HitInfo -> Ray -> Float
    scatteringPDF _ _ _ _ = 1 / (2 * pi)

makeLambertian :: SomeTexture -> Lambertian
makeLambertian tex =
    Lambertian
        { lambertianTexture = tex
        }

makeSolidLambertian :: Color -> Lambertian
makeSolidLambertian col =
    Lambertian
        { lambertianTexture = SomeTexture (makeSolidColor col)
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
                    , time = (time ray)
                    }
        let check = (dot (direction scattered) (normal hitted)) > 0
        if check
            then
                pure (Just (metalAlbedo mat, scattered))
            else
                pure Nothing

makeMetal :: Color -> Float -> Metal
makeMetal col fuz =
    let clampedFuzz = clamp (0, 1) fuz
     in Metal
            { metalAlbedo = col
            , fuzz = clampedFuzz
            }

data Dielectric = Dielectric {refractionIndex :: Float}

reflectance :: Dielectric -> Float -> Float
reflectance mat cosine =
    let refIndex = refractionIndex mat
        r0 = ((1 - refIndex) / (1 + refIndex)) ^ (2 :: Integer)
     in r0 + (1 - r0) * ((1 - cosine) ^ (5 :: Integer))

instance Material Dielectric where
    scatter :: Dielectric -> RandomGenerator -> Ray -> HitInfo -> IO (Maybe (Color, Ray))
    scatter mat rgen ray hitted = do
        randomSchlick <- randomFloat rgen

        let attenuation = (Vector3 1 1 1)
        let ri = if (isFront hitted) then 1.0 / (refractionIndex mat) else (refractionIndex mat)

        let unitDirection = unit (direction ray)
        let cosTheta = min (dot (-unitDirection) (normal hitted)) 1.0
        let sinTheta = sqrt (1.0 - cosTheta * cosTheta)
        let cannotRefract = (ri * sinTheta) > 1.0
        let schlickParameter = (reflectance mat cosTheta) > randomSchlick
        let scatterDirection = if (cannotRefract || schlickParameter) then reflect unitDirection (normal hitted) else refract unitDirection (normal hitted) ri
        let scattered =
                Ray
                    { origin = (p hitted)
                    , direction = scatterDirection
                    , time = (time ray)
                    }
        pure (Just (attenuation, scattered))

makeDielectric :: Float -> Dielectric
makeDielectric refr =
    Dielectric
        { refractionIndex = refr
        }

newtype Light = Light
    { emissionTexture :: SomeTexture
    }

instance Material Light where
    emit light coords point =
        sample (emissionTexture light) coords point

makeLight :: SomeTexture -> Light
makeLight tex =
    Light
        { emissionTexture = tex
        }

makeLightFromColor :: Color -> Light
makeLightFromColor col =
    Light
        { emissionTexture = SomeTexture (makeSolidColor col)
        }

newtype Isotropic = Isotropic
    { isotropicTexture :: SomeTexture
    }

instance Material Isotropic where
    scatter :: Isotropic -> RandomGenerator -> Ray -> HitInfo -> IO (Maybe (Color, Ray))
    scatter isotropic gen r info = do
        unitVec <- randomUnitVector gen
        let scattered =
                r
                    { origin = (p info)
                    , direction = unitVec
                    }
            attenuation = sample (isotropicTexture isotropic) (uv info) (p info)
        pure (Just (attenuation, scattered))

makeIsotropic :: SomeTexture -> Isotropic
makeIsotropic tex =
    Isotropic
        { isotropicTexture = tex
        }

makeSolidIsotropic :: Color -> Isotropic
makeSolidIsotropic col =
    Isotropic
        { isotropicTexture = SomeTexture (makeSolidColor col)
        }
