{-# LANGUAGE InstanceSigs #-}

module Geometry.ConstantMedium where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Geometry.AABB (AABB)
import Geometry.Hit (Hit (info), Hittable (boundingBox, hit), SomeHittable, hitT, makeHit)
import Geometry.HitInfo (HitInfo (t))
import Geometry.Ray (Ray (direction), at)
import Graphics.Materials (SomeMaterial (SomeMaterial), makeSolidIsotropic)
import Math (Color, Interval, RandomGenerator, Vector3 (Vector3), infinity, randomFloat, vecLength)

data ConstantMedium = ConstantMedium
    { mediumBoundary :: !SomeHittable
    , negInvDensity :: !Float
    , phaseFunction :: !SomeMaterial
    }

instance Hittable ConstantMedium where
    hit :: ConstantMedium -> RandomGenerator -> Ray -> Interval -> IO (Maybe Hit)
    hit medium gen r interval = runMaybeT $ do
        let infiniteInterval = (-infinity, infinity)

        hit1 <-
            MaybeT $
                hit (mediumBoundary medium) gen r infiniteInterval

        hit2 <-
            MaybeT $
                hit (mediumBoundary medium) gen r (hitT hit1, infinity)

        (newHit1, newHit2) <-
            MaybeT $
                pure (processHits (hit1, hit2))

        dist <-
            MaybeT $
                hitDistance (newHit1, newHit2)

        let finalHit = makeFinalHit newHit1 dist

        pure finalHit
      where
        processHits :: (Hit, Hit) -> Maybe (Hit, Hit)
        processHits (h1, h2) =
            let newH1T = max (hitT h1) (fst interval)
                newH2T = min (hitT h2) (snd interval)
                clampedH1T = if newH1T < 0 then 0 else newH1T
                areHitsValid = clampedH1T >= newH2T

                newH1 =
                    h1
                        { info =
                            (info h1)
                                { t = clampedH1T
                                }
                        }
                newH2 =
                    h2
                        { info =
                            (info h2)
                                { t = newH2T
                                }
                        }
             in if areHitsValid then Just (newH1, newH2) else Nothing

        raylength = vecLength (direction r)

        hitDistance :: (Hit, Hit) -> IO (Maybe Float)
        hitDistance (h1, h2) = do
            randomOffset <- randomFloat gen
            let distInsideBoundary = ((hitT h2) - (hitT h1)) * raylength
            let dist = negInvDensity medium * log randomOffset
            if dist > distInsideBoundary
                then
                    pure Nothing
                else pure (Just dist)

        makeFinalHit :: Hit -> Float -> Hit
        makeFinalHit h1 dist =
            let finalT = (hitT h1) + dist / raylength
                finalP = at r finalT
             in makeHit finalP (Vector3 1 0 0) finalT True (phaseFunction medium) (0, 0)

    boundingBox :: ConstantMedium -> AABB
    boundingBox medium = boundingBox (mediumBoundary medium)

makeMedium :: SomeHittable -> Float -> Color -> ConstantMedium
makeMedium obj density col =
    let neginvdens = -(1 / density)
        mat = makeSolidIsotropic col
     in ConstantMedium
            { mediumBoundary = obj
            , negInvDensity = neginvdens
            , phaseFunction = SomeMaterial mat
            }
