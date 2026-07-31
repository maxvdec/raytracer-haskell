{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.Texture where

import Codec.Picture (Image (imageHeight, imageWidth), Pixel (pixelAt), PixelRGB8 (PixelRGB8), convertRGB8, readImage)
import Data.Ord (clamp)
import Graphics.Noise (Perlin, makePerlinNoise, noise, turb)
import Math (Color, RandomGenerator, TextureCoord, Vector3 (Vector3), getX, getY, getZ, makeRandomGenerator, (*.))

class Texture a where
    sample :: a -> TextureCoord -> Vector3 -> Color

data SomeTexture = forall a. (Texture (a)) => SomeTexture a

instance Texture SomeTexture where
    sample :: SomeTexture -> TextureCoord -> Vector3 -> Color
    sample (SomeTexture t) = sample t

newtype SolidColor = SolidColor
    { textureAlbedo :: Color
    }

instance Texture SolidColor where
    sample :: SolidColor -> TextureCoord -> Vector3 -> Color
    sample col _ _ = (textureAlbedo col)

makeSolidColor :: Color -> SolidColor
makeSolidColor col =
    SolidColor
        { textureAlbedo = col
        }

data CheckerTexture = CheckerTexture
    { invScale :: Float
    , evenTexture :: SomeTexture
    , oddTexture :: SomeTexture
    }

instance Texture CheckerTexture where
    sample :: CheckerTexture -> TextureCoord -> Vector3 -> Color
    sample check coord p =
        let iscale = invScale check
            xInteger = (floor (iscale * getX p)) :: Integer
            yInteger = (floor (iscale * getY p)) :: Integer
            zInteger = (floor (iscale * getZ p)) :: Integer

            isEven = (xInteger + yInteger + zInteger) `mod` 2 == 0
         in if isEven then sample (evenTexture check) coord p else sample (oddTexture check) coord p

makeCheckerFromColors :: Float -> Color -> Color -> CheckerTexture
makeCheckerFromColors scale c1 c2 =
    CheckerTexture
        { invScale = 1.0 / scale
        , evenTexture = SomeTexture (makeSolidColor c1)
        , oddTexture = SomeTexture (makeSolidColor c2)
        }
makeCheckerFromTextures :: Float -> SomeTexture -> SomeTexture -> CheckerTexture
makeCheckerFromTextures scale t1 t2 =
    CheckerTexture
        { invScale = 1.0 / scale
        , evenTexture = t1
        , oddTexture = t2
        }

data ImageTexture = ImageTexture
    { textureImage :: !(Image PixelRGB8)
    }

loadImageTexture :: FilePath -> IO ImageTexture
loadImageTexture path = do
    result <- readImage path

    case result of
        Left errorMessage ->
            fail ("Could not load texture: " ++ errorMessage)
        Right dynamicImage ->
            pure
                ImageTexture
                    { textureImage = convertRGB8 dynamicImage
                    }

instance Texture ImageTexture where
    sample texture (u, v) _ =
        let image = textureImage texture
            width = imageWidth image
            height = imageHeight image

            clampedU = clamp (0, 1) u
            clampedV = 1 - (clamp (0, 1) v)

            x =
                min
                    (width - 1)
                    (floor (clampedU * fromIntegral width))

            y =
                min
                    (height - 1)
                    (floor (clampedV * fromIntegral height))

            PixelRGB8 red green blue = pixelAt image x y

            scale = 1 / 255
         in Vector3
                (fromIntegral red * scale)
                (fromIntegral green * scale)
                (fromIntegral blue * scale)

data NoiseTexture = NoiseTexture
    { perlinNoise :: Perlin
    , noiseFrequency :: Float
    }

instance Texture NoiseTexture where
    sample tex _ p =
        let perlin = (perlinNoise tex)
            scale = (noiseFrequency tex)
         in (Vector3 0.5 0.5 0.5) *. (1 + sin (scale * (getZ p) + 10 * turb perlin p 7))

makeNoiseTexture :: Float -> IO NoiseTexture
makeNoiseTexture freq = do
    randGen <- makeRandomGenerator
    perlin <- makePerlinNoise randGen 256
    pure
        ( NoiseTexture
            { perlinNoise = perlin
            , noiseFrequency = freq
            }
        )
