{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Graphics.Texture where

import Math (Color, TextureCoord, Vector3, getX, getY, getZ)

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
