module Math where

type Resolution = (Integer, Integer)
type Color = (Integer, Integer, Integer)
type ImageCoord = (Integer, Integer)

normalizeColor :: (Float, Float, Float) -> Color
normalizeColor (r, g, b) = (round (r * 255), round (g * 255), round (b * 255))

ratio :: Integer -> Integer -> Float
ratio a b = fromIntegral a / ((fromIntegral b) - 1)