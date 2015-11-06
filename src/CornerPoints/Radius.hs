module CornerPoints.Radius(Radius(..)) where
import CornerPoints.Transposable( TransposeLength, transpose)


{-|
Represents a radius of a circular shape, which is what all shapes in math polar are created from.
-}
data Radius = Radius {radius :: Double}
   deriving (Show)

radiusEqual :: (Eq a, Num a, Ord a, Fractional a) => a -> a -> Bool
radiusEqual  a b
  
  | (abs (a - b)) <= 0.011 = True
  | otherwise      = False

instance Eq Radius where
    Radius rad == Radius rad'
      | (radiusEqual rad rad') = True 
      | otherwise = False


instance TransposeLength Radius where
  transpose f (Radius a) = Radius $ f a
