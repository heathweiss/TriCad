{-
Typeclass for changing x y and/or z values of a Point or CubePoints.
-}

module TriCad.Transposable(Transpose, transposeX, transposeY, transposeZ) where



class Transpose a where
  transposeX :: (Double -> Double) -> a -> a
  transposeY :: (Double -> Double) -> a -> a
  transposeZ :: (Double -> Double) -> a -> a
  
