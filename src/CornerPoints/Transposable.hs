{-
Typeclass for changing x y and/or z values of a Point or CubePoints.
-}

module CornerPoints.Transposable(TransposePoint, transposeX, transposeY, transposeZ, TransposeLength, transpose) where



class TransposePoint a where
  transposeX :: (Double -> Double) -> a -> a
  transposeY :: (Double -> Double) -> a -> a
  transposeZ :: (Double -> Double) -> a -> a
  
class TransposeLength a where
  transpose :: (Double -> Double) -> a -> a
