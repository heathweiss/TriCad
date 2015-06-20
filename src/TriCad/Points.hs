module TriCad.Points (Point(..), transposePointz) where

{-------------------------- Point------------------------------
Points in 3D geometry.

Know uses:
Make up the corners of Cubes and Faces.
-}
data Point =  Point { x_axis :: Double, y_axis :: Double, z_axis :: Double } 
              deriving (Show)

{-
	Front view of cube:
	     2 ---------- 3
	       |		|	
	       |		|	x-axis ---->
	     1 ---------- 4 y-axis: coming toward you.
	     				z-axis: ^
	     						|
	     f: front of cube
	     b: back of cube
	Now each corner can be named with corner # and f (front) or b (back)
-}
instance Eq Point where
    Point x y z == Point xa ya za
      | (x == xa) && (y == ya)  && (z == za) = True 
      | otherwise = False


   
transposePointz :: (Double -> Double) -> Point -> Point
transposePointz transposeFormula (Point x y z) = Point x y (transposeFormula z)
