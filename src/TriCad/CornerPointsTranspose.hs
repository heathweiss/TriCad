module TriCad.CornerPointsTranspose (
transposeCornerPointsZ,
transposePointX,
transposePointY,
transposePointZ,
transposeCornerPointsX,
transposeCornerPointsY
) where
import TriCad.CornerPoints
import TriCad.Points
------------------------------------- transposing cubes/points ----------------------------------------------
{-
Used for: changing points by adding values, as opposed to mulipling with the scalePoints
-}


----------------------------- z-axis ---------------------------------------
transposeCornerPointsZ :: (Double -> Double) -> CornerPoints -> CornerPoints
transposeCornerPointsZ transposeFactor (CubePoints f1 f2 f3 f4 b1 b2 b3 b4)  = 
    CubePoints {f1=(transposePointZ transposeFactor f1),
                f2=(transposePointZ transposeFactor f2),
                f3=(transposePointZ transposeFactor f3),
                f4=(transposePointZ transposeFactor f4),
                b1=(transposePointZ transposeFactor b1),
                b2=(transposePointZ transposeFactor b2),
                b3=(transposePointZ transposeFactor b3),
                b4=(transposePointZ transposeFactor b4)}

transposeCornerPointsZ transposeFactor (TopFace b2 f2 b3 f3)= 
    TopFace {f2=(transposePointZ transposeFactor f2),
                f3=(transposePointZ transposeFactor f3),
                b2=(transposePointZ transposeFactor b2),
                b3=(transposePointZ transposeFactor b3)}

transposeCornerPointsZ transposeFactor (BottomFace b1 f1 b4 f4) = 
    BottomFace {f1=(transposePointZ transposeFactor f1),
                f4=(transposePointZ transposeFactor f4 ),
                b1=(transposePointZ transposeFactor b1 ),
                b4=(transposePointZ transposeFactor b4 )}

transposeCornerPointsZ transposeFactor (BottomFrontLine f1 f4) = 
    BottomFrontLine {f1=(transposePointZ transposeFactor f1),
         f4=(transposePointZ transposeFactor f4)
               }

--untested
transposeCornerPointsZ transposeFactor (BackTopLine b2 b3) =
    BackTopLine {b2=(transposePointZ transposeFactor b2),
         b3=(transposePointZ transposeFactor b3)
        }

transposeCornerPointsZ transposeFactor (FrontTopLine f2 f3) = 
    FrontTopLine {f2=(transposePointZ transposeFactor f2),
         f3=(transposePointZ transposeFactor f3)
               }

transposePointZ :: (Double -> Double)-> Point -> Point
transposePointZ transposeFactor (Point x y z) = Point {x_axis=x, y_axis=y, z_axis=(transposeFactor z)}


------------------------------- x-axis ---------------------------------
transposePointX :: (Double -> Double)-> Point -> Point
transposePointX transposeFactor (Point x y z) = Point {x_axis=(transposeFactor x), y_axis=y, z_axis=z}

transposeCornerPointsX transposeFactor (CubePoints f1 f2 f3 f4 b1 b2 b3 b4)  = 
    CubePoints {f1=(transposePointX transposeFactor f1),
                f2=(transposePointX transposeFactor f2),
                f3=(transposePointX transposeFactor f3),
                f4=(transposePointX transposeFactor f4),
                b1=(transposePointX transposeFactor b1),
                b2=(transposePointX transposeFactor b2),
                b3=(transposePointX transposeFactor b3),
                b4=(transposePointX transposeFactor b4)}

transposeCornerPointsX transposeFactor (TopFace b2 f2 b3 f3)= 
    TopFace {   f2=(transposePointX transposeFactor f2),
                f3=(transposePointX transposeFactor f3),
                b2=(transposePointX transposeFactor b2),
                b3=(transposePointX transposeFactor b3)}

transposeCornerPointsX transposeFactor (BottomFace b1 f1 b4 f4)= 
    BottomFace {f1=(transposePointX transposeFactor f1),
                f4=(transposePointX transposeFactor f4),
                b1=(transposePointX transposeFactor b1),
                b4=(transposePointX transposeFactor b4)}

------------------------------------ y-axis ---------------------------------
transposePointY :: (Double -> Double)-> Point -> Point
transposePointY transposeFactor (Point x y z) = Point {x_axis=(x), y_axis=(transposeFactor y), z_axis=z}

transposeCornerPointsY transposeFactor (CubePoints f1 f2 f3 f4 b1 b2 b3 b4)  = 
    CubePoints {f1=(transposePointY transposeFactor f1),
                f2=(transposePointY transposeFactor f2),
                f3=(transposePointY transposeFactor f3),
                f4=(transposePointY transposeFactor f4),
                b1=(transposePointY transposeFactor b1),
                b2=(transposePointY transposeFactor b2),
                b3=(transposePointY transposeFactor b3),
                b4=(transposePointY transposeFactor b4)}

transposeCornerPointsY transposeFactor (TopFace b2 f2 b3 f3)= 
    TopFace {   f2=(transposePointY transposeFactor f2),
                f3=(transposePointY transposeFactor f3),
                b2=(transposePointY transposeFactor b2),
                b3=(transposePointY transposeFactor b3)}

transposeCornerPointsY transposeFactor (BottomFace b1 f1 b4 f4)= 
    BottomFace {f1=(transposePointY transposeFactor f1),
                f4=(transposePointY transposeFactor f4),
                b1=(transposePointY transposeFactor b1),
                b4=(transposePointY transposeFactor b4)}
