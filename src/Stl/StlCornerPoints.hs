module Stl.StlCornerPoints((|+++^|), (||+++^||), (+++^)) where

import CornerPoints.CornerPoints (CornerPoints(..), Faces(..))
import Stl.StlBase (Triangle(..), newVertex)
import Control.Applicative


-- | Create triangles for a single CornerPoints
(+++^) :: Faces -> CornerPoints -> [Triangle]
face +++^ cornerPoints = getTriangles face cornerPoints

-- |Create triangles for a [CornerPoints], usually when dealing with a single layer.
(|+++^|) :: [Faces] -> [CornerPoints] -> [Triangle]
infix 3 |+++^|
faces |+++^| cornerPoints = concat $ zipWith (+++^) faces cornerPoints

-- |Create triangles for a [[CornerPoints]] such as all the CornerPoints used used to build up a shape.
(||+++^||) :: [[Faces]] -> [[CornerPoints]] -> [Triangle]
faces ||+++^|| cornerpoints = concat $ zipWith (|+++^|) faces cornerpoints
infix 3 ||+++^||
 

getTriangles :: Faces -> CornerPoints -> [Triangle]

getTriangles _ (CornerPointsError _) = []

getTriangles (FacesNada) c = []

getTriangles (FaceBack)  (CubePoints _ _ _ _ b1 b2 b3 b4) = 
 [
  (Triangle (newVertex b1) (newVertex b2) (newVertex b3)),
  (Triangle (newVertex b1) (newVertex b3) (newVertex b4))
 ]

getTriangles (FaceBottom)  (CubePoints f1 _ _ f4 b1 _ _ b4) = 
 [
  (Triangle (newVertex f1) (newVertex b1) (newVertex f4)),
  (Triangle (newVertex f4) (newVertex b1) (newVertex b4))
 ]

getTriangles (FaceFront)  (CubePoints f1 f2 f3 f4 _ _ _ _) = 
 [
  (Triangle (newVertex f1) (newVertex f3) (newVertex f2)),
  (Triangle (newVertex f1) (newVertex f4) (newVertex f3))
 ]

getTriangles (FaceLeft)  (CubePoints f1 f2 _ _ b1 b2 _ _) = 
 [
  (Triangle (newVertex f1) (newVertex f2) (newVertex b2)),
  (Triangle (newVertex f1) (newVertex b2) (newVertex b1))
 ]
 
getTriangles (FaceRight)  (CubePoints _ _ f3 f4 _ _ b3 b4) = 
 [
  (Triangle (newVertex f4) (newVertex b3) (newVertex f3)),
  (Triangle (newVertex f4) (newVertex b4) (newVertex b3))
 ] 

getTriangles (FaceTop)  (CubePoints _ f2 f3 _ _ b2 b3 _) = 
 [
  (Triangle (newVertex f2) (newVertex b3) (newVertex b2)),
  (Triangle (newVertex f2) (newVertex f3) (newVertex b3))
 ]

getTriangles (FacesAll) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceFront c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ] 

getTriangles (FacesAllButBack) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceFront c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ] 

getTriangles (FacesAllButBottom) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceFront c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ] 

getTriangles (FacesAllButFront) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
  
 ]

getTriangles (FacesAllButLeft) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceFront c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesAllButRight) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceFront c),
  (getTriangles FaceLeft c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBackBottom) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c)
 ]


getTriangles (FacesBackBottomFront) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesBackBottomFrontLeft) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceFront c),
  (getTriangles FaceBack c),
  (getTriangles FaceBottom c)
 ]

getTriangles (FacesBackBottomFrontLeftTop) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceFront c),
  (getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBackBottomFrontRight) c = concat
 [(getTriangles FaceRight c),
  (getTriangles FaceFront c),
  (getTriangles FaceBack c),
  (getTriangles FaceBottom c)
 ]

getTriangles (FacesBackBottomFrontRightTop) c = concat
 [(getTriangles FaceRight c),
  (getTriangles FaceFront c),
  (getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBackBottomFrontTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceFront c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBackBottomLeft) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceBack c),
  (getTriangles FaceBottom c)
 ]

getTriangles (FacesBackBottomLeftRight) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceBack c),
  (getTriangles FaceBottom c)
 ]

getTriangles (FacesBackBottomTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceBottom c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBackFront) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceFront c)
 ] 

getTriangles (FacesBackFrontLeft) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceLeft c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesBackFrontLeftRight) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesBackFrontLeftRightTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceFront c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBackFrontLeftTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceLeft c),
  (getTriangles FaceFront c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBackFrontRight) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceRight c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesBackFrontTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceFront c),
  (getTriangles FaceTop c)
 ] 


getTriangles (FacesBackLeftRightTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceFront c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ] 

getTriangles (FacesBackFrontRightTop) c = concat
 [(getTriangles FaceBack c),
  (getTriangles FaceFront c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ] 


getTriangles (FacesBottomFront) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesBottomFrontLeft) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceLeft c),
  (getTriangles FaceFront c)
 ]


getTriangles (FacesBottomFrontLeftRightTop) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceFront c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesBottomFrontTop) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceTop c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesBottomFrontRight) c = concat
 [(getTriangles FaceRight c),
  (getTriangles FaceFront c),
  (getTriangles FaceBottom c)
 ]

getTriangles (FacesBottomFrontLeftTop) c = concat
 [(getTriangles FaceTop c),
  (getTriangles FaceFront c),
  (getTriangles FaceLeft c),
  (getTriangles FaceBottom c)
 ]

 
getTriangles (FacesBottomFrontRightTop) c = concat
 [(getTriangles FaceTop c),
  (getTriangles FaceFront c),
  (getTriangles FaceRight c),
  (getTriangles FaceBottom c)
 ]

   
getTriangles (FacesBottomRightTop) c = concat
 [(getTriangles FaceRight c),
  (getTriangles FaceTop c),
  (getTriangles FaceBottom c)
 ]

getTriangles (FacesBottomLeft) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceLeft c)
 ]

getTriangles (FacesBottomLeftRight) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c)
 ]

getTriangles (FacesBottomLeftRightTop) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ]


getTriangles (FacesBottomFrontLeftRight) c = concat
 [(getTriangles FaceBottom c),
 (getTriangles FaceFront c),
  (getTriangles FaceLeft c),
  (getTriangles FaceRight c)
 ]

getTriangles (FacesBottomTop) c = concat
 [(getTriangles FaceBottom c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesFrontLeftRightTop) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesFrontLeft) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesFrontLeftTop) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceTop c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesFrontRight) c = concat
 [(getTriangles FaceFront c),
  (getTriangles FaceRight c)
 ]

getTriangles (FacesFrontTop) c = concat
 [(getTriangles FaceTop c),
  (getTriangles FaceFront c)
 ]

getTriangles (FacesFrontRightTop) c = concat
 [(getTriangles FaceTop c),
  (getTriangles FaceFront c),
  (getTriangles FaceRight c)
 ]

getTriangles (FacesLeftRightTop) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceRight c),
  (getTriangles FaceTop c)
 ]

getTriangles (FacesLeftTop) c = concat
 [(getTriangles FaceLeft c),
  (getTriangles FaceTop c)
 ]



