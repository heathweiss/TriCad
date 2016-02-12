module CornerPoints.FaceConversions(
  upperFaceFromLowerFace,
  f23LineFromF14Line,
  frontBottomLineFromFrontTopLine,
  backTopLineFromFrontTopLine,
  lowerFaceFromUpperFace,
  backBottomLineFromBottomFrontLine,
  frontTopLineFromBackTopLine,
  bottomFrontLineFromBackBottomLine,
  {-
  backFaceFromFrontLeftLine,
  backFaceFromFrontRightLine,
  backFaceFromLeftFace,
  backFaceFromRightFace,
  -}
  backFaceFromFrontFace,
  toBackFace,
  toFrontFace,
  invertFace,
  getBackFaceFromRightFace,
  getBackFaceFromRightFaceFromCube
) where
import CornerPoints.CornerPoints (CornerPoints(..))

{- |Converts BottomFace to TopFace. Inverts normal so it no longer faces outwards from the original cube. -}
upperFaceFromLowerFace:: CornerPoints -> CornerPoints
upperFaceFromLowerFace (BottomFace b1 f1 b4 f4) = TopFace b1 f1 b4 f4

{- |Converts BackFace to FrontFace. Inverts normal so it no longer faces outwards from the original cube. -}
backFaceFromFrontFace :: CornerPoints -> CornerPoints
backFaceFromFrontFace (FrontFace f1 f2 f3 f4) = BackFace f1 f2 f3 f4

{-
backFaceFromFrontLeftLine :: CornerPoints -> CornerPoints
backFaceFromFrontLeftLine (FrontLeftLine f1 f2) = BackFace f1 f2 f2 f1
-}
{-
backFaceFromFrontRightLine :: CornerPoints -> CornerPoints
backFaceFromFrontRightLine (FrontRightLine f3 f4) = BackFace f4 f3 f3 f4
-}
{-
--swing the left face counter-clockwise on its back line, so it becomes the back face
backFaceFromLeftFace :: CornerPoints -> CornerPoints
backFaceFromLeftFace (LeftFace b1 b2 f1 f2) = BackFace b1 b2 f2 f1
-}
--swing the right face clockwise on its back line, so it becomes back face
{-
backFaceFromRightFace :: CornerPoints -> CornerPoints
backFaceFromRightFace (RightFace b3 b4 f3 f4) = BackFace f4 f3 b3 b4
-}

{- |
Rotate the face to the back/line. Maintain normals so they still face outwards from the original cube..
Lines get rotated, then copied to form the 2 lines required to make a face.
In the case of a line, it will result in a BackFace with width or height, but not both.-}
toBackFace :: CornerPoints -> CornerPoints
toBackFace (RightFace b3 b4 f3 f4) = BackFace f4 f3 b3 b4
toBackFace (LeftFace b1 b2 f1 f2) = BackFace f1 f2 b2 b1
toBackFace (FrontRightLine f3 f4) = BackFace f4 f3 f3 f4
toBackFace (FrontLeftLine f1 f2) = BackFace f1 f2 f2 f1
toBackFace (FrontFace f1 f2 f3 f4) = BackFace f4 f3 f2 f1
toBackFace (BackRightLine b3 b4) = BackFace b4 b3 b3 b4


toFrontFace(RightFace b3 b4 f3 f4) = FrontFace b4 f3 b3 b4

f23LineFromF14Line :: CornerPoints -> CornerPoints
f23LineFromF14Line (BottomFrontLine f1 f4) = FrontTopLine f1 f4

frontBottomLineFromFrontTopLine :: CornerPoints -> CornerPoints
frontBottomLineFromFrontTopLine (FrontTopLine f2 f3) = BottomFrontLine f2 f3

backTopLineFromFrontTopLine :: CornerPoints -> CornerPoints
backTopLineFromFrontTopLine (FrontTopLine f2 f3) = BackTopLine f2 f3

lowerFaceFromUpperFace :: CornerPoints -> CornerPoints
lowerFaceFromUpperFace (TopFace b2 f2 b3 f3) = BottomFace b2 f2 b3 f3

backBottomLineFromBottomFrontLine :: CornerPoints -> CornerPoints
backBottomLineFromBottomFrontLine (BottomFrontLine f1 f4) = BackBottomLine f1 f4

frontTopLineFromBackTopLine :: CornerPoints -> CornerPoints
frontTopLineFromBackTopLine (BackTopLine b2 b3) = FrontTopLine b2 b3

bottomFrontLineFromBackBottomLine :: CornerPoints -> CornerPoints
bottomFrontLineFromBackBottomLine (BackBottomLine b1 b4) = BottomFrontLine b1 b4

invertFace :: CornerPoints -> CornerPoints
invertFace (FrontFace f1 f2 f3 f4) = FrontFace f4 f3 f2 f1
invertFace (BackFace b1 b2 b3 b4)  = BackFace b4 b3 b2 b1

getBackFaceFromRightFace (RightFace b3 b4 f3 f4) = BackFace f4 f3 b3 b4
getBackFaceFromRightFaceFromCube (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BackFace f4 f3 b3 b4
