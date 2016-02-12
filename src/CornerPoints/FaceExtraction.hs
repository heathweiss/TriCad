module CornerPoints.FaceExtraction (
 extractFrontFace,
 extractBottomFrontLine,
 extractFrontLeftLine,
 extractFrontRightLine,
 extractFrontTopLine,
 extractTopFace,
 extractRightFace,
 extractBackTopLine,
 extractBottomFace,
 extractBackBottomLine,
 extractBackFace,
 extractLeftFace,
 extractBackRightLine
 ) where
import CornerPoints.CornerPoints(CornerPoints(..))
import CornerPoints.Points(Point(..))

extractBottomFace :: CornerPoints -> CornerPoints
extractBottomFace (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BottomFace b1 f1 b4 f4

extractFrontFace :: CornerPoints -> CornerPoints
extractFrontFace (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = FrontFace f1 f2 f3 f4
extractFrontFace (CornerPointsError _) = FrontFace (Point 0 0 0) (Point 0 0 0) (Point 0 0 0) (Point 0 0 0)

extractRightFace :: CornerPoints -> CornerPoints
extractRightFace (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = RightFace b3 b4 f3 f4

extractTopFace :: CornerPoints -> CornerPoints
extractTopFace (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = TopFace b2 f2 b3 f3

extractBottomFrontLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BottomFrontLine f1 f4
extractBottomFrontLine (BottomFace b1 f1 b4 f4) = BottomFrontLine f1 f4
extractBottomFrontLine (FrontFace f1 f2 f3 f4) = BottomFrontLine f1 f4 


extractFrontTopLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = FrontTopLine f2 f3
extractFrontTopLine (TopFace b2 f2 b3 f3) = FrontTopLine f2 f3

extractBackTopLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BackTopLine b2 b3
extractBackTopLine (TopFace b2 f2 b3 f3) = BackTopLine b2 b3

extractBackBottomLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BackBottomLine b1 b4
extractBackBottomLine (BottomFace b1 f1 b4 f4) = BackBottomLine b1 b4



extractBackFace :: CornerPoints -> CornerPoints
extractBackFace (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BackFace b1 b2 b3 b4

extractFrontLeftLine :: CornerPoints -> CornerPoints
extractFrontLeftLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = FrontLeftLine f1 f2

extractBackRightLine :: CornerPoints -> CornerPoints
extractBackRightLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BackRightLine b3 b4

extractFrontRightLine :: CornerPoints -> CornerPoints
extractFrontRightLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = FrontRightLine f3 f4

extractLeftFace :: CornerPoints -> CornerPoints
extractLeftFace (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = LeftFace b1 b2 f1 f2
