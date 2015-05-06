module TriCad.CornerPointsFaceExtraction (
 extractFrontFace,
extractBottomFrontLine,
 extractFrontTopLine,
 extractTopFace,
 extractBackTopLine,
 extractBottomFace,
 extractBackBottomLine
 ) where
import TriCad.CornerPoints(CornerPoints(..))



extractTopFace :: CornerPoints -> CornerPoints
extractTopFace (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = TopFace b2 f2 b3 f3

extractFrontFace :: CornerPoints -> CornerPoints
extractFrontFace (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = FrontFace f1 f2 f3 f4

extractBottomFrontLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BottomFrontLine f1 f4
extractBottomFrontLine (BottomFace b1 f1 b4 f4) = BottomFrontLine f1 f4
extractBottomFrontLine (FrontFace f1 f2 f3 f4) = BottomFrontLine f1 f4 


extractFrontTopLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = FrontTopLine f2 f3
extractFrontTopLine (TopFace b2 f2 b3 f3) = FrontTopLine f2 f3

extractBackTopLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BackTopLine b2 b3
extractBackTopLine (TopFace b2 f2 b3 f3) = BackTopLine b2 b3

extractBackBottomLine (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BackBottomLine b1 b4
extractBackBottomLine (BottomFace b1 f1 b4 f4) = BackBottomLine b1 b4

extractBottomFace (CubePoints f1 f2 f3 f4 b1 b2 b3 b4) = BottomFace b1 f1 b4 f4

