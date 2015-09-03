{-
For creating hexahedrons such as cubes, rectangular cubes, etc.
-}

module TriCad.Shapes.Cubical(rectangularCube) where
import TriCad.CornerPoints(CornerPoints(..),(+++),(+++$),(+++>>))
import TriCad.Points (Point(..))
import  TriCad.CornerPointsTranspose (transposeZ,transposeX,transposeY)
import  TriCad.CornerPointsFaceConversions(
  upperFaceFromLowerFace,
  backFaceFromFrontFace,
  f23LineFromF14Line,
  frontBottomLineFromFrontTopLine,
  backTopLineFromFrontTopLine,
  lowerFaceFromUpperFace,
  backBottomLineFromBottomFrontLine,
  frontTopLineFromBackTopLine,
  bottomFrontLineFromBackBottomLine)
import  TriCad.Math(atanDegrees)


type ZHeight = Double
type XWidth = Double
type YLength = Double


--Cube :: { height :: ZHieght, width :: XWidth, length = YLength}

rectangularCube :: ZHeight -> XWidth -> YLength -> CornerPoints
rectangularCube height width length =
  
 let
    b1Point =  (Point 0 0 0) --the origin of the cube is the B1 corner
  
  in
     -- btmFace
    (transposeX (+ width) (B4 b1Point))
    +++
    (B1 b1Point)
    +++$  
    (transposeY (+ length) ( transposeX (+ width) (F4 b1Point)))
    +++   
    (transposeY (+ length) (F1 b1Point))
    +++>>
    --top face
    (\btmFace -> (upperFaceFromLowerFace  ( transposeZ (+ height) btmFace)))
  
   
---------------------------------------------------------------- polar rectanlge -------------------------------------------------------------------
{-
Create a rectangular cube, using the polar coordinate system used by TriCad.MathPolar module.
This will allow the recangle to be created from a center point, thus allowing for nested rectangles, as would be used
when a perimeter is desired.

Known use:
Create a rectangle with an outer wall, and inner wall, so that the center could be drilled out and tapped for a bolt.


polarRectangularCube
-}

quad1Angle length width = atan (length/2) (width /s) 
