module Examples.Primitives.Cube() where
import Primitives.Cubical(rectangularCube)
import Stl.StlCornerPoints((|+++^|))
import CornerPoints.CornerPoints( Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import CornerPoints.HorizontalFaces( rectangularSolidNoSlope)
import CornerPoints.Radius(Radius(..))
import CornerPoints.Points(Point(..))
import CornerPoints.Create(Angle(..))

writeJoinerStlFile = writeStlToFile stlFile
stlFile = newStlShape "joiner cube" rectangleTriangles

rectangle = rectangularSolidNoSlope (map (Radius) [10,10..])  (Point 0 0 0) (map (Angle) [20,160,200,340,20]) 2 
rectangleTriangles = [FacesBottomFrontTop | x <- [1..]] |+++^| rectangle

{-
rectangularSolidNoSlopeSquaredOff :: [Radius] -> Origin -> [Angle] -> Height -> Power -> [CornerPoints]
rectangularSolidNoSlopeSquaredOff    radiii    origin    angles     height    power  =
  createBottomFacesSquaredOff origin [radii] angles flatXSlope flatYSlope power
  |@+++#@|
  (upperFaceFromLowerFace . (transposeZ (+height)))
-}
