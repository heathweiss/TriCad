module Examples.Primitives.Cylinders where
import CornerPoints.HorizontalFaces(cylinderSolidNoSlopeSquaredOff, cylinderSolidNoSlope, cylinderWallsNoSlopeSquaredOff, cylinderWallsNoSlope )
import CornerPoints.Radius(Radius(..))
import CornerPoints.Points(Point(..))
import Stl.StlCornerPoints((|+++^|), (||+++^||))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import CornerPoints.Create(Angle(..))
import CornerPoints.CornerPoints(Faces(..))

angles = (map (Angle) [0,10..360])

solidCylinderSquared =
  let cylinder = cylinderSolidNoSlopeSquaredOff (Radius 10) (Point 0 0 0) angles (10 :: Height) (30 :: Power) 
      cylinderTriangles =  [FacesBackBottomFrontTop | x <- [1..35]]
             |+++^|
             cylinder
      cylinderStl = newStlShape "cylinder" cylinderTriangles
  in  writeStlToFile cylinderStl

walledCylinderSquared =
  let cylinder = cylinderWallsNoSlopeSquaredOff (Radius 100) (10::Thickness) (Point 0 0 0) angles (10 :: Height) (10 :: Power)
      --cylinderWallsNoSlope ::                  Radius -> Thickness ->       Origin ->    [Angle] -> Height -> [CornerPoints]
      cylinderTriangles =  [FacesBackBottomFrontTop | x <- [1..]]
             |+++^|
             cylinder
      cylinderStl = newStlShape "cylinder" cylinderTriangles
  in  writeStlToFile cylinderStl

walledCylinder =
  let cylinder = cylinderWallsNoSlope (Radius 100) (10::Thickness) (Point 0 0 0) angles (10 :: Height) 
      cylinderTriangles =  [FacesBackBottomFrontTop | x <- [1..35]]
             |+++^|
             cylinder
      cylinderStl = newStlShape "cylinder" cylinderTriangles
  in  writeStlToFile cylinderStl

      
type Power = Double
type Height = Double
type Thickness = Double
