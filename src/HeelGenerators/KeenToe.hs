module HeelGenerators.KeenToe where

import TriCad.MathPolar(
  slopeAdjustedForVerticalAngle,
  createTopFaces,
  createBottomFaces,
  createTopFacesWithVariableSlope,
  createBottomFacesWithVariableSlope,
  xyQuadrantAngle,
  QuadrantAngle(..),
  createCornerPoint,
  Slope(..),
  Radius(..),
  flatXSlope,
  flatYSlope,
  )
import TriCad.Points(Point(..))
import TriCad.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import TriCad.StlCornerPoints((+++^))
import TriCad.StlBase (StlShape(..), newStlShape, stlShapeToText)
import TriCad.StlFileWriter(writeStlToFile)
import TriCad.CornerPointsFaceExtraction ( extractTopFace, extractBottomFrontLine, extractFrontTopLine, extractBackTopLine, extractBottomFace, extractBackBottomLine, extractFrontFace ) 
import TriCad.CornerPointsFaceConversions(lowerFaceFromUpperFace, backBottomLineFromBottomFrontLine, backTopLineFromFrontTopLine, frontTopLineFromBackTopLine, upperFaceFromLowerFace, bottomFrontLineFromBackBottomLine)
import TriCad.CornerPointsTranspose ( transposeZ, transposeX, transposeY)
import TriCad.CornerPointsDebug((+++^?), CubeName(..), CubeDebug(..), CubeDebugs(..))


-------------------------------------------------------------------------- create the sole riser ------------------------------------------------
{-
Will be sloped on top
-}

---------------------------------------------------------------------------- create the shoe sole to tread adaptor ----------------------------------
adaptorTopOrigin = (Point{x_axis=0, y_axis=(0), z_axis=25})
adaptorBtmOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})

writeAdaptorToStlFile :: IO()
writeAdaptorToStlFile  =  writeFile "src/Data/temp.stl" $ stlShapeToText adaptorStlFile

adaptorStlFile = newStlShape "KeenToeAdaptor"  $  adaptorTriangles

adaptorTriangles =
   [FacesBackBottomFrontTop | x <- [1,2..36]]
   +++^
   adaptorCubes

adaptorCubesDebug = 
   [CubeName "adaptorCubes" | x <- [1..]]
   +++^?
   adaptorCubes

adaptorCubes = adaptorTopFaces ++++ adaptorBtmFaces

----------- top faces
adaptorTopFacesDebug =
   [CubeName "adaptorTopFaces" | x <- [1..]]
   +++^?
   adaptorTopFaces

adaptorTopFaces =
     --front line
    map (extractFrontTopLine) (createTopFaces adaptorTopOrigin soleRadius angles flatXSlope flatYSlope)
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces adaptorTopOrigin keyRadius angles flatXSlope flatYSlope)   


--------- btm faces
adaptorBtmFacesDebug =
   [CubeName "adaptorBtmFaces" | x <- [1..]]
   +++^?
   adaptorBtmFaces
    

adaptorBtmFaces = 
  --front line
  map (extractBottomFrontLine) (createBottomFaces adaptorBtmOrigin treadRadius angles flatXSlope flatYSlope)
  ++++
  --back line
  map (backBottomLineFromBottomFrontLine . extractBottomFrontLine) (createBottomFaces adaptorBtmOrigin keyRadius angles flatXSlope flatYSlope)

--------------------------------------------------------------------------- create the keyway ---------------------------------------------------------
writeKeyToStlFile :: IO()
writeKeyToStlFile  =  writeFile "src/Data/temp.stl" $ stlShapeToText keyStlFile

keyStlFile = newStlShape "KeenToeKey"  $  keyTriangles

keyTriangles = [FacesBottomFrontTop | x <- [1,2..36]]
 +++^
 keyCubes

keyDebug = 
  [CubeName "keyCubes" | x <- [1..]]
  +++^?
  keyCubes

keyCubes =
 createTopFaces topKeyOrigin keyRadius angles flatXSlope flatYSlope
 ++++
 createBottomFaces btmKeyOrigin keyRadius angles flatXSlope flatYSlope

topKeyOrigin = (Point{x_axis=0, y_axis=(0), z_axis=25})
btmKeyOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})




--------------------------------------------------------------------------------- radii and other base info --------------------------------------------------------------------
angles = [0,10..380]

--radius of the keyway, base on the shape of the keen sole.
keyRadius = map (\(Radius x) -> (Radius (x * 0.5))) soleRadius 

--radius of the tread of the original keen shoe
soleRadius = --  map (\(Radius x) -> Radius (x * 0.9))
    [Radius 54,--0
     Radius 57,--1
     Radius 58,--2
     Radius 57,--3
     Radius 54,--4
     Radius 52, --5
     Radius 49,--6
     Radius 47,--7
     Radius 45,--8
     Radius 45,--9
     Radius 46,--10
     Radius 47,--11
     Radius 49,--12
     Radius 52, --13
     Radius 59,--14
     Radius 68,--15
     Radius 81,--16
     Radius 77,--17
     Radius 75, --18 180 degrees
     Radius 77, --17
     Radius 81, --16
     Radius 67,--15
     Radius 60,--14
     Radius 55,--13
     Radius 50,--12
     Radius 45,--11
     Radius 42,--10
     Radius 41,--9
     Radius 40,--8
     Radius 40,--7
     Radius 40,--6
     Radius 41,--5
     Radius 43,--4
     Radius 46,--3
     Radius 49,--2
     Radius 52,--1
     Radius 54--0
    ] 



--the center val is the 180 deg val, which does not get mirrored.
--Create the full 360 deg radius set, by mirroring the first half, and adding in the 180 degree va..
treadRadius = concat [treadHalfRadius, [Radius (76 * 1 )], reverse treadHalfRadius]


--the final radii, after adustments
treadHalfRadius = zipWith (adjustRadius)  treadAdjFactors treadHalfRadiusAsMeasured

adjustRadius ::  (Double -> Double) -> Radius -> Radius
adjustRadius f (Radius rad)  = Radius (f rad)  

--the amount that most of the radii will be adjusted by.
treadRadiusAdjustmentFactor = 0.85

--as measured on the bottom of the tread, to the very outside of the grips.
--It is symmetrical, so only measure half. 180 degrees is put in separate so it does not get mirrored.
treadHalfRadiusAsMeasured = --map (\(Radius x) -> Radius (x * treadRadiusAdjustmentFactor))
 [
  Radius 56,--0
  Radius 57,--1
  Radius 57,--2
  Radius 57,--3
  Radius 55,--4
  Radius 53,--5
  Radius 51,--6
  Radius 50,--7
  Radius 50,--8
  Radius 50,--9
  Radius 52,--10
  Radius 55,--11
  Radius 58,--12
  Radius 63,--13
  Radius 67,--14
  Radius 70,--15
  Radius 81,--16
  Radius 77--17
 ]

--create an array of adjustment factors, to the radii can be fine tuned.
treadAdjFactors =
  [
  (*1.05),--0
  (*1.05),--1
  (*1.05),--2
  (*1),--3
  (*0.95),--4
  (*0.9),--5
  (*treadRadiusAdjustmentFactor),--6
  (*treadRadiusAdjustmentFactor),--7
  (*treadRadiusAdjustmentFactor),--8
  (*treadRadiusAdjustmentFactor),--9
  (*treadRadiusAdjustmentFactor),--10
  (*treadRadiusAdjustmentFactor),--11
  (*0.9),--12
  (*0.95),--13
  (*1),--14
  (*1.15),--15
  (*1),--16
  (*1)--17
  ]


