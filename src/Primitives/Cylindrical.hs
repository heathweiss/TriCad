{-# LANGUAGE ParallelListComp #-}
{---------------- intro -----------------------------
Supplies basic cylindrical shapes as cad building blocks.
Includes:
  ringBase
  cylinder
-}

module Primitives.Cylindrical(cylinderWallsNoSlope,cylinderWallsNoSlopeSquaredOff,
  cylinderSolidNoSlope,
  cylinderWallsVariableRadiusNoSlope,
  cylinderSolidNoSlopeSquaredOff,
  cylinderSolidNoSlopeLengthenY,
  cylinderSolidNoSlopeSquaredOffLengthenY,
  cylinderWallsNoSlopeSquaredOffLengthenY,
  cylinderWallsVariableThicknessSloped,
  cylinderWallsVariableThicknessNoSlope,) where

import CornerPoints.Create(slopeAdjustedForVerticalAngle, Slope(..), Angle(..), flatXSlope, flatYSlope, Origin(..))
import CornerPoints.HorizontalFaces(createTopFaces, createBottomFaces, createTopFacesWithVariableSlope, createTopFaces,createBottomFacesSquaredOff,
                                   createBottomFacesWithVariableSlope,createBottomFacesSquaredOffLengthenY,createBottomFacesLengthenY)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|))
import CornerPoints.FaceExtraction ( extractTopFace, extractBottomFrontLine, extractFrontTopLine, extractBackTopLine, extractBottomFace, extractBackBottomLine, extractFrontFace )
import CornerPoints.FaceConversions(lowerFaceFromUpperFace, backBottomLineFromBottomFrontLine, backTopLineFromFrontTopLine, frontTopLineFromBackTopLine,
                                    upperFaceFromLowerFace, bottomFrontLineFromBackBottomLine, backFaceFromFrontFace)
import CornerPoints.Radius(Radius(..))
import CornerPoints.Transpose (transposeZ)
import CornerPoints.Transposable(TransposePoint, transposeX, transposeY, transposeZ, TransposeLength, transpose)

type Thickness = Double
type Height = Double
type Power = Double
type LengthenFactor = Double

{- |
Create a solid cylinder with
-variable Radius
-variable Slope top
-flat bottom
-}
cylinderSolidVariableRadiusVariableSlope :: [Radius] -> Origin -> [Angle] -> [Slope] -> [Slope] -> Height -> [CornerPoints]
cylinderSolidVariableRadiusVariableSlope    radii       origin     angles     xSlopes    ySlopes    height  =
  --top faces
  (
   createTopFacesWithVariableSlope (transposeZ (+ height) origin ) radii angles xSlopes ySlopes
   
  )
  |+++|
  --bottom faces
  (
    createBottomFaces origin radii  angles flatXSlope flatYSlope 
   
  )

{- |
Create a no slope walled cylinder with different inner and outer radius.
-}
cylinderWallsVariableThicknessNoSlope :: [Radius] -> [Radius] -> [Angle] -> Origin -> Height -> [CornerPoints]
cylinderWallsVariableThicknessNoSlope    innerRadii  outerRadii  angles     origin    height  =
   ((--bottom faces
      (map (backBottomLineFromBottomFrontLine . extractBottomFrontLine) (createBottomFaces {-(Point 0 0 0)-}origin innerRadii angles flatXSlope flatYSlope)) 
      |+++|
      (map (extractBottomFrontLine) (createBottomFaces {-(Point 0 0 0)-}origin outerRadii angles flatXSlope flatYSlope)) 
    )
    |@+++#@|
    ((transposeZ (+ height)) . upperFaceFromLowerFace)
   )
   

cylinderWallsVariableThicknessSloped :: [Radius] -> [Radius] -> [Angle] -> Origin -> Slope -> Slope -> Height -> [CornerPoints]
cylinderWallsVariableThicknessSloped    innerRadii  outerRadii  angles     origin      xSlope   ySlope   height  =
  (  --top Sloped faces
    (map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces {-(Point 0 0 height)-}origin innerRadii angles xSlope ySlope))
    |+++|
    (map extractFrontTopLine (createTopFaces {-(Point 0 0 height)-}origin outerRadii angles xSlope ySlope))
  )
  |+++|
  ( map extractBottomFace
    (cylinderWallsVariableThicknessNoSlope innerRadii outerRadii angles origin (0::Height)  )
  )

{- |Create a cylindrical wall, based on a single radius.
It is a convenience wrapper around cylinderWallsVariableRadiusNoSlope.-}
cylinderWallsNoSlope :: Radius -> Thickness ->  Origin -> [Angle] -> Height -> [CornerPoints]
cylinderWallsNoSlope    innerRadius    wallThickness origin    angles     height =
  cylinderWallsVariableRadiusNoSlope  [innerRadius | x<- [1..] ]    wallThickness origin    angles     height

{- |Create a cylindrical wall, based on an [Radius].-}
cylinderWallsVariableRadiusNoSlope :: [Radius] -> Thickness ->  Origin -> [Angle] -> Height -> [CornerPoints]
cylinderWallsVariableRadiusNoSlope    innerRadii    wallThickness origin    angles     height =
        let  innerCubes = createBottomFaces origin innerRadii angles flatXSlope flatYSlope
                         |@+++#@|
                         (upperFaceFromLowerFace . (transposeZ (+height)))

             outerCubes = createBottomFaces origin (map (transpose (+wallThickness )) innerRadii) angles flatXSlope flatYSlope
                          |@+++#@|
                          (upperFaceFromLowerFace . (transposeZ (+height)))
             cylinderCubes = [(backFaceFromFrontFace . extractFrontFace) currCube  |currCube <- innerCubes]
                             |+++|
                             [ (extractFrontFace) currCube    |currCube <- outerCubes]
               
        in  cylinderCubes



cylinderWallsNoSlopeSquaredOff :: Radius ->    Origin -> [Angle] -> Height -> Thickness ->  Power -> [CornerPoints]
cylinderWallsNoSlopeSquaredOff    innerRadius  origin    angles     height wallThickness    power  =
        let  innerCubes = createBottomFacesSquaredOff origin [innerRadius | x <-  [1..]] angles flatXSlope flatYSlope power
                         |@+++#@|
                         (upperFaceFromLowerFace . (transposeZ (+height)))
         
             outerCubes = createBottomFacesSquaredOff origin [Radius ((radius innerRadius) + wallThickness) |x <- [1..]] angles flatXSlope flatYSlope power
                          |@+++#@|
                          (upperFaceFromLowerFace . (transposeZ (+height)))
             cylinderCubes = [(backFaceFromFrontFace . extractFrontFace) currCube  |currCube <- innerCubes]
                             |+++|
                             [ (extractFrontFace) currCube    |currCube <- outerCubes]
             
        in   cylinderCubes


cylinderWallsNoSlopeSquaredOffLengthenY :: Radius ->     Origin -> [Angle] -> Height -> Thickness -> Power -> LengthenFactor -> [CornerPoints]
cylinderWallsNoSlopeSquaredOffLengthenY    innerRadius  origin     angles     height   wallThickness power  lengthenFactor =
        let  innerCubes = createBottomFacesSquaredOffLengthenY origin [innerRadius | x <-  [1..]] angles flatXSlope flatYSlope power lengthenFactor
                         |@+++#@|
                         (upperFaceFromLowerFace . (transposeZ (+height)))
         
             outerCubes = createBottomFacesSquaredOffLengthenY origin [Radius ((radius innerRadius) + wallThickness) |x <- [1..]] angles flatXSlope flatYSlope power lengthenFactor
                          |@+++#@|
                          (upperFaceFromLowerFace . (transposeZ (+height)))
             cylinderCubes = [(backFaceFromFrontFace . extractFrontFace) currCube  |currCube <- innerCubes]
                             |+++|
                             [ (extractFrontFace) currCube    |currCube <- outerCubes]
             
        in   cylinderCubes

cylinderSolidNoSlope :: Radius -> Origin -> [Angle] -> Height -> [CornerPoints]
cylinderSolidNoSlope    radius    origin    angles     height  =
  createBottomFaces origin [radius | x <- [1..]] angles flatXSlope flatYSlope
  |@+++#@|
  (upperFaceFromLowerFace . (transposeZ (+height)))

cylinderSolidNoSlopeLengthenY :: Radius -> Origin -> [Angle] -> Height -> LengthenFactor -> [CornerPoints]
cylinderSolidNoSlopeLengthenY    radius    origin    angles     height    lengthenFactor  =
  createBottomFacesLengthenY origin [radius | x <- [1..]] angles flatXSlope flatYSlope lengthenFactor
  |@+++#@|
  (upperFaceFromLowerFace . (transposeZ (+height)))


cylinderSolidNoSlopeSquaredOffLengthenY :: Radius -> Origin -> [Angle] -> Height  -> Power -> LengthenFactor -> [CornerPoints]
cylinderSolidNoSlopeSquaredOffLengthenY    radius    origin    angles     height     power    lengthenFactor      =
--createBottomFacesSquaredOffLengthenY :: Origin -> [Radius] -> [Angle] -> Slope -> Slope -> Power -> LengthenFactor  -> [CornerPoints]
  createBottomFacesSquaredOffLengthenY origin [radius | x <- [1..]] angles flatXSlope flatYSlope power lengthenFactor 
  |@+++#@|
  (upperFaceFromLowerFace . (transposeZ (+height)))


--
cylinderSolidNoSlopeSquaredOff :: Radius -> Origin -> [Angle] -> Height -> Power -> [CornerPoints]
cylinderSolidNoSlopeSquaredOff    radius    origin    angles     height    power  =
  createBottomFacesSquaredOff origin [radius | x <- [1..]] angles flatXSlope flatYSlope power
  |@+++#@|
  (upperFaceFromLowerFace . (transposeZ (+height)))




  
-- =========================================== all following is the original cylinders =================================================
{--------- ringBase ------------------------
The base shape upon which all the others are based.
Do not export this, as it may be subject to change.

The top and bottom radius must be included so the values can vary, allowing it to be a cone.

The top and bottom origin must both be given so that it can stand at an angle, instead
of straight up.

The top and bottom are level horizontally, as no slope is given.
Need to go back and visit TriCad.MathPolar and work on the tapered vs non-taperered slopes first.
Till then, no point in doing anything with slopes.

ringBase btmOrigin btmInnerRadius btmOuterRadius topOrigin topInnerRadius topOuterRadius angles  =
  topFaces |+++| btmFaces
   where
     topFaces =
       (map (extractFrontTopLine) (createTopFaces topOrigin topOuterRadius (map (Angle) angles) flatXSlope flatYSlope))
       |+++|
       (map
        (backTopLineFromFrontTopLine . extractFrontTopLine)
        (createTopFaces topOrigin topInnerRadius (map (Angle) angles) flatXSlope flatYSlope)
       )
     
     btmFaces =
       (map (extractBottomFrontLine)
            (createBottomFaces btmOrigin btmOuterRadius (map (Angle) angles) flatXSlope flatYSlope))
       |+++|
       (map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
            (createBottomFaces btmOrigin btmInnerRadius (map (Angle) angles) flatXSlope flatYSlope)
       )
  
{------------ cylinders ----------------------------
Rings which have the same circumference from end to end, and therefore the same bottom/top radius.
Reduces the number of params req'd, to make code more readable

If it was to stand plumb, could have a height instead of a top origin.
-}


cylinderHollow btmOrigin topOrigin innerRadius outerRadius angles =
  ringBase btmOrigin innerRadius outerRadius topOrigin  innerRadius outerRadius angles

cylinderSolid btmOrigin topOrigin outerRadius angles =
  ringBase
    btmOrigin ( [Radius 0 | x <- [1..]]) outerRadius
    topOrigin ( [Radius 0 | x <- [1..]]) outerRadius
    angles

{-------------------- cones -------------------------------
Almost the same as ring base.
-}
--coneHollow  includes all that ringBase uses.

--coneSolid  Same as coneHollow, but no need for inner radii. Do the same as cylinder solid.
-}
