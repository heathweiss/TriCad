{---------------- intro -----------------------------
Supplies basic cylindrical shapes as cad building blocks.
Includes:
  ringBase
  cylinder
-}

module TriCad.Shapes.Cylindrical(hello, cylinderHollow, cylinderSolid) where

import TriCad.MathPolar(
  slopeAdjustedForVerticalAngle,
  createTopFaces,
  createBottomFaces,
  createTopFacesWithVariableSlope,
  Slope(..),
  Radius(..),
  Angle(..),
  flatXSlope,
  flatYSlope,
  )
import TriCad.Points(Point(..))
import TriCad.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import TriCad.CornerPointsFaceExtraction ( extractTopFace, extractBottomFrontLine, extractFrontTopLine, extractBackTopLine, extractBottomFace, extractBackBottomLine, extractFrontFace )

import TriCad.CornerPointsFaceConversions(lowerFaceFromUpperFace, backBottomLineFromBottomFrontLine, backTopLineFromFrontTopLine, frontTopLineFromBackTopLine, upperFaceFromLowerFace, bottomFrontLineFromBackBottomLine)

hello = do
  putStrLn "hello from cylindrical"


{--------- ringBase ------------------------
The base shape upon which all the others are based.
Do not export this, as it may be subject to change.

The top and bottom radius must be included so the values can vary, allowing it to be a cone.

The top and bottom origin must both be given so that it can stand at an angle, instead
of straight up.

The top and bottom are level horizontally, as no slope is given.
Need to go back and visit TriCad.MathPolar and work on the tapered vs non-taperered slopes first.
Till then, no point in doing anything with slopes.
-}
ringBase btmOrigin btmInnerRadius btmOuterRadius topOrigin topInnerRadius topOuterRadius angles  =
  topFaces ++++ btmFaces
   where
     topFaces =
       (map (extractFrontTopLine) (createTopFaces topOrigin topOuterRadius (map (Angle) angles) flatXSlope flatYSlope))
       ++++
       (map
        (backTopLineFromFrontTopLine . extractFrontTopLine)
        (createTopFaces topOrigin topInnerRadius (map (Angle) angles) flatXSlope flatYSlope)
       )
     
     btmFaces =
       (map (extractBottomFrontLine)
            (createBottomFaces btmOrigin btmOuterRadius (map (Angle) angles) flatXSlope flatYSlope))
       ++++
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
