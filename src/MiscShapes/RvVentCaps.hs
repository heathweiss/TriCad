{--------------------- left off--------------------------
Need to flip it with slic3r, and generate the gcode.
Could do it manually, but quite a bit of work.

Need to remove ring and cylinder to their own module.

Neither slic3r or netfabb are showing errors
-}



module MiscShapes.RvVentCaps(hello, debug, stlFile, writeRvCapStlFile) where
      
import TriCad.MathPolar(
  slopeAdjustedForVerticalAngle,
  createTopFaces,
  createBottomFaces,
  createTopFacesWithVariableSlope,
  createBottomFacesWithVariableSlope,
  radiusAdjustedForZslope,
  xValue,
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
import TriCad.StlBase (StlShape(..), newStlShape)
import TriCad.CornerPointsFaceExtraction ( extractTopFace, extractBottomFrontLine, extractFrontTopLine, extractBackTopLine, extractBottomFace, extractBackBottomLine, extractFrontFace )

import TriCad.CornerPointsFaceConversions(lowerFaceFromUpperFace, backBottomLineFromBottomFrontLine, backTopLineFromFrontTopLine, frontTopLineFromBackTopLine, upperFaceFromLowerFace, bottomFrontLineFromBackBottomLine)
import TriCad.CornerPointsTranspose ( transposeCornerPointsZ, transposeCornerPointsX, transposeCornerPointsY)
import TriCad.CornerPointsDebug((+++^?), CubeName(..), CubeDebug(..), CubeDebugs(..))
import TriCad.StlFileWriter(writeStlToFile)

{--------------------------- overview -------------------------------------------
The cap has to be printed upside-down, but flip it using Slic3r.
-}

--test for module connection
hello = do
  putStrLn "hello from rv vent caps"

stlFile = newStlShape "rv vent cap" $  innerPlateTriangles
                                      ++ riserJoinerTriangles
                                      ++ riserTriangles
                                      ++ outerPlateTriangles
                                      ++ dripCapJoinerTriangles
                                      ++ dripCapTriangles
{-
                                      innerPlateTriangles
                                      ++ riserJoinerTriangles
                                      ++ riserTriangles
                                      ++ outerPlateTriangles
                                      ++ dripCapJoinerTriangles
                                      ++ dripCapTriangles
-}
writeRvCapStlFile = writeStlToFile stlFile

debug =  [CubeName "" | x <- [1..]]
    +++^?
    dripCapCubes

plateTopOrigin = (Point{x_axis=0, y_axis=0, z_axis=23})
plateBtmOrigin = riserTopOrigin

riserBtmOrigin = (Point{x_axis=0, y_axis=0, z_axis=0})
riserTopOrigin = (Point{x_axis=0, y_axis=0, z_axis=20})

dripCapTopOrigin = plateBtmOrigin
dripCapBtmOrigin = (Point{x_axis=0, y_axis=0, z_axis=10})


--this is for all angles
angles = [0,10..360]

--start with the back radius for the ring that is the center of the cover
innerPlateBackRadius = [Radius 0.5 | x <- [1..37]]
innerPlateFrontRadius = [Radius 23 | x <- [1..37]]
riserBackRadius = innerPlateFrontRadius
riserFrontRadius = map (\(Radius x) -> Radius (x + 12)) riserBackRadius
outerPlateBackRadius = riserFrontRadius
outerPlateFrontRadius = map (\(Radius x) -> Radius (x + 12)) outerPlateBackRadius
dripCapBackRadius = outerPlateFrontRadius
dripCapFrontRadius =  map (\(Radius x) -> Radius (x + 3)) outerPlateFrontRadius

{-------------------- drip cap -------------------------}
dripCapTriangles =
   [FacesBackBottomFront | x <- [1..36]]
  +++^
  dripCapCubes
  
dripCapCubes =
  cylinder
    dripCapBtmOrigin
    dripCapTopOrigin
    dripCapBackRadius
    dripCapFrontRadius
    angles

{-------------------  drip cap joiner ---
the part of the top plate that joins the drip cap to the rest of the top plate

--}
dripCapJoinerTriangles =
  [FacesFrontTop | x <- [1..36]]
  +++^
  dripCapJoinerCubes
  
  

dripCapJoinerCubes =
  ring
    plateBtmOrigin
    dripCapBackRadius
    dripCapFrontRadius
    plateTopOrigin
    dripCapBackRadius
    dripCapFrontRadius
    angles 


{--------------------------- outer plate -----------
Part of top plate.
Section between the riser joiner and the drip cap joiner
-}
outerPlateTriangles =
   [FacesBottomTop | x <- [1..36]]
   +++^
   outerPlateCubes

outerPlateCubes =
  ring
    plateBtmOrigin
    outerPlateBackRadius
    outerPlateFrontRadius
    plateTopOrigin
    outerPlateBackRadius
    outerPlateFrontRadius
    angles 
    

{----------------------  inner plate section ---------------------------------
The section of the top plate that is inside of the inner riser ring.

Left a small hole in the middle, so slic3r does not have to repair it.
-}
innerPlateTriangles =  [FacesBackBottomTop | x <- [1..36]]
                                 +++^
                                 innerPlateCubes

innerPlateCubes = ring
                              plateBtmOrigin
                              innerPlateBackRadius
                              riserBackRadius
                              plateTopOrigin
                              innerPlateBackRadius
                              riserBackRadius
                              angles

{-------------------------- riser joiner  section -----------
Part of the top plate. Joins the top plate to the inner riser.

Done:
Need to remove the bottoms where meets risers, so it will print proper.
Need to remove everything else, except the tops.
-}

--innerRiserJoinerTriangles = [FacesBackBottomFrontTop | x <- [1..36]]
riserJoinerFaces = concat [
                               [FaceTop | x <- [1..3] ],
                               [FacesBottomTop | x <- [4..9]],
                               [FaceTop | x <- [10..12] ],
                               [FacesBottomTop | x <- [13..18]],
                               [FaceTop | x <- [19..21] ],
                               [FacesBottomTop | x <- [22..27]],
                               [FaceTop | x <- [28..30] ],
                               [FacesBottomTop | x <- [31..36]]
                             ]

riserJoinerFacesCloseBackFront = concat [
                               [FacesBackFront | x <- [1..3] ],
                               [FacesBackFront | x <- [4..9]],
                               [FacesBackFront | x <- [10..12] ],
                               [FacesBackFront | x <- [13..18]],
                               [FacesBackFront | x <- [19..21] ],
                               [FacesBackFront | x <- [22..27]],
                               [FacesBackFront | x <- [28..30] ],
                               [FacesBackFront | x <- [31..36]]
                             ]

riserJoinerFacesCloseFront = concat [
                               [FaceFront | x <- [1..3] ],
                               [FaceFront | x <- [4..9]],
                               [FaceFront | x <- [10..12] ],
                               [FaceFront | x <- [13..18]],
                               [FaceFront | x <- [19..21] ],
                               [FaceFront | x <- [22..27]],
                               [FaceFront | x <- [28..30] ],
                               [FaceFront | x <- [31..36]]
                             ] 

riserJoinerTriangles =  
   (riserJoinerFaces +++^ riserJoinerCubes)
   

riserJoinerCubes = ring plateBtmOrigin riserBackRadius riserFrontRadius 
                        plateTopOrigin  riserBackRadius riserFrontRadius angles
 

{------------------------- inner riser ring----------------------------------
This is the ring that sits on the existing cap.

Make it thicker so that holes can be drilled for studs

Make gaps between studs for venting.

Done:
Removed the tops so that it will print proper.

-}

riserFaces = concat [
                               [FacesBackBottomFrontRight ],
                               [FacesBackBottomFront],
                               [FacesBackBottomFrontLeft],
                               [FacesNada | x <- [4..9]],
                               [FacesBackBottomFrontRight ],
                               [FacesBackBottomFront],
                               [FacesBackBottomFrontLeft],
                               [FacesNada | x <- [13..18]],
                               [FacesBackBottomFrontRight ],
                               [FacesBackBottomFront],
                               [FacesBackBottomFrontLeft],
                               [FacesNada | x <- [22..27]],
                               [FacesBackBottomFrontRight ],
                               [FacesBackBottomFront],
                               [FacesBackBottomFrontLeft],
                               [FacesNada | x <- [30..36]]
                             ]

riserFacesCloseTop = concat [
                               [FaceTop ],
                               [FaceTop],
                               [FaceTop],
                               [FacesNada | x <- [4..9]],
                               [FaceTop ],
                               [FaceTop],
                               [FaceTop],
                               [FacesNada | x <- [13..18]],
                               [FaceTop ],
                               [FaceTop],
                               [FaceTop],
                               [FacesNada | x <- [22..27]],
                               [FaceTop ],
                               [FaceTop],
                               [FaceTop],
                               [FacesNada | x <- [30..36]]
                             ]


riserTriangles = (riserFaces +++^ riserCubes)
                

riserCubes = ring riserBtmOrigin riserBackRadius riserFrontRadius riserTopOrigin
                 riserBackRadius riserFrontRadius angles
 

{--------- ring ------------------------
Create a ring function.
The top and bottom radius must be included so the values can vary.

The top and bottom origin must be given so that it can stand at an angle, instead
of straight up.

The top and bottom are level horizontally, as no slope is given.
-}
ring btmOrigin btmInnerRadius btmOuterRadius topOrigin topInnerRadius topOuterRadius angles  =
  topFaces ++++ btmFaces
   where
     topFaces =
       (map (extractFrontTopLine) (createTopFaces topOrigin topOuterRadius angles flatXSlope (PosYSlope 0)))
       ++++
       (map
        (backTopLineFromFrontTopLine . extractFrontTopLine)
        (createTopFaces topOrigin topInnerRadius angles flatXSlope flatYSlope)
       )
     
     btmFaces =
       (map (extractBottomFrontLine)
        (createBottomFaces btmOrigin btmOuterRadius angles flatXSlope flatYSlope))
       ++++
       (map
        (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
        (createBottomFaces btmOrigin btmInnerRadius angles flatXSlope flatYSlope)
       )
  
{------------ cylinder ----------------------------
A ring which has the same circumference, and therefore the same bottom/top radius.
Reduces the number of params req'd, to make code more readable
-}
cylinder btmOrigin topOrigin backRadius frontRadius angles =
  ring btmOrigin backRadius frontRadius topOrigin backRadius frontRadius angles
