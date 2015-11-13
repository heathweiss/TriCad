{-# LANGUAGE ParallelListComp #-}
module CornerPoints.HorizontalFaces(
  createTopFaces,
  createBottomFaces,
  createTopFacesWithVariableSlope,
  createBottomFacesWithVariableSlope,
  cylinderWallsNoSlope,
  cylinderSolidNoSlope
  )where
import CornerPoints.Create( Slope(..), Origin(..), createCornerPoint, Angle(..),  flatXSlope, flatYSlope,)
import CornerPoints.CornerPoints(CornerPoints(..), (+++>), (+++), (|+++|), Faces(..), (|@+++#@|))
import CornerPoints.Points(Point(..))
import CornerPoints.Radius(Radius(..))
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.Transpose (transposeZ)

{- |Create a single CornerPoints.BottomRightLine, and add it to the head of [BottomLeftLine]. This will give a BottomFace.
Then add the next BottomLeftLine to this BottomFace to get the next BottomFace. Continue through the [BottomLeftLine] till
the [BottomFace] is done.

All Lines are created from the inputs, using Polar math.
QuadrantAngle
-}

createBottomFaces :: Origin -> [Radius] -> [Angle] -> Slope -> Slope -> [CornerPoints]
createBottomFaces inOrigin radii angles xSlope ySlope  =
    (createCornerPoint
      (F4)
      inOrigin
      (head radii)
      (head angles)
      xSlope
      ySlope
    ) 
    +++
    B4 inOrigin
    +++>
    [(createCornerPoint
      (F1)
      inOrigin
      radius
      angle
      xSlope
      ySlope
     ) 
     +++
     B1 inOrigin
       | angle <- tail angles
       | radius <- tail radii
    ]


createBottomFacesWithVariableSlope :: Origin -> [Radius] -> [Angle] -> [Slope] -> [Slope] -> [CornerPoints]
createBottomFacesWithVariableSlope inOrigin inRadius inAngles xSlope ySlope  =
    (createCornerPoint
      (F4)
      inOrigin
      (head inRadius) 
      (head inAngles)
      (head xSlope)
      (head ySlope)
    ) 
    +++
    B4 inOrigin
    +++>
    [(createCornerPoint
      (F1)
      inOrigin
      currRadius
      angle
      currXSlope currYSlope
     ) 
     +++
     B1 inOrigin
       | angle <- tail inAngles
       | currRadius <- tail inRadius
       | currXSlope <- tail xSlope
       | currYSlope <- tail ySlope
    ]

{---------------------------------------------------------------- createTopFaces ----------------------------

-}
createTopFaces :: Origin -> [Radius] -> [Angle] -> Slope -> Slope -> [CornerPoints]
createTopFaces inOrigin inRadius inAngles xSlope ySlope  =
     (createCornerPoint
      (F3)
      inOrigin
      (head inRadius) 
      (head inAngles)
      xSlope ySlope
    ) 
    +++
    B3 inOrigin
    +++>
    [(createCornerPoint
      (F2)
      inOrigin
      currRadius
      angle
      xSlope
      ySlope
     ) 
     +++
     B2 inOrigin
       | angle <- tail inAngles
       | currRadius <- tail inRadius
    ]

    
createTopFacesWithVariableSlope inOrigin inRadius inAngles xSlope ySlope  =
    (createCornerPoint
      (F3)
      inOrigin
      (head inRadius) 
      
      (Angle (head inAngles))
      (head xSlope)
      (head ySlope)
    ) 
    +++
    B3 inOrigin
    +++>
    [(createCornerPoint
      (F2)
      inOrigin
      currRadius
      (Angle angle)
      currXSlope
      currYSlope
     ) 
     +++
     B2 inOrigin
       | angle <- tail inAngles
       | currRadius <- tail inRadius
       | currXSlope <- tail xSlope
       | currYSlope <- tail ySlope
    ]


cylinderWallsNoSlope :: Radius -> Thickness ->  Origin -> [Angle] -> Height -> [CornerPoints]
cylinderWallsNoSlope    innerRadius    wallThickness origin    angles     height =
        let  --innerCubes = createBottomFaces origin (map (Radius) [innerRadius,innerRadius..]) angles flatXSlope flatYSlope
             innerCubes = createBottomFaces origin [innerRadius | x <-  [1..]] angles flatXSlope flatYSlope
                         |@+++#@|
                         (upperFaceFromLowerFace . (transposeZ (+height)))

             outerCubes = --createBottomFaces origin (map (Radius) [(innerRadius + wallThickness),(innerRadius + wallThickness)..]) angles flatXSlope flatYSlope
                         createBottomFaces origin [Radius ((radius innerRadius) + wallThickness) |x <- [1..]] angles flatXSlope flatYSlope
                         |@+++#@|
                         (upperFaceFromLowerFace . (transposeZ (+height)))
             cylinderCubes = [(backFaceFromFrontFace . extractFrontFace) currCube  |currCube <- innerCubes]
                             |+++|
                             [ (extractFrontFace) currCube    |currCube <- outerCubes]
               --outerFrontFaces = [ (extractFrontFace) currCube    |currCube <- outerCubesTemp]
               --outerBackFaces = [(backFaceFromFrontFace . extractFrontFace) currCube  |currCube <- innerCubes]
               --outerCubes = outerBackFaces |+++| outerFrontFaces
        in  cylinderCubes

cylinderSolidNoSlope :: Radius -> Origin -> [Angle] -> Height -> [CornerPoints]
cylinderSolidNoSlope    radius    origin    angles     height  =
  createBottomFaces origin [radius | x <- [1..]] angles flatXSlope flatYSlope
  |@+++#@|
  (upperFaceFromLowerFace . (transposeZ (+height)))


type Thickness = Double
type Height = Double
