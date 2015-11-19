{-# LANGUAGE ParallelListComp #-}
module CornerPoints.HorizontalFaces(
  createTopFaces,
  createBottomFaces,
  createTopFacesWithVariableSlope,
  createBottomFacesWithVariableSlope,
  createBottomFacesSquaredOff,
  createBottomFacesSquaredOffLengthenY,
  createBottomFacesLengthenY,
  
  )where
import CornerPoints.Create( Slope(..), Origin(..), createCornerPoint, createCornerPointSquaredOff, Angle(..),  flatXSlope, flatYSlope,)
import CornerPoints.CornerPoints(CornerPoints(..), (+++>), (+++), (|+++|), Faces(..), (|@+++#@|))
import CornerPoints.Points(Point(..))
import CornerPoints.Radius(Radius(..))
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.Transpose (transposeZ, transposeY)


type Thickness = Double
type Height = Double
type Power = Double
type LengthenFactor = Double



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



createBottomFacesSquaredOff :: Origin -> [Radius] -> [Angle] -> Slope -> Slope ->  Power -> [CornerPoints]
createBottomFacesSquaredOff    inOrigin  radii       angles     xSlope   ySlope    power  =
    (createCornerPointSquaredOff 
      (F4)
      inOrigin
      (head radii)
      (head angles)
      xSlope
      ySlope
      power
    ) 
    +++
    B4 inOrigin
    +++>
    [(createCornerPointSquaredOff 
      (F1)
      inOrigin
      radius
      angle
      xSlope
      ySlope
      power
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





{-
Used to expand a radial shape along the y-axis. Expands away from the origin to keep it centered.
-}
createBottomFacesLengthenY :: Origin -> [Radius] -> [Angle] -> Slope -> Slope -> LengthenFactor -> [CornerPoints]
createBottomFacesLengthenY inOrigin radii angles xSlope ySlope lengthenFactor =
  let createRightLine (Angle angle') cube
        | angle' <= 90 =
            F1 (transposeY ((+)(negate $ lengthenFactor/2)) $ f1 cube)
            +++ (B1 $ b1 cube)
        | angle' <= 270 =
            F1 (transposeY (+(lengthenFactor/2)) $ f1 cube)
            +++ (B1 $ b1 cube)
        | otherwise =
            F1 (transposeY ((+)(negate $lengthenFactor/2)) $ f1 cube)
            +++ (B1 $ b1 cube)
  in
    
     (transposeY ((+)(negate $ lengthenFactor/2))
      (createCornerPoint
        (F4)
        inOrigin
        (head radii)
        (head angles)
        xSlope
        ySlope
      )
     )
      +++
      B4 inOrigin
     +++>
     [ createRightLine angle
       (
        (createCornerPoint
         (F1)
          inOrigin
          radius
          angle
          xSlope
          ySlope
        ) 
        +++
        B1 inOrigin
       )
      
       | angle <- tail angles
       | radius <- tail radii
     ]


{-
Used to expand a radial shape along the y-axis. Expands away from the origin to keep it centered.
-}
createBottomFacesSquaredOffLengthenY :: Origin -> [Radius] -> [Angle] -> Slope -> Slope -> Power -> LengthenFactor  -> [CornerPoints]
createBottomFacesSquaredOffLengthenY inOrigin radii angles xSlope ySlope power lengthenFactor  =
  let createRightLine (Angle angle') cube
        | angle' <= 90 =
            F1 (transposeY ((+)(negate $ lengthenFactor/2)) $ f1 cube)
            +++ (B1 $ b1 cube)
        | angle' <= 270 =
            F1 (transposeY (+(lengthenFactor/2)) $ f1 cube)
            +++ (B1 $ b1 cube)
        | otherwise =
            F1 (transposeY ((+)(negate $lengthenFactor/2)) $ f1 cube)
            +++ (B1 $ b1 cube)
  in
    
     (transposeY ((+)(negate $ lengthenFactor/2))
      (createCornerPointSquaredOff 
        (F4)
        inOrigin
        (head radii)
        (head angles)
        xSlope
        ySlope
        power
      )
     )
      +++
      B4 inOrigin
     +++>
     [ createRightLine angle
       (
        (createCornerPointSquaredOff 
         (F1)
          inOrigin
          radius
          angle
          xSlope
          ySlope
          power
        ) 
        +++
        B1 inOrigin
       )
      
       | angle <- tail angles
       | radius <- tail radii
     ]
