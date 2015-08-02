{-# LANGUAGE ParallelListComp #-}
module TriCad.MathPolar(
  slopeAdjustedForVerticalAngle,
  createTopFaces,
  createBottomFaces,
  createRightFaces,
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
  ) where
import TriCad.Points(Point(..))
import TriCad.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import TriCad.Math(sinDegrees, cosDegrees)

{--------------------overview----------------------------------------
Creates a radial shape using polar cood's.

0 degrees is as the max neg y axis, and rotates clockwise into the pos x axis.
-}

{---------------------------------------------------------------------------
e

Functions for calculating the current x and y values adjusted for xy degrees


-}


flatXSlope = PosXSlope 0
flatYSlope = PosYSlope 0



--type Radius = Double

data Radius = Radius {radius :: Double}
             | DownRadius {radius :: Double}
             | UpRadius {radius :: Double}
   deriving (Show, Eq)

data QuadrantAngle = Quadrant1Angle Double
                   | Quadrant2Angle Double
                   | Quadrant3Angle Double
                   | Quadrant4Angle Double
                   | Angle Double
                   
  deriving (Show, Eq)

{-
Used for:
Neg/Pos XY Slope: 
-Describe the slope resulting from the combining of the Neg/Pos X/Y slopes
-Neg slopes downwards from the neg x/y quadrants
-Pos slopes upwards from the neg x/y quadrants
-}
data Slope = NegXYSlope {angle :: Double}
           | PosXYSlope {angle :: Double}
           | NegXSlope {angle :: Double}
           | PosXSlope {angle :: Double}
           | NegYSlope {angle :: Double}
           | PosYSlope {angle :: Double}
  deriving (Show, Eq)

{-
Each quadrant will be 0-90 degrees.
Orientated such as sin can be used for all quadrants, to calculate x, and cos for y
-}
xyQuadrantAngle :: Double ->  QuadrantAngle
xyQuadrantAngle currAngle 
  | currAngle < 0 = xyQuadrantAngle (360 - currAngle)
  | currAngle <= 90 = Quadrant1Angle currAngle
  | currAngle <= 180 = Quadrant2Angle (180 - currAngle)
  | currAngle <= 270 = Quadrant3Angle $ 90 - (270 - currAngle)
  | currAngle <= 360 = Quadrant4Angle (360 - currAngle)
  | currAngle > 360 = xyQuadrantAngle (currAngle - 360)


{-Don't seem to be using this function, should look at getting rid of it.-}
xValue :: Radius -> QuadrantAngle -> Point -> Double
xValue (Radius radius) (Quadrant1Angle angle) origin = x_axis origin + sinDegrees angle * radius
xValue (Radius radius) (Quadrant2Angle angle) origin = x_axis origin + sinDegrees angle * radius
xValue (Radius radius) (Quadrant3Angle angle) origin = x_axis origin - sinDegrees angle * radius
xValue (Radius radius) (Quadrant4Angle angle) origin = x_axis origin - sinDegrees angle * radius

{-
Functions for calculating the current Z plane angle. It is a combination of the slope,
and the current XY angle. It has to be calculated because the slope changes as you work around the center.
This change in slope is because cubes radiate from the center, so their endpoints will be at various slopes from the center.
When all done, they should all run together at the target slope.

They also have to take into account, what quadrant they are in, as it affects the angles used with the trig.
This could be done as a separate function, which would pre-adjust the angle, or pass it in with the slope curried in.

should this return a type Angle = NegZAngle PosZAngle then continue on with testing  


-}

slopeAdjustedForVerticalAngle :: Slope -> Slope -> QuadrantAngle -> Slope
slopeAdjustedForVerticalAngle (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant1Angle xyAngle) | ((sinDegrees xyAngle) * xSlope) >= ((cosDegrees xyAngle) * ySlope)  = PosXYSlope $ abs $ ((sinDegrees xyAngle) * xSlope) - ((cosDegrees xyAngle) * ySlope)
                                                                                        | otherwise = NegXYSlope $ abs $ ((sinDegrees xyAngle) * xSlope) - ((cosDegrees xyAngle) * ySlope)
--slopeAdjustedForVerticalAngle (NegXSlope xSlope) (PosYSlope ySlope) (Quadrant1Angle xyAngle) = PosXYSlope 0

slopeAdjustedForVerticalAngle (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant1Angle xyAngle) =  PosXYSlope $ ((sinDegrees xyAngle) * xSlope) + ((cosDegrees xyAngle) * ySlope)
--slopeAdjustedForVerticalAngle (NegXSlope xSlope) (NegYSlope ySlope) (Quadrant1Angle xyAngle) = PosXYSlope 0

slopeAdjustedForVerticalAngle (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant2Angle xyAngle) = PosXYSlope $ ((sinDegrees xyAngle) * xSlope) + ((cosDegrees xyAngle) * ySlope)
--slopeAdjustedForVerticalAngle (NegXSlope xSlope) (PosYSlope ySlope) (Quadrant2Angle xyAngle) = PosXYSlope 0

slopeAdjustedForVerticalAngle (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant2Angle xyAngle) | ((sinDegrees xyAngle) * xSlope) >= ((cosDegrees xyAngle) * ySlope)  = PosXYSlope $ abs $ ((sinDegrees xyAngle) * xSlope) - ((cosDegrees xyAngle) * ySlope)
                                                                                             | otherwise = NegXYSlope $ abs $ ((sinDegrees xyAngle) * xSlope) - ((cosDegrees xyAngle) * ySlope)
--slopeAdjustedForVerticalAngle (NegXSlope xSlope) (NegYSlope ySlope) (Quadrant2Angle xyAngle) = PosXYSlope 0

--                                                                                         ySlope is >= xSlope so it is a PosXYSlope
slopeAdjustedForVerticalAngle (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant3Angle xyAngle) | ((cosDegrees xyAngle) * ySlope)  >= ((sinDegrees xyAngle) * xSlope) = PosXYSlope $ abs $  ((cosDegrees xyAngle) * ySlope) - ((sinDegrees xyAngle) * xSlope)
                                                                                        --xSlope is >  ySlope so it is a NegXYSlope
                                                                                        | otherwise = NegXYSlope $ abs $ ((cosDegrees xyAngle) * ySlope) - ((sinDegrees xyAngle) * xSlope) 
--slopeAdjustedForVerticalAngle (NegXSlope xSlope) (PosYSlope ySlope) (Quadrant3Angle xyAngle) = PosXYSlope 0

slopeAdjustedForVerticalAngle (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant3Angle xyAngle) = NegXYSlope $ ((sinDegrees xyAngle) * xSlope) + ((cosDegrees xyAngle) * ySlope)
--slopeAdjustedForVerticalAngle (NegXSlope xSlope) (NegYSlope ySlope) (Quadrant3Angle xyAngle) = PosXYSlope 0

slopeAdjustedForVerticalAngle (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant4Angle xyAngle) = NegXYSlope $ ((sinDegrees xyAngle) * xSlope) + ((cosDegrees xyAngle) * ySlope)
--slopeAdjustedForVerticalAngle (NegXSlope xSlope) (PosYSlope ySlope) (Quadrant4Angle xyAngle) = PosXYSlope 0

                                                                                        --ySlope is >= xSlope so it is a PosXYSlope
slopeAdjustedForVerticalAngle (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant4Angle xyAngle) | ((cosDegrees xyAngle) * ySlope)  >= ((sinDegrees xyAngle) * xSlope) = PosXYSlope $ abs $  ((cosDegrees xyAngle) * ySlope) - ((sinDegrees xyAngle) * xSlope)
                                                                                        --xSlope is >  ySlope so it is a NegXYSlope
                                                                                        | otherwise = NegXYSlope $ abs $ ((cosDegrees xyAngle) * ySlope) - ((sinDegrees xyAngle) * xSlope) 
--slopeAdjustedForVerticalAngle (NegXSlope xSlope) (NegYSlope ySlope) (Quadrant4Angle xyAngle) = PosXYSlope 0

                                                                                        

{-
Shorten the radius on the xy plane, for the changes in the z plane.
As per standard 3D polar to cartesian conversion methods.

radiusAdjustedForZslope :: Radius -> Slope -> Slope -> QuadrantAngle -> Radius 
radiusAdjustedForZslope (Radius radius) xSlope ySlope xyAngle = PosRadius $ radius * cosDegrees (angle (slopeAdjustedForVerticalAngle xSlope ySlope  xyAngle) )
-}
radiusAdjustedForZslope :: Radius -> Slope -> Radius 
radiusAdjustedForZslope (Radius radius) (PosXYSlope xySlope) = UpRadius $ radius * (cosDegrees (xySlope))
radiusAdjustedForZslope (Radius radius) (NegXYSlope xySlope) = DownRadius $ radius * (cosDegrees (xySlope))

 
--xRadiusAdjustedForZslope radius xSlope ySlope xyAngle = radiusAdjustedForZslope radius xSlope ySlope xyAngle

--yRadiusAdjustedForZslope radius xSlope ySlope xyAngle = negate $ radiusAdjustedForZslope radius xSlope ySlope xyAngle
--yRadiusAdjustedForZslope radius xSlope ySlope xyAngle = negate $ radiusAdjustedForZslope radius xSlope ySlope xyAngle


{-
Change slope and xy angle into a single Slope value.
Change radius into a Radius

Given:
cPoint : (Point-> CornerPoints)
-A CornerPoints constructor such as F1

adjustedRadius: Radius: 
-The radius, after being adjusted for slope

slope: Slope:
The slope, having already been adjusted for x/y slopes and xy quadrant

orign: Point
-The starting point which gets adjusted to give the return point. That way, this point can be created relative to some position, instead of at an origin of 0 0 0

-}
createCornerPoint :: (Point-> CornerPoints) -> Point -> Radius -> Radius ->  QuadrantAngle -> Slope -> CornerPoints
createCornerPoint cPoint origin (Radius horizRadius) (DownRadius adjustedRadius)  (Quadrant1Angle xyAngle) (NegXYSlope slope) = cPoint (Point 
                                    (--x:
                                     x_axis origin + adjustedRadius * (sinDegrees  xyAngle))
                                    
                                    (--y:
                                     y_axis origin - adjustedRadius * (cosDegrees  xyAngle))
                                    
                                    (--z:
                                     z_axis origin - horizRadius * (sinDegrees (slope)))
                                  )

createCornerPoint cPoint origin (Radius horizRadius) (UpRadius adjustedRadius)  (Quadrant1Angle xyAngle) (PosXYSlope slope) = cPoint (Point 
                                    (--x:
                                     x_axis origin + adjustedRadius * (sinDegrees  xyAngle))
                                    
                                    (--y:
                                     y_axis origin - adjustedRadius * (cosDegrees  xyAngle))
                                    
                                    (--z:
                                     z_axis origin + horizRadius * (sinDegrees (slope)))
                                  )

createCornerPoint cPoint origin (Radius horizRadius) (UpRadius adjustedRadius)  (Quadrant2Angle xyAngle) (PosXYSlope slope) = cPoint (Point 
                                    (--x:
                                     x_axis origin + adjustedRadius * (sinDegrees  xyAngle))
                                    
                                    (--y:
                                     y_axis origin + adjustedRadius * (cosDegrees  xyAngle))
                                    
                                    (--z:
                                     z_axis origin + horizRadius * (sinDegrees (slope)))
                                  )



createCornerPoint cPoint origin (Radius horizRadius) (DownRadius adjustedRadius)  (Quadrant2Angle xyAngle) (NegXYSlope slope) = cPoint (Point 
                                    (--x:
                                     x_axis origin + adjustedRadius * (sinDegrees  xyAngle))
                                    
                                    (--y:
                                     y_axis origin + adjustedRadius * (cosDegrees  xyAngle))
                                    
                                    (--z:
                                     z_axis origin - horizRadius * (sinDegrees (slope)))
                                  )



createCornerPoint cPoint origin (Radius horizRadius) (UpRadius adjustedRadius)  (Quadrant3Angle xyAngle) (PosXYSlope slope) = cPoint (Point 
                                    (--x:
                                     x_axis origin - adjustedRadius * (sinDegrees  xyAngle))
                                    
                                    (--y:
                                     y_axis origin + adjustedRadius * (cosDegrees  xyAngle))
                                    
                                    (--z:
                                     z_axis origin + horizRadius * (sinDegrees (slope)))
                                  )


createCornerPoint cPoint origin (Radius horizRadius) (DownRadius adjustedRadius)  (Quadrant3Angle xyAngle) (NegXYSlope slope) = cPoint (Point 
                                    (--x:
                                     x_axis origin - adjustedRadius * (sinDegrees  xyAngle))
                                    
                                    (--y:
                                     y_axis origin + adjustedRadius * (cosDegrees  xyAngle))
                                    
                                    (--z:
                                     z_axis origin - horizRadius * (sinDegrees (slope)))
                                  )

createCornerPoint cPoint origin (Radius horizRadius) (DownRadius adjustedRadius)  (Quadrant4Angle xyAngle) (NegXYSlope slope) = cPoint (Point 
                                    (--x:
                                     x_axis origin - adjustedRadius * (sinDegrees  xyAngle))
                                    
                                    (--y:
                                     y_axis origin - adjustedRadius * (cosDegrees  xyAngle))
                                    
                                    (--z:
                                     z_axis origin - horizRadius * (sinDegrees (slope)))
                                  )

--untested. 
createCornerPoint cPoint origin (Radius horizRadius) (UpRadius adjustedRadius)  (Quadrant4Angle xyAngle) (PosXYSlope slope) = cPoint (Point 
                                    (--x:
                                     x_axis origin - adjustedRadius * (sinDegrees  xyAngle))
                                    
                                    (--y:
                                     y_axis origin - adjustedRadius * (cosDegrees  xyAngle))
                                    
                                    (--z:
                                     z_axis origin + horizRadius * (sinDegrees (slope)))
                                  )


{------------------------------------------------------------- createPerimeterBottomFaces--------------------------------------------

-}
createBottomFaces inOrigin inRadius inAngles xSlope ySlope  =
    (createCornerPoint
      (F4)
      inOrigin
      (head inRadius) 
      (radiusAdjustedForZslope (head inRadius) (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle (head inAngles))))
      (xyQuadrantAngle (head inAngles))
      (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle (head inAngles)))
    ) 
    +++
    B4 inOrigin
    ++>
    [(createCornerPoint
      (F1)
      inOrigin
      currRadius
      (radiusAdjustedForZslope currRadius (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle angle)))
      (xyQuadrantAngle angle)
      (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle angle))
     ) 
     +++
     B1 inOrigin
       | angle <- tail inAngles
       | currRadius <- tail inRadius
    ]

createBottomFacesWithVariableSlope inOrigin inRadius inAngles xSlope ySlope  =
    (createCornerPoint
      (F4)
      inOrigin
      (head inRadius) 
      (radiusAdjustedForZslope (head inRadius) (slopeAdjustedForVerticalAngle (head xSlope) (head ySlope) (xyQuadrantAngle (head inAngles))))
      (xyQuadrantAngle (head inAngles))
      (slopeAdjustedForVerticalAngle (head xSlope) (head ySlope) (xyQuadrantAngle (head inAngles)))
    ) 
    +++
    B4 inOrigin
    ++>
    [(createCornerPoint
      (F1)
      inOrigin
      currRadius
      (radiusAdjustedForZslope currRadius (slopeAdjustedForVerticalAngle currXSlope currYSlope (xyQuadrantAngle angle)))
      (xyQuadrantAngle angle)
      (slopeAdjustedForVerticalAngle currXSlope currYSlope (xyQuadrantAngle angle))
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
createTopFaces inOrigin inRadius inAngles xSlope ySlope  =
     (createCornerPoint
      (F3)
      inOrigin
      (head inRadius) 
      (radiusAdjustedForZslope (head inRadius) (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle (head inAngles))))
      (xyQuadrantAngle (head inAngles))
      (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle (head inAngles)))
    ) 
    +++
    B3 inOrigin
    ++>
    [(createCornerPoint
      (F2)
      inOrigin
      currRadius
      (radiusAdjustedForZslope currRadius (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle angle)))
      (xyQuadrantAngle angle)
      (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle angle))
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
      (radiusAdjustedForZslope (head inRadius) (slopeAdjustedForVerticalAngle (head xSlope) (head ySlope) (xyQuadrantAngle (head inAngles))))
      (xyQuadrantAngle (head inAngles))
      (slopeAdjustedForVerticalAngle (head xSlope) (head ySlope) (xyQuadrantAngle (head inAngles)))
    ) 
    +++
    B3 inOrigin
    ++>
    [(createCornerPoint
      (F2)
      inOrigin
      currRadius
      (radiusAdjustedForZslope currRadius (slopeAdjustedForVerticalAngle currXSlope currYSlope (xyQuadrantAngle angle)))
      (xyQuadrantAngle angle)
      (slopeAdjustedForVerticalAngle currXSlope currYSlope (xyQuadrantAngle angle))
     ) 
     +++
     B2 inOrigin
       | angle <- tail inAngles
       | currRadius <- tail inRadius
       | currXSlope <- tail xSlope
       | currYSlope <- tail ySlope
    ]


{----------------------------------------- create right faces ----------------------------------------------------------
Known uses:
Scanning
Scanning is done vertically, so it would be best to build the model that way, instead of transforing the data
to fit the horizontal model supplied by 'createBottomFaces'/'createTopFaces'

Create a set of right faces, which will be used as the initial faces, to which all the subsequent left
faces will be added.

Create in a top down direction, as that is the way the openCV data is supplied.
-}

createRightFaces inOrigin inRadius inAngles xSlope ySlope  =
    (createCornerPoint
      (F3)
      inOrigin
      (head inRadius) 
      (radiusAdjustedForZslope (head inRadius) (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle (head inAngles))))
      (xyQuadrantAngle (head inAngles))
      (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle (head inAngles)))
    ) 
    +++
    B3 inOrigin
    ++>
    [(createCornerPoint
      (F4)
      inOrigin
      currRadius
      (radiusAdjustedForZslope currRadius (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle angle)))
      (xyQuadrantAngle angle)
      (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle angle))
     ) 
     +++
     B4 inOrigin
       | angle <- tail inAngles
       | currRadius <- tail inRadius
    ]

{-
additions required
F3 +++ B3
present: y
tested:  y

F4 +++ B4
present: y
tested:  y

TopRightLine +++ BottomRightLine
present: y
tested:  y

RightFace +++ BottomRightLine
present: y
tested:  y

-}
