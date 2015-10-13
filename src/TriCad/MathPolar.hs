{-# LANGUAGE ParallelListComp #-}
module TriCad.MathPolar(
  slopeAdjustedForVerticalAngle,
  createTopFaces,
  createBottomFaces,
  createRightFaces,
  createLeftFaces,
  createVerticalCubes,
  createLeftFacesMultiColumns,
  createTopFacesWithVariableSlope,
  createBottomFacesWithVariableSlope,
  radiusAdjustedForZslope,
  xyQuadrantAngle,
  createCornerPoint,
  createCornerPointSimplified,
  Slope(..),
  Radius(..),
  SingleDegreeRadii(..),
  MultiDegreeRadii(..),
  flatXSlope,
  flatYSlope,
  Angle(..),
  --setYPolarityForQuadrant,
  --setXPolarityForQuadrant,
  Degree(..),
  ) where
import TriCad.Points(Point(..))
import TriCad.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import TriCad.Math(sinDegrees, cosDegrees)
import TriCad.CornerPointsTranspose (transposeZ)

{--------------------overview----------------------------------------
Creates a radial shape using polar cood's.

0 degrees is as the max neg y axis, and rotates clockwise into the pos x axis.

For some reason, it has a slight drift. Eg: At 90 degrees, the y axis should be 0,
but it drifts off the y-axis.

safari books: Triginometry 3rd edition is a good ref.
-}

{---------------------------------------------------------------------------
e

Functions for calculating the current x and y values adjusted for xy degrees


-}

-- |Many shapes, if not most, do not have sloped tops, therefore it is handy
-- to have easily callable no-slope values.
flatXSlope = PosXSlope 0 
flatYSlope = PosYSlope 0

-- |Degree of a circle.
type Degree = Double
 

{-|
Represents a radius, which is what all shapes in math polar, are creates from.


Known uses:
-Everywhere that a CornerPoints is made via MathPolar.


-Scan.Parse.Raw uses it for parsing raw image data into json.

-Scan.Json uses it for encoding/decoding scanner data.
 It is made an instance of ToJSON and FromJSON in Scan.Json module.
 Allows it to be stored as json data, so the raw data only has to be processed once.

-}
data Radius = Radius {radius :: Double}
             | DownRadius {radius :: Double} -- | Radius slopes down from origin z-axis. 
             | UpRadius {radius :: Double}
   deriving (Show, Eq)

{-
Contains the [Radius] associated with a single degree from a vertical scan.

Scan.Json module declares it an instance of ToJSON and FromJSON for the aeson package.

Known uses:
Raw image data is parse into Scan datatype, which contains [Degree]. This is then
processed into cubes.

Store the processed raw data as json, so the processing only has to be done once.
-}
data SingleDegreeRadii = SingleDegreeRadii {degree::Degree, radii::[Radius]}
     deriving (Show, Eq)

{- |
Contains all the filtered data from a scan.
Is a [SingleDegreeRadii] and an assoc'd name.

Known uses:
Raw scan image data is processed into this, which is the last ADT, before being turned in CornerPoints.
It can be read to/from json, so that all the processing of scan data, can be saved to file.
-}
data MultiDegreeRadii = MultiDegreeRadii {name::String, degrees::[SingleDegreeRadii]}
          deriving (Show, Eq)
{-
There are 4 quadrants to work with therfore the Quadarant1/2/3/4Angle
They should be eliminated later, when I use quadrant correcting as per trig rules for obtuse angles.

Angle:
This will be the replacement for all of the others. It is simply a wrapper around Double.
Once the others are gone, should make it a newtype, for efficiency.
-}
data Angle =         Quadrant1Angle  { quadAngle::Double}
                   | Quadrant2Angle  { quadAngle::Double}
                   | Quadrant3Angle  { quadAngle::Double}
                   | Quadrant4Angle  { quadAngle::Double}
                   | Angle           { quadAngle::Double}
                   
  deriving (Show, Eq)


{-
What quadrant of the xy plane, is the angle in?

Used for:
setYPolarityForQuadrant & setXPolarityForQuadrant use it to adjust the polarity of x/y for trig calculations

getCurrentQuadrant :: QuadrantAngle -> Quadrant
getCurrentQuadrant ang       | quadAngle ang  < 0 = getCurrentQuadrant $ Angle (360 + (quadAngle ang))
                             | quadAngle ang <= 90 = Quadrant1
                             | quadAngle ang <= 180 = Quadrant2
                             | quadAngle ang <= 270 = Quadrant3
                             | quadAngle ang <= 360 = Quadrant4
                             | quadAngle ang > 360 = getCurrentQuadrant $ Angle ((quadAngle ang) - 360)
-}
{-
Corresponds to the xy plane quadrants.

Used for:
getCurentQuadrent returns this type when it calculates the current quadrant for an angle.

-}
data Quadrant = Quadrant1 -- neg y, pos x
              | Quadrant2 -- pos y, pos x
              | Quadrant3 -- pos y, neg x
              | Quadrant4 -- neg y, neg x

{-
Used for:
Neg/Pos XY Slope: 
-Describe the slope resulting from the combining of the Neg/Pos X/Y slopes
-Neg slopes downwards from the neg x/y quadrants
-Pos slopes upwards from the neg x/y quadrants

The NegYSlope and PosYSlope describe a slope(change of z axis) along the y axis.
NegYSlope:
-in quadrants 1 & 4, which are the -y quads, slope upwards from the origin to the front faces.
-in quads 2 & 3, the + y quads, slope downwards from the origin to the front faces.

PosYSlope:
-in quads 1 & 4 , the - y quads, slope from the origin, downwards to the front faces.
-in quads 2 & 3, the  + y quads, slope from the origin, updwards to the front faces.

The NegXSlope and PosXSlope describe a slope(change of z axis) along the x axis.
NegXSlope
-in quads 1 & 2, the + x quads, slope from the origin, downwards to the front faces.
-in quads 3 & 4, the - x quads, slope from the origin, upwards to the front faces.

PosXSlope
-in quads 1 & 2, the + x quads, slope from the origin, upwards to the front faces.
-in quads 3 & 4, the - x quads, slope from the origin, downwards to the front faces.
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
xyQuadrantAngle :: Double ->  Angle
xyQuadrantAngle currAngle 
  | currAngle < 0 = xyQuadrantAngle (360 - currAngle)
  | currAngle <= 90 = Quadrant1Angle currAngle
  | currAngle <= 180 = Quadrant2Angle (180 - currAngle)
  | currAngle <= 270 = Quadrant3Angle $ currAngle - 180   -- 90 - (270 - currAngle)
  | currAngle <= 360 = Quadrant4Angle (360 - currAngle)
  | currAngle > 360 = xyQuadrantAngle (currAngle - 360)



{-
Functions for calculating the current Z plane angle. It is a combination of the slope,
and the current XY angle. It has to be calculated because the slope changes as you work around the center.
This change in slope is because cubes radiate from the center, so their endpoints will be at various slopes from the center.
When all done, they should all run together at the target slope.

They also have to take into account, what quadrant they are in, as it affects the angles used with the trig.
This could be done as a separate function, which would pre-adjust the angle, or pass it in with the slope curried in.

should this return a type Angle = NegZAngle PosZAngle then continue on with testing  


-}

slopeAdjustedForVerticalAngle :: Slope -> Slope -> Angle -> Slope
slopeAdjustedForVerticalAngle (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant1Angle xyAngle)
  | ((sinDegrees xyAngle) * xSlope) >= ((cosDegrees xyAngle) * ySlope)  = PosXYSlope $ abs $ ((sinDegrees xyAngle) * xSlope) - ((cosDegrees xyAngle) * ySlope)
  | otherwise = NegXYSlope $ abs $ ((sinDegrees xyAngle) * xSlope) - ((cosDegrees xyAngle) * ySlope)

slopeAdjustedForVerticalAngle (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant1Angle xyAngle) =
  PosXYSlope $ ((sinDegrees xyAngle) * xSlope) + ((cosDegrees xyAngle) * ySlope)

slopeAdjustedForVerticalAngle (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant2Angle xyAngle) =
  PosXYSlope $ ((sinDegrees xyAngle) * xSlope) + ((cosDegrees xyAngle) * ySlope)

slopeAdjustedForVerticalAngle (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant2Angle xyAngle)
  | ((sinDegrees xyAngle) * xSlope) >= ((cosDegrees xyAngle) * ySlope)  = PosXYSlope $ abs $ ((sinDegrees xyAngle) * xSlope) - ((cosDegrees xyAngle) * ySlope)
  | otherwise = NegXYSlope $ abs $ ((sinDegrees xyAngle) * xSlope) - ((cosDegrees xyAngle) * ySlope)

-- ySlope is >= xSlope so it is a PosXYSlope
slopeAdjustedForVerticalAngle (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant3Angle xyAngle)
  | ((cosDegrees xyAngle) * ySlope)  >= ((sinDegrees xyAngle) * xSlope) = PosXYSlope $ abs $  ((cosDegrees xyAngle) * ySlope) - ((sinDegrees xyAngle) * xSlope)
  --xSlope is >  ySlope so it is a NegXYSlope
  | otherwise = NegXYSlope $ abs $ ((cosDegrees xyAngle) * ySlope) - ((sinDegrees xyAngle) * xSlope) 

slopeAdjustedForVerticalAngle (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant3Angle xyAngle) =
  NegXYSlope $ ((sinDegrees xyAngle) * xSlope) + ((cosDegrees xyAngle) * ySlope)

slopeAdjustedForVerticalAngle (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant4Angle xyAngle) =
  NegXYSlope $ ((sinDegrees xyAngle) * xSlope) + ((cosDegrees xyAngle) * ySlope)

--ySlope is >= xSlope so it is a PosXYSlope
slopeAdjustedForVerticalAngle (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant4Angle xyAngle)
  | ((cosDegrees xyAngle) * ySlope)  >= ((sinDegrees xyAngle) * xSlope) = PosXYSlope $ abs $  ((cosDegrees xyAngle) * ySlope) - ((sinDegrees xyAngle) * xSlope)
  --xSlope is >  ySlope so it is a NegXYSlope
  | otherwise = NegXYSlope $ abs $ ((cosDegrees xyAngle) * ySlope) - ((sinDegrees xyAngle) * xSlope) 

                                                                                        

{-
Shorten the radius on the xy plane, for the changes in the z plane.
As per standard 3D polar to cartesian conversion methods.
-}
radiusAdjustedForZslope :: Radius -> Slope -> Radius 
radiusAdjustedForZslope (Radius radius) (PosXYSlope xySlope) = UpRadius $ radius * (cosDegrees (xySlope))
radiusAdjustedForZslope (Radius radius) (NegXYSlope xySlope) = DownRadius $ radius * (cosDegrees (xySlope))


  
{-
For trig xy calculations, the neg/pos aspect of y will depend upon the current quadrant of the xy plane


setYPolarityForQuadrant :: QuadrantAngle -> Double -> Double
setYPolarityForQuadrant angle val = case getCurrentQuadrant angle of
                                     Quadrant1 -> negate val
                                     Quadrant2 -> val
                                     Quadrant3 -> val
                                     Quadrant4 -> negate val
-}
        

{-
Change slope and xy angle into a single Slope value.
Change radius into a Radius

Given:
cPoint : (Point-> CornerPoints)
-A CornerPoints constructor such as F1

adjustedRadius: Radius: 
-The radius, after being adjusted for slope
-Should get rid of this, and do the calculations inside createCornerPoint.
 This would simplify calling this function.
 It was used before for pattern matching, but that is no longer done.
 However, will need to pass in xSlope and ySlope, instead of just the xySlope

slope: Slope:
The slope, having already been adjusted for x/y slopes and xy quadrant

orign: Point
-The starting point which gets adjusted to give the return point. That way, this point can be created relative to some position, instead of at an origin of 0 0 0

-}
createCornerPoint :: (Point-> CornerPoints) -> Point -> Radius -> Radius ->  Angle -> Slope -> CornerPoints
createCornerPoint cPoint origin (Radius horizRadius) (adjustedRadius)  (Angle xyAngle) (slope) =
                             let setXPolarityForQuadrant :: Angle -> Double -> Double
                                 setXPolarityForQuadrant angle val = case getCurrentQuadrant angle of
                                     Quadrant1 -> val
                                     Quadrant2 -> val
                                     Quadrant3 -> negate val
                                     Quadrant4 -> negate val
                                     
                                 setYPolarityForQuadrant :: Angle -> Double -> Double
                                 setYPolarityForQuadrant angle val = case getCurrentQuadrant angle of
                                     Quadrant1 -> negate val
                                     Quadrant2 -> val
                                     Quadrant3 -> val
                                     Quadrant4 -> negate val

                                 setZPolarityForQuadrant :: Slope -> Double -> Double
                                 --all math polar tests pass, but the keen heel sole adaptor slopes are nfg.
                                 setZPolarityForQuadrant slope val =
                                   case slope of
                                     (PosXYSlope _) ->  val
                                     otherwise  -> negate val


                                 
                                 --What quadrant of the xy plane, is the angle in?
                                 getCurrentQuadrant :: Angle -> Quadrant
                                 getCurrentQuadrant ang       | quadAngle ang  < 0 = getCurrentQuadrant $ Angle (360 + (quadAngle ang))
                                                              | quadAngle ang <= 90 = Quadrant1
                                                              | quadAngle ang <= 180 = Quadrant2
                                                              | quadAngle ang <= 270 = Quadrant3
                                                              | quadAngle ang <= 360 = Quadrant4
                                                              | quadAngle ang > 360 = getCurrentQuadrant $ Angle ((quadAngle ang) - 360)
                                     
                             in       
                                 cPoint (Point 
                                    (--x:
                                     x_axis origin + (setXPolarityForQuadrant (Angle xyAngle) $
                                      (radius adjustedRadius) * (sinDegrees  (quadAngle $ xyQuadrantAngle xyAngle))))--tested good
                                    
                                    
                                    (--y:
                                     y_axis origin + (setYPolarityForQuadrant (Angle xyAngle) $
                                       (radius adjustedRadius) * (cosDegrees  (quadAngle $ xyQuadrantAngle  xyAngle))))-- tested good
                                    
                                    (--z:
                                      case slope of
                                        NegXYSlope slope' -> z_axis origin + (setZPolarityForQuadrant (NegXYSlope slope') (horizRadius * (sinDegrees (slope'))))
                                        PosXYSlope slope' -> z_axis origin + (setZPolarityForQuadrant (PosXYSlope slope') (horizRadius * (sinDegrees (slope'))))
                                    )
                                  )


createCornerPointSimplified :: (Point-> CornerPoints) -> Point -> Radius ->  Angle -> Slope -> Slope -> CornerPoints
createCornerPointSimplified cPoint origin horizRadius xyAngle xSlope ySlope  =
                             let setXPolarityForQuadrant :: Angle -> Double -> Double
                                 setXPolarityForQuadrant angle val = case getCurrentQuadrant angle of
                                     Quadrant1 -> val
                                     Quadrant2 -> val
                                     Quadrant3 -> negate val
                                     Quadrant4 -> negate val
                                     
                                 setYPolarityForQuadrant :: Angle -> Double -> Double
                                 setYPolarityForQuadrant angle val = case getCurrentQuadrant angle of
                                     Quadrant1 -> negate val
                                     Quadrant2 -> val
                                     Quadrant3 -> val
                                     Quadrant4 -> negate val

                                 setZPolarityForQuadrant :: Slope -> Double -> Double
                                 --all math polar tests pass, but the keen heel sole adaptor slopes are nfg.
                                 setZPolarityForQuadrant slope val =
                                   case slope of
                                     (PosXYSlope _) ->  val
                                     otherwise  -> negate val


                                 
                                 --What quadrant of the xy plane, is the angle in?
                                 getCurrentQuadrant :: Angle -> Quadrant
                                 getCurrentQuadrant ang       | quadAngle ang  < 0 = getCurrentQuadrant $ Angle (360 + (quadAngle ang))
                                                              | quadAngle ang <= 90 = Quadrant1
                                                              | quadAngle ang <= 180 = Quadrant2
                                                              | quadAngle ang <= 270 = Quadrant3
                                                              | quadAngle ang <= 360 = Quadrant4
                                                              | quadAngle ang > 360 = getCurrentQuadrant $ Angle ((quadAngle ang) - 360)



                                 adjustedSlope = slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle (quadAngle xyAngle))
                                                                                      
                                 adjustedRadius = (radiusAdjustedForZslope horizRadius adjustedSlope)
                                 
                             in       
                                 cPoint (Point 
                                    (--x:
                                     x_axis origin + (setXPolarityForQuadrant xyAngle $
                                      (radius adjustedRadius) * (sinDegrees  (quadAngle $ xyQuadrantAngle (quadAngle xyAngle)))))--tested good
                                    
                                    
                                    (--y:
                                     y_axis origin + (setYPolarityForQuadrant xyAngle $
                                       (radius adjustedRadius) * (cosDegrees  (quadAngle $ xyQuadrantAngle  (quadAngle xyAngle)))))-- tested good

                                    (--z:
                                       case  (adjustedSlope) of
                                        NegXYSlope slope' -> z_axis origin + (setZPolarityForQuadrant (NegXYSlope slope') ((radius horizRadius) * (sinDegrees (slope'))))
                                        PosXYSlope slope' -> z_axis origin + (setZPolarityForQuadrant (PosXYSlope slope') ((radius horizRadius) * (sinDegrees (slope'))))
                                    )
                                  )
 
{- |Create a single CornerPoints.BottomRightLine, and add it to the head of [BottomLeftLine]. This will give a BottomFace.
Then add the next BottomLeftLine to this BottomFace to get the next BottomFace. Continue through the [BottomLeftLine] till
the [BottomFace] is done.

All Lines are created from the inputs, using Polar math.
QuadrantAngle
-}

createBottomFaces :: Point -> [Radius] -> [Angle] -> Slope -> Slope -> [CornerPoints]
createBottomFaces inOrigin radii angles xSlope ySlope  =
    (createCornerPointSimplified
      (F4)
      inOrigin
      (head radii)
      (head angles)
      xSlope
      ySlope
    ) 
    +++
    B4 inOrigin
    ++>
    [(createCornerPointSimplified
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


{-
createBottomFacesWithVariableSlopeOrig :: Point -> [Radius] -> [Double] -> [Slope] -> [Slope] -> [CornerPoints]
createBottomFacesWithVariableSlopeOrig inOrigin inRadius inAngles xSlope ySlope  =
    (createCornerPoint
      (F4)
      inOrigin
      (head inRadius) 
      (radiusAdjustedForZslope (head inRadius) (slopeAdjustedForVerticalAngle (head xSlope) (head ySlope) (xyQuadrantAngle (head inAngles))))
      (Angle (head inAngles))
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
      (Angle angle)
      (slopeAdjustedForVerticalAngle currXSlope currYSlope (xyQuadrantAngle angle))
     ) 
     +++
     B1 inOrigin
       | angle <- tail inAngles
       | currRadius <- tail inRadius
       | currXSlope <- tail xSlope
       | currYSlope <- tail ySlope
    ]

-}
--the new simplfied
createBottomFacesWithVariableSlope :: Point -> [Radius] -> [Angle] -> [Slope] -> [Slope] -> [CornerPoints]
createBottomFacesWithVariableSlope inOrigin inRadius inAngles xSlope ySlope  =
    (createCornerPointSimplified
      (F4)
      inOrigin
      (head inRadius) 
      (head inAngles)
      (head xSlope)
      (head ySlope)
    ) 
    +++
    B4 inOrigin
    ++>
    [(createCornerPointSimplified
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
createTopFaces inOrigin inRadius inAngles xSlope ySlope  =
     (createCornerPoint
      (F3)
      inOrigin
      (head inRadius) 
      (radiusAdjustedForZslope (head inRadius) (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle (head inAngles))))
      (Angle (head inAngles))
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
      (Angle angle)
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
      (Angle (head inAngles))
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
      (Angle angle)
      (slopeAdjustedForVerticalAngle currXSlope currYSlope (xyQuadrantAngle angle))
     ) 
     +++
     B2 inOrigin
       | angle <- tail inAngles
       | currRadius <- tail inRadius
       | currXSlope <- tail xSlope
       | currYSlope <- tail ySlope
    ]





----------------------------------------- create left/right faces from Scan datatype------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{----------------------------------------- create left/right faces, no Scan datatype----------------------------------------------------
This is a base that gets called by createLeftFaces and createRightFaces, with just a different set of CornerPoints constructors.
It uses [[Radius]] and [degrees], instead of the new Scan datatype. Once the Scan datatype can be used, this will be removed.

Build [Left/RightFaces], from an array of Radius that runs downwards along
a single degree. This is the way that the scanner process the images, and gives them to TriCad.
If the data is supplied in an upwards direction, reverse the data, then use this same function, instead of
building a separate function for that purpose.

The vertical distance of each height, is the same for all pixels. Therefore, the height of each pixel
is a factor of location in the array. So to calculate the z_axis(height), an array is passed in which
will be zipped with the RightFaces after they have been created, using TransposeZ.
It would be more efficient to calculate it as the Faces are made up, but then would not be able to re-use
the createCornerPoint function.


Known uses:
Scanning
Scanning is done vertically, so it would be best to build the model that way, instead of transforing the data
to fit the horizontal model supplied by 'createBottomFaces'/'createTopFaces'

Create a set of right faces, which will be used as the initial faces, to which all the subsequent left
faces will be added via ++>. This is used to build both right/left faces.

Create in a top down direction, as that is the way the openCV data is supplied.

------------ given -----------
topOrigin:
-Point that gives the inner origin for the topmost face caculated. This is also the first Radius.

degree:
-The current degree on xy axis. Should be 0.

xSlope:
-x-axis slope on the top of the shape

ySlope:
-y-axis slope on the top of the shape

zTransposeFactor:
-An array of the heights(z_axis value) associated with each face. This value will be subtracted
 from the topOrigin z_axis.

inRadius:
-The array of Radius, which is the value read in from the file, as was calculated openCV.
 This is the location of the target value, in pixels. It needs to be translated into a distance.

---------returns ----------
An array of LeftFace or RightFace depending on data constructors passed in.
-}



createVerticalFaces :: Point -> SingleDegreeRadii -> Slope -> Slope -> [Double] -> (Point-> CornerPoints) ->
                       (Point-> CornerPoints) -> (Point-> CornerPoints) -> (Point-> CornerPoints) -> [CornerPoints]
createVerticalFaces topOrigin inDegree xSlope ySlope zTransposeFactor topFrontConstructor topBackConstructor
                            btmFrontConstructor btmBackConstructor =
  
  --zipWith  (\x y -> transposeZ ((-x)+) y  ) --negating x causes the z_axis to decrease from the top, as it should.
  -- zTransposeFactor --this should be an infinit of the height for each pixel, measured from topOrigin.
  
   ((createCornerPoint
      (topFrontConstructor)
      topOrigin
      (head $ radii inDegree) 
      (radiusAdjustedForZslope (head $ radii inDegree) (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle (degree inDegree))))
      (Angle (degree inDegree))
      (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle (degree inDegree)))
    ) 
    +++
    topBackConstructor topOrigin
    ++>
    [(createCornerPoint
      (btmFrontConstructor)
      (transposeZ (+(-currZVal)) topOrigin)  --topOrigin
      currRadius
      (radiusAdjustedForZslope currRadius (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle (degree inDegree))))
      (Angle (degree inDegree))
      (slopeAdjustedForVerticalAngle xSlope ySlope (xyQuadrantAngle (degree inDegree)))
     ) 
     +++
     btmBackConstructor (transposeZ (+(-currZVal)) topOrigin)  --topOrigin
       | currRadius <- tail $ radii inDegree
       | currZVal <-  tail zTransposeFactor
    ]
   )


--RightFace version of createVerticalFaces
createRightFaces :: Point -> SingleDegreeRadii -> Slope -> Slope -> [Double] -> [CornerPoints]
createRightFaces topOrigin inDegree xSlope ySlope zTransposeFactor  =
  createVerticalFaces topOrigin inDegree xSlope ySlope zTransposeFactor (F3) (B3) (F4) (B4)


--LeftFace version of createVerticalFaces
createLeftFaces :: Point -> SingleDegreeRadii -> Slope -> Slope -> [Double] -> [CornerPoints]
createLeftFaces topOrigin inDegree xSlope ySlope zTransposeFactor  =
  createVerticalFaces topOrigin inDegree xSlope ySlope zTransposeFactor (F2) (B2) (F1) (B1)



{-
Can already create LeftFace, problem is to create an array of them from the vertical [Radius].
Cannot just map over them with createLeftFaces, because the degree is changing from column to column.
For this reason, will have to use recursion.
-}
createLeftFacesMultiColumns ::  Point -> [SingleDegreeRadii] -> Slope -> Slope -> [Double] -> [[CornerPoints]]
createLeftFacesMultiColumns _ [] _ _ _ = []
createLeftFacesMultiColumns topOrigin (d:ds) xSlope ySlope zTransposeFactor =
  (createLeftFaces topOrigin d xSlope ySlope zTransposeFactor ) :
    (createLeftFacesMultiColumns topOrigin ds xSlope ySlope zTransposeFactor)



{-
Join a [RightFace] to [[LeftFace]] using a ++>
Normally: RightFace ++> [LeftFaces] so need recursion to work through the extra level of lists.

I stopped the test for now, till I can create a [[LeftFace]].
At this point I can only create a[LeftFace]
-}
createVerticalCubes :: [CornerPoints] -> [[CornerPoints]] -> [CornerPoints]
createVerticalCubes ([]) _ = []
createVerticalCubes (x:xs) (ys) =
  let headOfLeftFaces = map (head) ys
  in (x ++> headOfLeftFaces) ++  (createVerticalCubes xs (map (tail) ys) )
