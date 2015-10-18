{-# LANGUAGE ParallelListComp #-}
module TriCad.MathPolar(
  slopeAdjustedForVerticalAngle,
  adjustRadiusForSlope,
  --quadrantOfAngle, renamed to trigAngle for now
  trigAngle,
  createCornerPoint,
  Slope(..),
  Radius(..),
  SingleDegreeRadii(..),
  MultiDegreeRadii(..),
  flatXSlope,
  flatYSlope,
  Degree(..),
  Point(..),
  Origin(..),
  Angle(..),
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

-- |The center of a radial shape.
type Origin = Point


 

{-|
Represents a radius of a circular shape, which is what all shapes in math polar are created from.
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
data Angle =         Quadrant1Angle  { angle::Double}
                   | Quadrant2Angle  { angle::Double}
                   | Quadrant3Angle  { angle::Double}
                   | Quadrant4Angle  { angle::Double}
                   | Angle           { angle::Double}
                   
  deriving (Show, Eq)


{-
What quadrant of the xy plane, is the angle in?

Used for:
setYPolarityForQuadrant & setXPolarityForQuadrant use it to adjust the polarity of x/y for trig calculations

getCurrentQuadrant :: QuadrantAngle -> Quadrant
getCurrentQuadrant ang       | angle ang  < 0 = getCurrentQuadrant $ Angle (360 + (angleang))
                             | angleang <= 90 = Quadrant1
                             | angleang <= 180 = Quadrant2
                             | angleang <= 270 = Quadrant3
                             | angleang <= 360 = Quadrant4
                             | angleang > 360 = getCurrentQuadrant $ Angle ((angleang) - 360)
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
data Slope = NegSlope {slope :: Double}
           | PosSlope {slope :: Double}
           | NegXSlope {slope :: Double}
           | PosXSlope {slope :: Double}
           | NegYSlope {slope :: Double}
           | PosYSlope {slope :: Double}
           
  deriving (Show, Eq)

-- length of an axis.
--A 3D cartesian point is the result of x,y,z AxisLengths from the origin
type AxisLength = Double

{-
Each quadrant will be 0-90 degrees.
Orientated so that, for all quadrants:
 -use sin to calculate x-axis
 -use cos to calculate y-axis
-}

--quadrantOfAngle :: Double ->  Angle
trigAngle :: Double ->  Angle
trigAngle currAngle 
  | currAngle < 0 = trigAngle (360 - currAngle)
  | currAngle <= 90 = Quadrant1Angle currAngle
  | currAngle <= 180 = Quadrant2Angle (180 - currAngle)
  | currAngle <= 270 = Quadrant3Angle $ currAngle - 180   -- 90 - (270 - currAngle)
  | currAngle <= 360 = Quadrant4Angle (360 - currAngle)
  | currAngle > 360 = trigAngle (currAngle - 360)




{-
Functions for calculating the current Z plane angle. It is a combination of the slope,
and the current XY angle. It has to be calculated because the slope changes as you work around the center.
This change in slope is because cubes radiate from the center, so their endpoints will be at various slopes from the center.
When all done, they should all run together at the target slope.

It is a wrapper around slopeAdjustedForVerticalAngleBase, so that the calculation of the trigAngle only
has to be written out once, instead of in each function.
-}
slopeAdjustedForVerticalAngle :: Slope -> Slope -> Angle -> Slope
slopeAdjustedForVerticalAngle xSlope ySlope (Angle verticalAngle) =
  slopeAdjustedForVerticalAngleBase xSlope ySlope (trigAngle verticalAngle)

slopeAdjustedForVerticalAngleBase :: Slope -> Slope -> Angle -> Slope
slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant1Angle verticalAngle)
  | ((sinDegrees verticalAngle) * xSlope) >= ((cosDegrees verticalAngle) * ySlope)  =
      PosSlope $ abs $ ((sinDegrees verticalAngle) * xSlope) - ((cosDegrees verticalAngle) * ySlope)
  | otherwise = NegSlope $ abs $ ((sinDegrees verticalAngle) * xSlope) - ((cosDegrees verticalAngle) * ySlope)

slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant1Angle verticalAngle) =
  PosSlope $ ((sinDegrees verticalAngle) * xSlope) + ((cosDegrees verticalAngle) * ySlope)

slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (PosYSlope ySlope) (Quadrant1Angle verticalAngle) =
   NegSlope $ (getXSlope xSlope verticalAngle) + (getYSlope ySlope verticalAngle)

--not tested
slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (NegYSlope ySlope) (Quadrant1Angle verticalAngle)
  | (getYSlope ySlope verticalAngle) >= (getXSlope xSlope verticalAngle) = PosSlope $ (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle)
  | otherwise = NegSlope $ (getXSlope xSlope verticalAngle) - (getYSlope ySlope verticalAngle)

slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant2Angle verticalAngle) =
  PosSlope $(getXSlope xSlope verticalAngle) + (getYSlope ySlope verticalAngle)

slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant2Angle verticalAngle)
  | (getXSlope xSlope verticalAngle) >= (getYSlope ySlope verticalAngle)  =
      PosSlope $ abs $ (getXSlope xSlope verticalAngle) - (getYSlope ySlope verticalAngle)
  | otherwise = NegSlope $ abs $ (getXSlope xSlope verticalAngle) - (getYSlope ySlope verticalAngle)

--not tested
slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (PosYSlope ySlope) (Quadrant2Angle verticalAngle)
  | (getYSlope ySlope verticalAngle) <= (getXSlope xSlope verticalAngle) = PosSlope $ (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle)
  | otherwise = NegSlope $ (getXSlope xSlope verticalAngle) - (getYSlope ySlope verticalAngle)

--not tested
slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (NegYSlope ySlope) (Quadrant2Angle verticalAngle) =
  NegSlope $ (getXSlope xSlope verticalAngle) + (getYSlope ySlope verticalAngle)

slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant3Angle verticalAngle)
  | (getYSlope ySlope verticalAngle)  >= (getXSlope xSlope verticalAngle) =
      PosSlope $ abs $  (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle)
  | otherwise = NegSlope $ abs $ (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle) 

slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant3Angle verticalAngle) =
  NegSlope $ ((sinDegrees verticalAngle) * xSlope) + ((cosDegrees verticalAngle) * ySlope)

--not tested
slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (PosYSlope ySlope) (Quadrant3Angle verticalAngle)  =
  PosSlope $ (getXSlope xSlope verticalAngle) + (getYSlope ySlope verticalAngle)

--not tested
slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (NegYSlope ySlope) (Quadrant3Angle verticalAngle)
  | (getXSlope xSlope verticalAngle) >= (getYSlope ySlope verticalAngle) = PosSlope $ (getXSlope xSlope verticalAngle) - (getYSlope ySlope verticalAngle)
  | otherwise = NegSlope $ (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle)

slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (PosYSlope ySlope) (Quadrant4Angle verticalAngle) =
  NegSlope $ (getXSlope xSlope verticalAngle) + (getYSlope ySlope verticalAngle)

slopeAdjustedForVerticalAngleBase (PosXSlope xSlope) (NegYSlope ySlope) (Quadrant4Angle verticalAngle)
  | (getYSlope ySlope verticalAngle)  >= (getXSlope xSlope verticalAngle) =
      PosSlope $ abs $  (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle)
  | otherwise = NegSlope $ abs $ (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle)

--not tested
slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (PosYSlope ySlope) (Quadrant4Angle verticalAngle)
  | (getXSlope xSlope verticalAngle) >= (getYSlope ySlope verticalAngle) = PosSlope $ (getXSlope xSlope verticalAngle) - (getYSlope ySlope verticalAngle)
  | otherwise = NegSlope $ (getYSlope ySlope verticalAngle) - (getXSlope xSlope verticalAngle)

--not tested
slopeAdjustedForVerticalAngleBase (NegXSlope xSlope) (NegYSlope ySlope) (Quadrant4Angle verticalAngle) =
  PosSlope $ (getXSlope xSlope verticalAngle) + (getYSlope ySlope verticalAngle)
   
getXSlope xSlope' verticalAngle' = ((sinDegrees verticalAngle') * xSlope')
getYSlope ySlope' verticalAngle' = ((cosDegrees verticalAngle') * ySlope')


{-
Shorten the radius on the xy plane, for the changes in the z plane.
As per standard 3D polar to cartesian conversion methods.
-}
adjustRadiusForSlope :: Radius -> Slope -> Radius 
adjustRadiusForSlope (Radius radius) (PosSlope xySlope) = UpRadius $ radius * (cosDegrees (xySlope))
adjustRadiusForSlope (Radius radius) (NegSlope xySlope) = DownRadius $ radius * (cosDegrees (xySlope))


  
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

createCornerPoint :: (Point-> CornerPoints) -> Origin -> Radius ->  Angle -> Slope -> Slope -> CornerPoints
createCornerPoint cPoint origin horizRadius verticalAngle xSlope ySlope  =
                             let 
                                 
                                 --What quadrant of the xy plane, is the angle in?
                                 getCurrentQuadrant :: Angle -> Quadrant
                                 getCurrentQuadrant ang       | angle ang  < 0 = getCurrentQuadrant $ Angle (360 + (angle ang))
                                                              | angle ang <= 90 = Quadrant1
                                                              | angle ang <= 180 = Quadrant2
                                                              | angle ang <= 270 = Quadrant3
                                                              | angle ang <= 360 = Quadrant4
                                                              | angle ang > 360 = getCurrentQuadrant $ Angle ((angle ang) - 360)



                                 currentSlope = slopeAdjustedForVerticalAngle xSlope ySlope verticalAngle
                                                                                      
                                 radiusAdjustedForSlope = radius (adjustRadiusForSlope horizRadius currentSlope)

                                 baseOfAngle = (angle $ trigAngle (angle verticalAngle))
                                 sinOfVerticalAngle = sinDegrees baseOfAngle
                                 cosOfVerticalAngle = cosDegrees baseOfAngle
                                 
                                 
                                 setXaxis =
                                   let length = radiusAdjustedForSlope * sinOfVerticalAngle
                                       x_axis' = x_axis origin
                                   in

                                    case getCurrentQuadrant verticalAngle of
                                      Quadrant1 -> x_axis' + length
                                      Quadrant2 -> x_axis' + length
                                      Quadrant3 -> x_axis' - length
                                      Quadrant4 -> x_axis' - length
                                 
                                 
                                 setYaxis' =
                                   let length = radiusAdjustedForSlope * cosOfVerticalAngle
                                       y_axis' = y_axis origin
                                   in

                                    case getCurrentQuadrant verticalAngle of
                                      Quadrant1 -> y_axis' - length
                                      Quadrant2 -> y_axis' + length
                                      Quadrant3 -> y_axis' + length
                                      Quadrant4 -> y_axis' - length

                                 
                                 setZaxis =
                                   let length = (radius horizRadius) * (sinDegrees (slope currentSlope))
                                       z_axis' = z_axis origin
                                   in
                                    case currentSlope of
                                     (PosSlope _) -> z_axis' +  length
                                     otherwise  -> z_axis' - length
                                 
                                 
                             in       
                                 cPoint (Point setXaxis setYaxis' setZaxis)

                                  
 





