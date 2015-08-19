{-# LANGUAGE ParallelListComp #-}
module Tests.MathPolarTest (mathPolarTestDo) where
import Test.HUnit
import TriCad.CornerPoints(CornerPoints(..), (+++))
import TriCad.Points(Point(..))
import TriCad.MathPolar(
  slopeAdjustedForVerticalAngle,
  radiusAdjustedForZslope,
  Radius(..),
  xyQuadrantAngle,
  createCornerPoint,
  Slope(..),
  QuadrantAngle(..),
  setYPolarityForQuadrant,
  setXPolarityForQuadrant,
  )
import TriCad.Math(sinDegrees,cosDegrees)


mathPolarTestDo = do

  putStrLn "internal support functions" 

  runTestTT getQuadrantAngleTest
  runTestTT getQuadrantAngleTest2
  runTestTT getQuadrantAngleTest3
  runTestTT getQuadrantAngleTest4
  runTestTT getQuadrantAngleTest5
  runTestTT getQuadrantAngleTest6
  runTestTT getQuadrantAngleTest7

  runTestTT slopeForXYAngleAndYslopeTestXPos10YPos1XY90
  runTestTT slopeForXYAngleAndYslopeTestX0Ypos0XY0
  runTestTT slopeForXYAngleAndYslopeTestX0Ypos10XY10
  runTestTT slopeForXYAngleAndYslopeTestXPos1Ypos10XY10
  runTestTT slopeForXYAngleAndYslopeTestXPos10Ypos1XY10
  runTestTT slopeForXYAngleAndYslopeTestXPos10Yneg1XY10
  runTestTT slopeForXYAngleAndYslopeTestXPos1Yneg10XY10
  runTestTT slopeForXYAngleAndYslopeTestXPos1YPos10XY80
  runTestTT slopeForXYAngleAndYslopeTestXPos10YPos1XY80
  runTestTT slopeForXYAngleAndYslopeTestXPos10Yneg1XY80
  runTestTT slopeForXYAngleAndYslopeTestXPos1Yneg10XY80
  runTestTT slopeForXYAngleAndYslopeTestXPos1YPos10XY100
  runTestTT slopeForXYAngleAndYslopeTestXPos10YPos1XY100
  runTestTT slopeForXYAngleAndYslopeTestXPos1YNeg10XY100
  runTestTT slopeForXYAngleAndYslopeTestXPos1YPos10XY170
  runTestTT slopeForXYAngleAndYslopeTestXPos10YPos1XY170
  runTestTT slopeForXYAngleAndYslopeTestXPos0YNeg1XY170
  runTestTT slopeForXYAngleAndYslopeTestXPos1YNeg1XY170
  runTestTT slopeForXYAngleAndYslopeTestXPos1YPos10XY190
  runTestTT slopeForXYAngleAndYslopeTestXPos10YPos1XY190
  runTestTT slopeForXYAngleAndYslopeTestXPos1YNeg10XY190
  runTestTT slopeForXYAngleAndYslopeTestXPos1YPos10XY260
  runTestTT slopeForXYAngleAndYslopeTestXPos10YPos1XY260
  runTestTT slopeForXYAngleAndYslopeTestXPos1YPos10XY280
  runTestTT slopeForXYAngleAndYslopeTestXPos10YPos1XY280
  runTestTT slopeForXYAngleAndYslopeTestXPos1YNeg10XY280
  runTestTT slopeForXYAngleAndYslopeTestXPos10YNeg1XY280
  runTestTT slopeForXYAngleAndYslopeTestXPos1YPos10XY350
  runTestTT slopeForXYAngleAndYslopeTestXPos10YPos1XY350
  runTestTT slopeForXYAngleAndYslopeTestNeedMoreTesting
 
  
  runTestTT radiusAdjustedForZslopeTestRad10PosX10PosY0XY10
  runTestTT radiusAdjustedForZslopeTestRad10PosX0PosY10XY10
  runTestTT radiusAdjustedForZslopeTestRad10PosX1NegY10XY10
  runTestTT radiusAdjustedForZslopeTestRad10PosX10NegY1XY10
  runTestTT radiusAdjustedForZslopeTestRad10PosX1PosY10XY80
  runTestTT radiusAdjustedForZslopeTestRad10PosX1PosY10XY100
  runTestTT radiusAdjustedForZslopeTestRad10PosX10NegY1XY100
  runTestTT radiusAdjustedForZslopeTestRad10PosX1NegY10XY100
  runTestTT radiusAdjustedForZslopeTestRad10PosX1PosY10XY170
  runTestTT radiusAdjustedForZslopeTestRad10PosX10NegY1XY170 
  runTestTT radiusAdjustedForZslopeTestRad10PosX1PosY10XY190 
  runTestTT radiusAdjustedForZslopeTestRad10PosX10PosY1XY190
  runTestTT radiusAdjustedForZslopeTestRad10PosX1PosY10XY260
  runTestTT radiusAdjustedForZslopeTestRad10PosX10PosY1XY260
  runTestTT radiusAdjustedForZslopeTestRad10PosX1PosY10XY280 
  runTestTT radiusAdjustedForZslopeTestRad10PosX10PosY1XY280
  runTestTT radiusAdjustedForZslopeTestRad10PosX1PosY10XY350
  runTestTT radiusAdjustedForZslopeTestRad10PosX10NegY1XY190
  runTestTT radiusAdjustedForZslopeTestNeedMoreTesting 
  runTestTT radiusAdjustedForZslopeTestRad10PosX1NegY1oXY190

  {-Have a look at the bottom front right corner first, as this is the first corner gen'd.-}
  putStrLn "\n\n"
  putStrLn "bottomFrontRightCorner"
  runTestTT createCornerPointTestR10PosX0PosY0XY10
  runTestTT createCornerPointTestR10PosX0PosY10XY10
  runTestTT createCornerPointTestR10PosX0PosY10XY80
  runTestTT createCornerPointTestR10PosX1PosY10XY100
  runTestTT createCornerPointTestR10PosX1PosY10XY170
  runTestTT createCornerPointTestR10PosX10PosY1XY170
  runTestTT createCornerPointTestR10PosX1PosY10XY190 
  runTestTT createCornerPointTestR10PosX1PosY10XY260
  --last round of patterns
  runTestTT createCornerPointTestR10PosX1PosY10XY280 
  runTestTT createCornerPointTestR10PosX1PosY10XY350
  runTestTT createCornerPointTestR10PosX1NegY10XY190
  runTestTT createCornerPointTestR10PosX1NegY10XY170
  runTestTT createFrontCornerTest

   --following 2 have not been calc'd/verified the output, though they look good in netfabb
  --Leave them till all createCornerPoints changes are done
  runTestTT createCornerPoint10DegTestSlopedMaleLegos
  runTestTT createCornerPointTest90DegSlopedMaleLegos

  
  {-This will test all the layer A F1 corners-}
  putStrLn "\n\n" 
  putStrLn "bottomFrontLeftCorners"
  -- runTestTT listOfF1PointsTest

  ---------------- set xy quadrant tests==============================
  runTestTT setQuadrant1YvalTest
  runTestTT setQuadrant2YvalTest
  runTestTT setQuadrant2Yval170Test
  runTestTT setQuadrant3Xval190Test


fail1 = TestCase $ assertEqual
 "fail 1=============================================" (True) (False)

fail2 = TestCase $ assertEqual
 "fail 2============================================" (True) (False)
{----------------------------------- set x/y values for target quadrant ------------------------------------------------
setYPolarityForQuadrant :: QuadrantAngle -> Double -> Double
setYPolarityForQuadrant angle val = case getCurrentQuadrant angle of
                                     Quadrant1 -> negate val
                                     Quadrant2 -> val
                                     Quadrant3 -> val
                                     Quadrant4 -> negate val
-}
setQuadrant1YvalTest = TestCase $ assertEqual
  "set yval for quadrant1"
  (-1)
  (setYPolarityForQuadrant (Quadrant1Angle 1) 1   )

setQuadrant2YvalTest = TestCase $ assertEqual
  "set yval for quadrant2"
  (-1)
  (setYPolarityForQuadrant (Quadrant2Angle 1) 1   )

setQuadrant2Yval170Test = TestCase $ assertEqual
  "set yval for quadrant2 at 170"
  (1)
  (setYPolarityForQuadrant (Quadrant2Angle 170) 1   )

setQuadrant3Xval190Test = TestCase $ assertEqual
  "set xval for quadrant3 at 190"
  (-1)
  (setXPolarityForQuadrant (Quadrant3Angle 190) 1   )
 
{----------------------------------- Try the quadrant angles -------------------------------------------------------------------
xyQuadrantAngle currAngle 
-}
getQuadrantAngleTest = TestCase $ assertEqual 
  "getQuadrantAngleTest" (Quadrant1Angle 10) (xyQuadrantAngle 10  )

getQuadrantAngleTest2 = TestCase $ assertEqual 
  "getQuadrantAngleTest2" (Quadrant2Angle 80) (xyQuadrantAngle 100  )

getQuadrantAngleTest3 = TestCase $ assertEqual 
  "getQuadrantAngleTest3" (Quadrant2Angle 10) (xyQuadrantAngle 170  )

getQuadrantAngleTest4 = TestCase $ assertEqual 
  "getQuadrantAngleTest4" (Quadrant3Angle 10) (xyQuadrantAngle 190  )

getQuadrantAngleTest5 = TestCase $ assertEqual 
  "getQuadrantAngleTest5" (Quadrant3Angle 80) (xyQuadrantAngle 260  )

getQuadrantAngleTest6 = TestCase $ assertEqual 
  "getQuadrantAngleTest6" (Quadrant4Angle 80) (xyQuadrantAngle 280  )

getQuadrantAngleTest7 = TestCase $ assertEqual 
  "getQuadrantAngleTest7" (Quadrant4Angle 10) (xyQuadrantAngle 350  )





{-----------------------------------Test the current z-slope -----------------------------
slopeAdjustedForVerticalAngle xSlope ySlope xyAngle
-}

{--------all the y angles without an x angle-------}
slopeForXYAngleAndYslopeTestX0Ypos0XY0 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestX0Ypos0XY0" (PosXYSlope (0)) (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 0) (xyQuadrantAngle 0)  )


slopeForXYAngleAndYslopeTestX0Ypos10XY10 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestY10" (NegXYSlope (4.92403876506104)) (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 5) (xyQuadrantAngle 10)  )

slopeForXYAngleAndYslopeTestXPos1Ypos10XY10 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestY10" (NegXYSlope (9.67442935245515)) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 10)  )

slopeForXYAngleAndYslopeTestXPos10Ypos1XY10 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestY10" (PosXYSlope (0.7516740236570952)) (slopeAdjustedForVerticalAngle (PosXSlope 10) (PosYSlope 1) (xyQuadrantAngle 10)  )

{-
x1 = sin(10) * 10 = 1.73648177667 pos
y10  = cos(10) * 1 = 0.984807753012 pos
y + x = 2.721289529682 so it is a PosXYSlope

-}
slopeForXYAngleAndYslopeTestXPos10Yneg1XY10 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos10Yneg1XY10" (PosXYSlope (2.721289529681511)) (slopeAdjustedForVerticalAngle (PosXSlope 10) (NegYSlope 1) (xyQuadrantAngle 10)  )

{-
x1 = sin(10) * 1 = 0.173648177667 pos
y10  = cos(10) * 10 = 9.84807753012 pos
y + x = 10.021725707787 so it is a PosXYSlope

-}
slopeForXYAngleAndYslopeTestXPos1Yneg10XY10 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos1Yneg10XY10" (PosXYSlope (10.02172570778901)) (slopeAdjustedForVerticalAngle (PosXSlope 1) (NegYSlope 10) (xyQuadrantAngle 10)  )


slopeForXYAngleAndYslopeTestXPos1YPos10XY80 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestY80" (NegXYSlope 0.7516740236570961) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 80)  )

slopeForXYAngleAndYslopeTestXPos10YPos1XY80 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestY80" (PosXYSlope 9.67442935245515) (slopeAdjustedForVerticalAngle (PosXSlope 10) (PosYSlope 1) (xyQuadrantAngle 80)  )

{-
x1 = sin(80) * 10 = 9.84807753012 pos
y10  = cos(80) * 1 = 0.173648177667 pos
y + x = 10.021725707787 so it is a PosXYSlope
-}
slopeForXYAngleAndYslopeTestXPos10Yneg1XY80 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos10Yneg1XY80" (PosXYSlope (10.02172570778901)) (slopeAdjustedForVerticalAngle (PosXSlope 10) (NegYSlope 1) (xyQuadrantAngle 80)  )

{-
x1 = sin(80) * 1 = 0.984807753012 pos
y10  = cos(80) * 10 = 1.73648177667 pos
y + x = 2.721289529682 so it is a PosXYSlope
-}
slopeForXYAngleAndYslopeTestXPos1Yneg10XY80 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos1Yneg1XY80" (PosXYSlope 2.721289529681512) (slopeAdjustedForVerticalAngle (PosXSlope 1) (NegYSlope 10) (xyQuadrantAngle 80)  )

slopeForXYAngleAndYslopeTestXPos10YPos1XY90 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestY90" (PosXYSlope 10) (slopeAdjustedForVerticalAngle (PosXSlope 10) (PosYSlope 1) (xyQuadrantAngle 90)  )

slopeForXYAngleAndYslopeTestXPos1YPos10XY100 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestY80" (PosXYSlope 2.721289529681512) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 100)  )

slopeForXYAngleAndYslopeTestXPos10YPos1XY100 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestY80" (PosXYSlope 10.02172570778901) (slopeAdjustedForVerticalAngle (PosXSlope 10) (PosYSlope 1) (xyQuadrantAngle 100)  )

{-
x1 = sin(100) * 1 = 0.984807753012 pos
y10  = cos(80) * 10 = 1.73648177667 neg
y - x = 0.751674023658 NegXYSlope
-}
slopeForXYAngleAndYslopeTestXPos1YNeg10XY100 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos1YNeg10XY100" (NegXYSlope 0.7516740236570961) (slopeAdjustedForVerticalAngle (PosXSlope 1) (NegYSlope 10) (xyQuadrantAngle 100)  )


slopeForXYAngleAndYslopeTestXPos1YPos10XY170 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestY80" (PosXYSlope 10.02172570778901) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 170)  )

slopeForXYAngleAndYslopeTestXPos10YPos1XY170 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestY80" (PosXYSlope 2.721289529681511) (slopeAdjustedForVerticalAngle (PosXSlope 10) (PosYSlope 1) (xyQuadrantAngle 170)  )

{-
x1 = sin(10) * 0 = 0.173648177667 pos
y10  = cos(10) * 10 = 9.84807753012 neg
y - x = 9.84807753012 NegXYSlope
-}
slopeForXYAngleAndYslopeTestXPos0YNeg1XY170 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos0YNeg1XY170" (NegXYSlope 9.84807753012208) (slopeAdjustedForVerticalAngle (PosXSlope 0) (NegYSlope 10) (xyQuadrantAngle 170)  )

{-
x1 = sin(10) * 1 = 0 pos
y10  = cos(10) * 10 = 9.84807753012 neg
y - x = 9.674429352453 NegXYSlope
-}
slopeForXYAngleAndYslopeTestXPos1YNeg1XY170 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos0YNeg1XY170" (NegXYSlope 9.67442935245515) (slopeAdjustedForVerticalAngle (PosXSlope 1) (NegYSlope 10) (xyQuadrantAngle 170)  )

slopeForXYAngleAndYslopeTestXPos1YPos10XY190 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos1YPos10XY190" (PosXYSlope 9.67442935245515) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 190)  )

slopeForXYAngleAndYslopeTestXPos10YPos1XY190 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos10YPos1XY190" (NegXYSlope 0.7516740236570952) (slopeAdjustedForVerticalAngle (PosXSlope 10) (PosYSlope 1) (xyQuadrantAngle 190)  )

{-
x1 = sin(10) * 1 = 0.173648177667 neg
y10  = cos(10) * 10 = 9.84807753012 neg
y + x = 10.021725707787 NegXYSlope
continue here with testing
-}
slopeForXYAngleAndYslopeTestXPos1YNeg10XY190 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos1YNeg10XY190" (NegXYSlope 10.02172570778901) (slopeAdjustedForVerticalAngle (PosXSlope 1) (NegYSlope 10) (xyQuadrantAngle 190)  )


slopeForXYAngleAndYslopeTestXPos1YPos10XY260 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos1YPos10XY260" (PosXYSlope 0.7516740236570961) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 260)  )

slopeForXYAngleAndYslopeTestXPos10YPos1XY260 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos10YPos1XY260" (NegXYSlope 9.67442935245515) (slopeAdjustedForVerticalAngle (PosXSlope 10) (PosYSlope 1) (xyQuadrantAngle 260)  )

slopeForXYAngleAndYslopeTestXPos1YPos10XY280 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos1YPos10XY280" (NegXYSlope 2.721289529681512) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 280)  )

slopeForXYAngleAndYslopeTestXPos10YPos1XY280 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos10YPos1XY280" (NegXYSlope 10.02172570778901) (slopeAdjustedForVerticalAngle (PosXSlope 10) (PosYSlope 1) (xyQuadrantAngle 280)  )

{-
x1 = sin(80) * 1 = 0.984807753012 neg
y10  = cos(80) * 10 = 1.73648177667 pos
y - x = 0.751674023658 PosXYSlope
0.7516740236570961
-}
slopeForXYAngleAndYslopeTestXPos1YNeg10XY280 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos1YNeg10XY280" (PosXYSlope 0.7516740236570961) (slopeAdjustedForVerticalAngle (PosXSlope 1) (NegYSlope 10) (xyQuadrantAngle 280)  )

{-
x1 = sin(80) * 10 = 9.84807753012 neg
y10  = cos(80) * 1 = 0.173648177667 pos
y - x = 9.674429352453 NegXYSlope
0.7516740236570961
-}
slopeForXYAngleAndYslopeTestXPos10YNeg1XY280 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos10YNeg1XY280" (NegXYSlope 9.67442935245515) (slopeAdjustedForVerticalAngle (PosXSlope 10) (NegYSlope 1) (xyQuadrantAngle 280)  )


slopeForXYAngleAndYslopeTestXPos1YPos10XY350 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos1YPos10XY350" (NegXYSlope 10.02172570778901) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 350)  )

slopeForXYAngleAndYslopeTestXPos10YPos1XY350 = TestCase $ assertEqual 
  "slopeForXYAngleAndYslopeTestXPos10YPos1XY350" (NegXYSlope 2.721289529681511) (slopeAdjustedForVerticalAngle (PosXSlope 10) (PosYSlope 1) (xyQuadrantAngle 350)  )

slopeForXYAngleAndYslopeTestNeedMoreTesting = TestCase $ assertBool 
  "slopeForXYAngleAndYslopeTest: need to test for more variations of Pos/Neg X/Yslopes" False


{-test for radius adjustment on the xy plane, for various x and y slopes
radiusAdjustedForZslope :: Radius -> Slope  -> Radius 
-}
radiusAdjustedForZslopeTestRad10PosX10PosY0XY10 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX10PosY0XY10" (DownRadius 9.857785663826117) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 10)))

radiusAdjustedForZslopeTestRad10PosX0PosY10XY10 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX0PosY10XY10" (UpRadius 9.999139447055672) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 10) (PosYSlope 1) (xyQuadrantAngle 10)))

{-
x1 = sin(10) * 1 = 0.173648177667 pos
y10  = cos(10) * 10 = 9.84807753012 pos
y + x = 10.02260159449 so it is a PosXYSlope
adjustedRadius = Rad * cos(slope) = 10 * cos(10.02260159449) =  9.84739177006 UpRadius
-}
radiusAdjustedForZslopeTestRad10PosX1NegY10XY10 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX1PosY10XY10" (UpRadius 9.84741837407899) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (NegYSlope 10) (xyQuadrantAngle 10)))

{-
x1 = sin(10) * 1 = 1.73648177667 pos
y10  = cos(10) * 1 = 0.984807753012 pos
y + x = 2.721289529682 so it is a PosXYSlope
adjustedRadius = Rad * cos(slope) = 10 * cos(2.721289529682) =  9.9887230255 UpRadius
-}
radiusAdjustedForZslopeTestRad10PosX10NegY1XY10 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX10NegY1XY10" (UpRadius 9.988723025495544) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 10) (NegYSlope 1) (xyQuadrantAngle 10)))



radiusAdjustedForZslopeTestRad10PosX1PosY10XY80 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX1PosY10XY80" (DownRadius 9.999139447055672) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 80)))

radiusAdjustedForZslopeTestRad10PosX1PosY10XY100 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX1PosY10XY100" (UpRadius 9.988723025495544) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 100)))

{-
x1 = sin(80) * 10 = 9.84807753012 pos
y10  = cos(80) * 1 = 0.173648177667 neg
y + x = 9.674429352453 so it is a PosXYSlope
adjustedRadius = Rad * cos(slope) = 10 * cos(9.674429352453) =  9.85778566383 UpRadius
-}
radiusAdjustedForZslopeTestRad10PosX10NegY1XY100 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX10NegY1XY100" (UpRadius 9.857785663826117) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 10) (NegYSlope 1) (xyQuadrantAngle 100)))

{-
x1 = sin(80) * 1 = 0.984807753012 pos
y10  = cos(80) * 10 = 1.73648177667 neg
y + x = 0.751674023658 NegXYSlope
adjustedRadius = Rad * cos(slope) = 10 * cos(0.751674023658) =  9.99913944706 DownRadius
-}
radiusAdjustedForZslopeTestRad10PosX1NegY10XY100 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX1NegY10XY100" (DownRadius  9.999139447055672) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (NegYSlope 10) (xyQuadrantAngle 100)))



radiusAdjustedForZslopeTestRad10PosX1PosY10XY170 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX1PosY10XY170" (UpRadius 9.84741837407899) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 170)))

{-((sinDegrees xyAngle) * xSlope) - ((cosDegrees xyAngle) * ySlope)
x1 = sin(10) * 10 = 1.73648177667 pos
y10  = cos(10) * 1 = 0.984807753012 neg
y - x = 0.751674023658 PosXYSlope
adjustedRadius = Rad * cos(slope) = 10 * cos(0.751674023658) =  9.99913944706 UpRadius
-}
radiusAdjustedForZslopeTestRad10PosX10NegY1XY170 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX10NegY1XY170" (UpRadius  9.999139447055672) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 10) (NegYSlope 1) (xyQuadrantAngle 170)))

{-
x1 = sin(10) * 10 = 1.73648177667 pos
y10  = cos(10) * 1 = 0.984807753012 neg
y - x = 9.674429352453 NegXYSlope
adjustedRadius = Rad * cos(slope) = 10 * cos(9.674429352453) =  9.85778566383 DownRadius
-}
radiusAdjustedForZslopeTestRad10PosX1NegY10XY170 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX1NegY10XY170" (DownRadius  9.857785663826117) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 10) (NegYSlope 1) (xyQuadrantAngle 170)))


{-
x1 = sin(10) * 1 = 0.17452406437 neg
y10  = cos(10) * 10 = 9.84807753012 pos
y - x = 9.67355346575 so it is a PosXYSlope
adjustedRadius = Rad * cos(slope) = 10 * cos(9.67355346575) =  9.857785663826117 UpRadius
-}
radiusAdjustedForZslopeTestRad10PosX1PosY10XY190 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX1PosY10XY190" (UpRadius 9.857785663826117) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 190)))

{-
x10 = sin(10) * 10 = 1.73648177667 neg as in 3rd quad
y1 - cos(10) * 1 = 0.984807753012
y - x = -0.751674023658 so it is a NegXYSlope
adjustedRadius = Rad * cos(xySlope) = 10 * cos(0.751674023658) = 9.999139447055672 DownRadius as it was a NegXYSlope
-}
radiusAdjustedForZslopeTestRad10PosX10PosY1XY190 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX10PosY1XY190" (DownRadius 9.999139447055672) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 10) (PosYSlope 1) (xyQuadrantAngle 190)))







{-
x1 = sin(10) * 1 = 0.173648177667 neg 
y10 = cos(10) * 10 = 9.84807753012 neg
y - x = 10.021725707787 so it is a NegXYSlope
adjustedRadius = Rad * cos(xySlope) = 10 * cos( 10.021725707787) = 9.84741837408 DownRadius as it was a NegXYSlope
-}
radiusAdjustedForZslopeTestRad10PosX1NegY1oXY190 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX10PosY1XY190" (DownRadius 9.84741837407899) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (NegYSlope 10) (xyQuadrantAngle 190)))








{-
x10 = sin(10) * 10 = 1.73648177667 neg as in 3rd quad
y1 - cos(10) * 1 = 0.984807753012 neg as in 3rd quad
y + x = -2.721289529682 so it is a NegXYSlope
adjustedRadius = Rad * cos(xySlope) = 10 * cos(2.721289529682) = 9.9887230255 DownRadius as it was a NegXYSlope
-}
radiusAdjustedForZslopeTestRad10PosX10NegY1XY190 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX10NegY1XY190" (DownRadius 9.988723025495544) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 10) (NegYSlope 1) (xyQuadrantAngle 190)))

{-
x1 = sin(80) * 1 = 0.984807753012 neg as it is in 3rd quad
y10 = cos(80) * 10 = 1.73648177667 pos as it is in 3rd quad
y - x = 0.751674023658 posXYSlope
adjustedRadius = Rad * cos(xySlope) = 10 * cos(0.751674023658) = 9.999139447055672 UpRadius as it was a PosXYSlope
-}
radiusAdjustedForZslopeTestRad10PosX1PosY10XY260 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX1PosY10XY260" (UpRadius 9.999139447055672) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 260)))

{-
x10 = sin(80) * 10 = 9.84807753012 neg as it is in 3rd quad
y1 = cos(80) * 1 = 0.173648177667 pos as it is in 3rd quad
y - x = 9.674429352453 NegXYSlope
adjustedRadius = Rad * cos(xySlope) = 10 * cos(9.674429352453) = 9.85778566383 DownRadius as it was a PosXYSlope
-}
radiusAdjustedForZslopeTestRad10PosX10PosY1XY260 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX10PosY1XY260" (DownRadius 9.857785663826117) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 10) (PosYSlope 1) (xyQuadrantAngle 260)))

{-
x1 = sin(80) * 1 = 0.984807753012 neg as it is in 4 quad
y10 = cos(80) * 10 = 1.73648177667 neg as it is in 4 quad
y + x = 2.721289529682 NegXYSlope
adjustedRadius = Rad * cos(xySlope) = 10 * cos(2.721289529682) = 9.9887230255 DownRadius as it was a NegXYSlope
-}
radiusAdjustedForZslopeTestRad10PosX1PosY10XY280 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX1PosY10XY280" (DownRadius 9.988723025495544) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 280)))

{-
x10 = sin(80) * 10 = 9.84807753012 neg as it is in 4 quad
y1 = cos(80) * 1 = 0.173648177667 neg as it is in 4 quad
y + x = 10.021725707787 NegXYSlope
adjustedRadius = Rad * cos(xySlope) = 10 * cos(10.021725707787) = 9.84741837408 DownRadius as it was a NegXYSlope
-}
radiusAdjustedForZslopeTestRad10PosX10PosY1XY280 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX10PosY1XY280" (DownRadius 9.84741837407899) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 10) (PosYSlope 1) (xyQuadrantAngle 280)))

{-
x1 = sin(10) * 1 = 0.173648177667 neg as it is in 4 quad
y10 = cos(10) * 10 = 9.84807753012 neg as it is in 4 quad
y + x = 10.021725707787 NegXYSlope
adjustedRadius = Rad * cos(xySlope) = 10 * cos(10.021725707787) = 9.84741837408 DownRadius as it was a NegXYSlope
-}
radiusAdjustedForZslopeTestRad10PosX1PosY10XY350 = TestCase $ assertEqual 
  "radiusAdjustedForZslopeTestRad10PosX1PosY10XY350" (DownRadius 9.84741837407899) (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 350)))


radiusAdjustedForZslopeTestNeedMoreTesting = TestCase $ assertBool 
  "radiusAdjustedForZslopeTest: need to test for more variations of Pos/Neg X/Yslopes" False
{--------------------------------------------Create CornerPonts and test them.----------------------------------------------------------}

{-This is a bottom front right corner with no z-slope.
singleF4PointTest  = TestCase $ assertEqual 
  --test values: xyAngle 0; xSlope 0, ySlope 0, radius 18
  "the bottom right front corner using Equal instance" ( F4  ( Point 0 (-18) 0 )) (createCornerPoint  (F4) (Point{x_axis=0, y_axis=0, z_axis=0})18 0 0 0   ) 


createCornerPoint :: (Point-> CornerPoints) -> Point -> Radius ->  QuadrantAngle -> Slope -> CornerPoints  
-}

createFrontCornerTest = TestCase $ assertEqual
  "recreate the front corner to see what is wrong with my vertical faces"
  (F3 (Point 1 (-6.123233995736766e-17) 50))
  (createCornerPoint
    (F3)
    (Point{x_axis=0, y_axis=0, z_axis=50})
    (Radius 1)
    (radiusAdjustedForZslope (Radius 1) (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 0) (xyQuadrantAngle 90)))
    (Angle 90)--(xyQuadrantAngle 90)
    (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 0) (xyQuadrantAngle 90))
  ) 
createCornerPointTestR10PosX0PosY0XY10  = TestCase $ assertEqual 
  "createCornerPointTestR10PosX0PosY0XY10 this is the first non-exhaust failure"
  ( F4  ( Point 1.7364817766693033 (-9.84807753012208) 0 ))
  (createCornerPoint
    (F4)
    (Point{x_axis=0, y_axis=0, z_axis=0})
    (Radius 10)
    (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 0) (xyQuadrantAngle 10)))
    (Angle 10)--(xyQuadrantAngle 10)
    (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 0) (xyQuadrantAngle 10))
  ) 

{-
z = radius * sin(xySlope)
  = 10 * sin(9.67442935245515) = 1.68049451236
-}
createCornerPointTestR10PosX0PosY10XY10  = TestCase $ assertEqual 
  "createCornerPointTestR10PosX0PosY10XY10"
  ( F4  ( Point 1.7117865163545964 (-9.708023749268555) (-1.6804945123576784) ))
  (createCornerPoint
    (F4)
    (Point{x_axis=0, y_axis=0, z_axis=0})
    (Radius 10)
    (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 10)))
    (Angle 10)--(xyQuadrantAngle 10)
    (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 10))
  ) 

{-
z = radius * sin(xySlope)
  = 10 * sin(9.67442935245515) = -0.13118810287215432
this is just assumed, as I did not have a corresponding xySlope test. If all the others work out though, I will ass-u-me this is right.
-}
createCornerPointTestR10PosX0PosY10XY80  = TestCase $ assertEqual 
  "createCornerPointTestR10PosX0PosY10XY80"
  ( F4  ( Point 9.847230050910628 (-1.7363323432187356) (-0.13118810287215432) ))
  (createCornerPoint
    (F4)
    (Point{x_axis=0, y_axis=0, z_axis=0})
    (Radius 10)
    (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 80)))
    (Angle 80)--(xyQuadrantAngle 80)
    (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 80))
  ) 
{-
z = radius * sin(xySlope)
  = 10 * sin( 2.721289529681512) =  0.47477607347
-}

createCornerPointTestR10PosX1PosY10XY100  = TestCase $ assertEqual 
  "createCornerPointTestR10PosX1PosY10XY100"
  ( F4  ( Point 9.83697187819957 (1.734523550597009) (0.47477607346532197) ))
  (createCornerPoint
    (F4)
    (Point{x_axis=0, y_axis=0, z_axis=0})
    (Radius 10)
    (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 100)))
    (Angle 100)--(Quadrant2Angle 100)--(xyQuadrantAngle 100)
    (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 100))
  ) 

--createCornerPoint cPoint origin (Radius horizRadius) (DownRadius adjustedRadius)  (Quadrant2Angle xyAngle) (NegXYSlope slope)
{-
createCornerPoint cPoint origin (Radius horizRadius) (DownRadius adjustedRadius)  (Quadrant2Angle xyAngle) (NegXYSlope slope) = cPoint (Point 
                                    (--x:
                                     x_axis origin + adjustedRadius * (sinDegrees  xyAngle))
                                    0 +               9.857785663826117 * sin(10) = 1.7117865163552831
                                    (--y:
                                     y_axis origin + adjustedRadius * (cosDegrees  xyAngle))
                                     0 +              9.857785663826117 * cos(10) = 9.708023749266505093
                                    (--z:
                                     z_axis origin + horizRadius * (sinDegrees (slope)))
                                             0 -     10 * sin(9.67442935245515) = -1.68049451236
                                  )
-}
createCornerPointTestR10PosX1NegY10XY170  = TestCase $ assertEqual 
  "createCornerPointTestR10PosX1NegY10XY170 and now this one is failing"
  ( F4  ( Point  1.7117865163545964 (9.708023749268555) (-1.6804945123576784) ))
  (createCornerPoint
    (F4)
    (Point{x_axis=0, y_axis=0, z_axis=0})
    (Radius 10)
    (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (NegYSlope 10) (xyQuadrantAngle 170)))
    (Angle 170)--(xyQuadrantAngle 170)
    (slopeAdjustedForVerticalAngle (PosXSlope 1) (NegYSlope 10) (xyQuadrantAngle 170))
  ) 

{-
createCornerPoint cPoint origin (Radius horizRadius) (DownRadius adjustedRadius)  (Quadrant2Angle xyAngle) (NegXYSlope slope) = cPoint (Point 
                                    (--x:
                                     x_axis origin + adjustedRadius * (sinDegrees  xyAngle))
                                    
                                    (--y:
                                     y_axis origin + adjustedRadius * (cosDegrees  xyAngle))
                                    
                                    (--z:
                                     z_axis origin + horizRadius * (sinDegrees (slope)))
                                  )

-}
createCornerPointTestR10PosX1PosY10XY170  = TestCase $ assertEqual 
  "createCornerPointTestR10PosX1PosY10XY170"
  ( F4  ( Point 1.7099862553826626 (9.69781396194786) 1.740215896333419 ))
  (createCornerPoint
    (F4)
    (Point{x_axis=0, y_axis=0, z_axis=0})
    (Radius 10)
    (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 170)))
    (Angle 170)--(xyQuadrantAngle 170)
    (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 170))
  ) 

{-
z = radius * sin(xySlope)
  = 10 * sin() =  
-}
createCornerPointTestR10PosX10PosY1XY170  = TestCase $ assertEqual 
  "createCornerPointTestR10PosX1PosY10XY170"
  ( F4  ( Point 1.734523550597008 (9.83697187819957) 0.47477607346532175 ))
  (createCornerPoint
    (F4)
    (Point{x_axis=0, y_axis=0, z_axis=0})
    (Radius 10)
    (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 10) (PosYSlope 1) (xyQuadrantAngle 170)))
    (Angle 170)--(xyQuadrantAngle 170)
    (slopeAdjustedForVerticalAngle (PosXSlope 10) (PosYSlope 1) (xyQuadrantAngle 170))
  ) 

{-
radiusAdjustedForZslope: UpRadius 9.857785663826117
xyQuadrantAngle 190: Quadrant3Angle 10

(slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 190)): PosXYSlope 9.67442935245515

createCornerPoint cPoint origin (UpRadius adjustedRadius)  (Quadrant3Angle xyAngle) (PosXYSlope slope) = cPoint (Point 
                                    (--x:
                                     x_axis origin - adjustedRadius * (sinDegrees  xyAngle))
                                     0 - 9.857785663826117 * sin(10) = - 1.7117865163552831
                                    
                                    (--y:
                                     y_axis origin + adjustedRadius * (cosDegrees  xyAngle))
                                     0 + 9.857785663826117 * cos(10) = 9.708023749266505093
                                    
                                    (--z:
                                     z_axis origin + adjustedRadius * (sinDegrees (slope)))
                                     0 + 10 * sin(9.67442935245515) = 1.68049451236
                                  )
-}
createCornerPointTestR10PosX1PosY10XY190  = TestCase $ assertEqual 
  "createCornerPointTestR10PosX1PosY10XY190"
  ( F4  ( Point (-1.7117865163545964) 9.708023749268555 1.6804945123576784 ))
  (createCornerPoint
    (F4)
    (Point{x_axis=0, y_axis=0, z_axis=0})
    (Radius 10)
    (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 190)))
    (Angle 190)--(xyQuadrantAngle 190)
    (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 190))
  ) 



{-                                                                                    
                                                        
                                        10              9.84741837407899                        10          10.02172570778901
createCornerPoint cPoint origin (Radius horizRadius) (DownRadius adjustedRadius)  (Quadrant3Angle xyAngle) (NegXYSlope slope) = cPoint (Point 
                                    (--x:
                                     x_axis origin - adjustedRadius * (sinDegrees  xyAngle))
                                            0      - 9.84741837407899 * sin(10)  = -1.70998625538334872
                                    
                                    (--y:
                                     y_axis origin + adjustedRadius * (cosDegrees  xyAngle))
                                    0 +             9.84741837407899  * cos(10) = 9.69781396194581261
                                    (--z:
                                     z_axis origin - horizRadius * (sinDegrees (slope)))
                                             0     -   10  * sin(10.02172570778901) = -1.74021589633
                                  )
-}


createCornerPointTestR10PosX1NegY10XY190  = TestCase $ assertEqual 
  "createCornerPointTestR10PosX1NegY10XY190 fail 0"
  ( F4  ( Point (-1.7099862553826626) 9.69781396194786 (-1.740215896333419) ))
  (createCornerPoint
    (F4)
    (Point{x_axis=0, y_axis=0, z_axis=0})
    (Radius 10)
    (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (NegYSlope 10) (xyQuadrantAngle 190)))
    (Angle 190)--(xyQuadrantAngle 190)
    (slopeAdjustedForVerticalAngle (PosXSlope 1) (NegYSlope 10) (xyQuadrantAngle 190))
  ) 





{- has not been verified thought it looks good in netfabb
radiusAdjustedForZslope: UpRadius 9.999139447055672
xyQuadrantAngle 260: Quadrant3Angle 80

(slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 260)): PosXYSlope 0.7516740236570961

createCornerPoint cPoint origin (UpRadius adjustedRadius)  (Quadrant3Angle xyAngle) (PosXYSlope slope) = cPoint (Point 
                                    (--x:
                                     x_axis origin - adjustedRadius * (sinDegrees  xyAngle))
                                     0 - 9.999139447055672 * sin(80) = - 9.847230050908548482
                                    
                                    (--y:
                                     y_axis origin + adjustedRadius * (cosDegrees  xyAngle))
                                     0 + 9.999139447055672 * cos(80) = 1.736332343219431471
                                    
                                    (--z:
                                     z_axis origin + adjustedRadius * (sinDegrees (slope)))
                                     0 + 10 * sin(0.7516740236570961) = 0.13118810287
                                  )

-}
createCornerPointTestR10PosX1PosY10XY260  = TestCase $ assertEqual 
  "createCornerPointTestR10PosX1PosY10XY260"
  ( F4  ( Point (-9.847230050910628) (1.7363323432187356) (0.13118810287215432) ))
  (createCornerPoint
    (F4)
    (Point{x_axis=0, y_axis=0, z_axis=0})
    (Radius 10)
    (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 260)))
    (Angle 260)--(xyQuadrantAngle 260)
    (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 260))
  )

{- has not been verified though it looks good in netfabb
radiusAdjustedForZslope: DownRadius 9.988723025495544
xyQuadrantAngle 280: Quadrant4Angle 80

(slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 280)): NegXYSlope 2.721289529681512

createCornerPoint cPoint origin (DownRadius adjustedRadius)  (Quadrant4Angle xyAngle) (NegXYSlope slope) = cPoint (Point 
                                    (--x:
                                     x_axis origin - adjustedRadius * (sinDegrees  xyAngle))
                                     0 - 9.988723025495544 * sin(80) = - 9.836971878197493074
                                    (--y:
                                     y_axis origin - adjustedRadius * (cosDegrees  xyAngle))
                                     0 - 9.988723025495544 * cos(80) = - 1.734523550597703995
                                    (--z:
                                     z_axis origin - adjustedRadius * (sinDegrees (slope)))
                                     0 - 10 * sin(2.721289529681512) = - 0.47477607347
                                  )


-}
createCornerPointTestR10PosX1PosY10XY280  = TestCase $ assertEqual 
  "createCornerPointTestR10PosX1PosY10XY280 fail 280"
  ( F4  ( Point (-9.83697187819957) (-1.734523550597009) (-0.47477607346532197) ))
  (createCornerPoint
    (F4)
    (Point{x_axis=0, y_axis=0, z_axis=0})
    (Radius 10)
    (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 280)))
    (Angle 280)--(xyQuadrantAngle 280)
    (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 280))
  ) 

{-
radiusAdjustedForZslope: DownRadius 9.84741837407899
xyQuadrantAngle 350: Quadrant4Angle 10

(slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 350)): NegXYSlope 10.02172570778901

createCornerPoint cPoint origin (DownRadius adjustedRadius)  (Quadrant4Angle xyAngle) (NegXYSlope slope) = cPoint (Point 
                                    (--x:
                                     x_axis origin - adjustedRadius * (sinDegrees  xyAngle))
                                     0 - 9.84741837407899 * sin(10) = - 1.70998625538334872
                                    (--y:
                                     y_axis origin - adjustedRadius * (cosDegrees  xyAngle))
                                     0 - 9.84741837407899 * cos(10) = - 9.69781396194581261
                                    (--z:
                                     z_axis origin - adjustedRadius * (sinDegrees (slope)))
                                     0 - 10 * sin(10.02172570778901) = - 1.74021589633
                                  )


-}
createCornerPointTestR10PosX1PosY10XY350  = TestCase $ assertEqual 
  "createCornerPointTestR10PosX1PosY10XY350 fail 350"
  ( F4  ( Point (-1.7099862553826626) (-9.69781396194786) (-1.740215896333419) ))
  (createCornerPoint
    (F4)
    (Point{x_axis=0, y_axis=0, z_axis=0})
    (Radius 10)
    (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 350)))
    (Angle 350)--(xyQuadrantAngle 350)
    (slopeAdjustedForVerticalAngle (PosXSlope 1) (PosYSlope 10) (xyQuadrantAngle 350))
  ) 

{----------------------------------------------------------------------------------------------------------------------
The following 2 tests recreate CowboyHeelAdaptor top layer(3) which is the male legos adaptors on a slope.
These always give a netfabb error when more than 1 lego attachment is used. The results of this test do not show any reason why this would be.

The test had to be broken up into 2 parts due to truncating of the test results.

0-10 degree lego attachment

CubePoints {
f1 = Point {x_axis = 1.7364817766693033, y_axis = -9.84807753012208, z_axis = 0.0}, 
f2 = Point {x_axis = 1.7108942434345316, y_axis = -14.702963418078856, z_axis = 7.2896369405281}, 
f3 = Point {x_axis = 0.0, y_axis = -14.84807753012208, z_axis = 7.2635182233306965}, 
f4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0}, 
b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, 
b2 = Point {x_axis = 0.0, y_axis = -5.0, z_axis = 9.0}, 
b3 = Point {x_axis = 0.0, y_axis = -5.0, z_axis = 9.0}, 
b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}}


80-90 degree lego attachment

CubePoints {
f1 = Point {x_axis = 10.0, y_axis = -6.123233995736766e-16, z_axis = 0.0}, 
f2 = Point {x_axis = 10.0, y_axis = -5.000000000000001, z_axis = 9.0}, 
f3 = Point {x_axis = 9.84355497972206, y_axis = -6.735684329012167, z_axis = 8.696973151007013}, 
f4 = Point {x_axis = 9.84807753012208, y_axis = -1.7364817766693041, z_axis = 0.0}, 
b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, 
b2 = Point {x_axis = 0.0, y_axis = -5.0, z_axis = 9.0}, 
b3 = Point {x_axis = 0.0, y_axis = -5.0, z_axis = 9.0}, 
b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}}

-}
createCornerPoint10DegTestSlopedMaleLegos  = TestCase $ assertEqual 
  "createCornerPoint10DegTestSlopedMaleLegos"
  CubePoints {f1 = Point {x_axis = 1.7364817766693033, y_axis = -9.84807753012208, z_axis = 0.0}, f2 = Point {x_axis = 1.7108942434345316, y_axis = -14.702963418078856, z_axis = 7.2896369405281}, f3 = Point {x_axis = 0.0, y_axis = -14.84807753012208, z_axis = 7.2635182233306965}, f4 = Point {x_axis = 0.0, y_axis = -10.0, z_axis = 0.0}, b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, b2 = Point {x_axis = 0.0, y_axis = -5.0, z_axis = 9.0}, b3 = Point {x_axis = 0.0, y_axis = -5.0, z_axis = 9.0}, b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}}
   
  
    (((
      (createCornerPoint
      (F4)
      (Point{x_axis=0, y_axis=0, z_axis=0})
      (Radius 10)
      (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 0) (xyQuadrantAngle 0)))
      (Angle 0)
      (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 0) (xyQuadrantAngle 0)))
      +++
      B4 (Point{x_axis=0, y_axis=0, z_axis=0})
    )
    +++
    (
      (createCornerPoint
      (F1)
      (Point{x_axis=0, y_axis=0, z_axis=0})
      (Radius 10)
      (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 0) (xyQuadrantAngle 10)))
      (Angle 10)
      (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 0) (xyQuadrantAngle 10)))
      +++
      B1 (Point{x_axis=0, y_axis=0, z_axis=0})
    ))

    +++

    ((
      (createCornerPoint
      (F3)
      (Point{x_axis=0, y_axis=(-5), z_axis=9})
      (Radius 10)
      (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 10) (xyQuadrantAngle 0)))
      (Angle 0)
      (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 10) (xyQuadrantAngle 0)))
      +++
      B3 (Point{x_axis=0, y_axis=(-5), z_axis=9})
    )
    +++
    (
      (createCornerPoint
      (F2)
      (Point{x_axis=0, y_axis=(-5), z_axis=9})
      (Radius 10)
      (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 10) (xyQuadrantAngle 10)))
      (Angle 10)
      (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 10) (xyQuadrantAngle 10)))
      +++
      B2 (Point{x_axis=0, y_axis=(-5), z_axis=9})
    )))
  
  
{-------------------------------------------create sloped lego pieces, to recreate the error I am getting--------------------------------------------------------------------------}

createCornerPointTest90DegSlopedMaleLegos  = TestCase $ assertEqual 
  "createCornerPoint90DegTestSlopedMaleLegos"
  CubePoints {f1 = Point {x_axis = 10.0, y_axis = -6.123233995736766e-16, z_axis = 0.0}, f2 = Point {x_axis = 10.0, y_axis = -5.000000000000001, z_axis = 9.0}, f3 = Point {x_axis = 9.84355497972206, y_axis = -6.735684329012167, z_axis = 8.696973151007013}, f4 = Point {x_axis = 9.84807753012208, y_axis = -1.7364817766693041, z_axis = 0.0}, b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, b2 = Point {x_axis = 0.0, y_axis = -5.0, z_axis = 9.0}, b3 = Point {x_axis = 0.0, y_axis = -5.0, z_axis = 9.0}, b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}}
   
  
    
    (  ((
      (createCornerPoint
      (F4)
      (Point{x_axis=0, y_axis=0, z_axis=0})
      (Radius 10)
      (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 0) (xyQuadrantAngle 80)))
      (Angle 80)
      (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 0) (xyQuadrantAngle 80)))
      +++
      B4 (Point{x_axis=0, y_axis=0, z_axis=0})
    )
    +++
    (
      (createCornerPoint
      (F1)
      (Point{x_axis=0, y_axis=0, z_axis=0})
      (Radius 10)
      (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 0) (xyQuadrantAngle 90)))
      (Angle 90)
      (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 0) (xyQuadrantAngle 90)))
      +++
      B1 (Point{x_axis=0, y_axis=0, z_axis=0})
    ))

    +++

    ((
      (createCornerPoint
      (F3)
      (Point{x_axis=0, y_axis=(-5), z_axis=9})
      (Radius 10)
      (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 10) (xyQuadrantAngle 80)))
      (Angle 80)
      (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 10) (xyQuadrantAngle 80)))
      +++
      B3 (Point{x_axis=0, y_axis=(-5), z_axis=9})
    )
    +++
    (
      (createCornerPoint
      (F2)
      (Point{x_axis=0, y_axis=(-5), z_axis=9})
      (Radius 10)
      (radiusAdjustedForZslope (Radius 10) (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 10) (xyQuadrantAngle 90)))
      (Angle 90)
      (slopeAdjustedForVerticalAngle (PosXSlope 0) (PosYSlope 10) (xyQuadrantAngle 90)))
      +++
      B2 (Point{x_axis=0, y_axis=(-5), z_axis=9})
    )))
  
  
