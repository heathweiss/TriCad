module Tests.ParseJuicyTest (parseJuicyTestDo) where
import Test.HUnit
import Scan.ParseJuicy( getThePixelsRightOfCenter, convertPixelsToMillmeters, calculateRadiusFrom,
                       removeLeftOfCenterPixels, TargetValueIndex(..), ofThe, forThe, andThen, adjustedFor, andThe)
import CornerPoints.Radius(Radius(..))

parseJuicyTestDo = do
  runTestTT calculateRadiusFromPixelsRightOfCenterTest
  
  runTestTT calculateMillimetersTest
  --get pixels right of center
  runTestTT adjustPixelIndexForLeftSlopeTestRedo

{-
calculateMillimetersFromPixelsRightOfCenter :: PixelToMillmeterConversionFactor -> NumberOfPixels -> Angle ->  Millimeters
calculateMillimetersFromPixelsRightOfCenter    conversionFactor                    pixelCount        angle  =
  (pixelCount / (sinDegrees angle)) * conversionFactor

-}
calculateRadiusFromPixelsRightOfCenterTest = TestCase $ assertEqual
  ("calculate Radius From Pixels Right Of Center Test")
  (Radius 4)
  (let pixelsPerMillemter = 100/1
       pixelsRightOfCenter = 200
       cameraAngle = 30
   in  calculateRadiusFrom  pixelsRightOfCenter (adjustedFor pixelsPerMillemter) $ andThe cameraAngle
       
  )
  
calculateMillimetersTest = TestCase $ assertEqual
  ("calculate millimeters from pixels")
  (2)
  (let pixelsPerMillemter = 50/1
   in  convertPixelsToMillmeters pixelsPerMillemter 100 
  )
  

{-
Given a center for top and bottom of image, and an index of the target value,
calculate the radius of the target value, adjusting for the slope of the laser line.
-}
adjustPixelIndexForLeftSlopeTestRedo = TestCase $ assertEqual
  ("adjust Pixel Index For Left Slope Test")
  ([10.0::TargetValueIndex, 20.0::TargetValueIndex, 30.0::TargetValueIndex])
  (let totalRows = 100
       topCenter = 110
       btmCenter = 90
       topRow = 0
       middleRow = 50
       btmRow = totalRows
       targetValueIndex = 120
       pixelsRightOfCenter = getThePixelsRightOfCenter $ andThen  (removeLeftOfCenterPixels btmCenter topCenter totalRows)
       
   in
       [
        pixelsRightOfCenter (forThe targetValueIndex) $ ofThe topRow,
        pixelsRightOfCenter (forThe targetValueIndex) $ ofThe middleRow,
        pixelsRightOfCenter (forThe targetValueIndex) $ ofThe btmRow 
       ]
  )

