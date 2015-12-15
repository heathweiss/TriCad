{-# LANGUAGE ParallelListComp #-}
{- |
Should be deleted as I am no longer going to scan the boot.
Leave it here for now, as it is the latest use of ParseJuicy, including things such as removeNulls.
Once I have used them in a new project, then delete this file.

Also has an example of filtering the MDR down to a single section.
-}
module Examples.ShoeLift.SnowBoardBoot () where

import Scan.ParseJuicy( getRedLaserLineSingleImage, removeLeftOfCenterPixels, getThePixelsRightOfCenter,  reduceScanRows, reduceScan,
                       calculateRadiusFrom, calculatePixelsPerMillmeter)
import Data.Word(Word8)
import CornerPoints.Radius(resetMultiDegreeRadiiIfNullWithPreviousValue, MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..), )
import CornerPoints.Points(Point(..))
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Helpers.DSL (ofThe, forThe, andThen, adjustedFor, andThe)
import CornerPoints.VerticalFaces(createHorizontallyAlignedCubesNoSlope)
import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), (|@+++#@|), (&+++#@), CornerPointsBuilder(..))




{- |Process all 36 images, reducing down to MultiDegreeRadii of the red laser line.
This is the red laser line in pixels Right of Left hand edge.
It is written out to json as a MultiDegreeRadii, even though the data is
in the form of pixel indices RightOfLHS, in other words, not processed at all beyond finding the laser line.

This file is full of null values, as I am now attempting to have an open area above and below the shape being scanned. This is to
make it easier to scan, by not having to worry about the scan object exactly filling the images.


-}
--process10DegreeImagesToMultiDegreeRadii has now been replaced by processImagesIntoFull360DegreeMultiDegreeRadiiAt10DegreeIntervals
--processImagesToRedLaserLineAsPixelsRightOfLHS = process10DegreeImagesToMultiDegreeRadii (getRedLaserLineSingleImage redLaserLine)


{- |Now replace all the null values by supplying a starting value. All [Radius] which start with a null, will use this starting value.
After that, each null value will use the preceding Radius.
I chose the value 2500 as it is larger than any value found on the original scan. So when I view this object, I can tell that the
largest cicumferences will be the null values that were replaced.

Write the file back to json.

-}
removeNullValues = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  case (decode contents) of
      Just (MultiDegreeRadii name' degrees') ->
        let multiDegreeRadii = resetMultiDegreeRadiiIfNullWithPreviousValue 2500 $ MultiDegreeRadii name' degrees'
        in  BL.writeFile "src/Data/scanFullData.json" $ encode $ multiDegreeRadii
        
      Nothing                              -> putStrLn "scanFullData.json not read"


{-Now convert the average pixel position of the red laser line, as measured from lhs, into radius as millimeters.
This has to calculate:
 -where the center position is for the current row
 -how many pixels right of center is the measured pixel location
 -convert this value into millimeters by using calibration measurements, and the angle of the laser/camera.
-}
convertPixelsToRadiiForMultiDegreeRadii ::   IO ()
convertPixelsToRadiiForMultiDegreeRadii   = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let multiDegreeRadii =
              MultiDegreeRadii
                name'
                [ convertPixelsToRadiiForASingleDegreeRadii currSingleDegreeRadii
                  | currSingleDegreeRadii <- degrees'
                ]
               
        in
           BL.writeFile "src/Data/scanFullData.json" $ encode $ multiDegreeRadii
           
      Nothing                              -> putStrLn "Nothing"


toeStl :: Double ->  MultiDegreeRadii -> IO ()
toeStl  rowReductionFactor multiDegreeRadii = 
  let triangles = 
                   [FacesBackFront | x <- [1..]] :
                   [[FacesAll | x <- [1..]] | y <- [1..]]
                  
                  ||+++^||
                  (createHorizontallyAlignedCubesNoSlope (Point 0 0 100) multiDegreeRadii [0,pixelsPerMM..])
                  
      stlFile = newStlShape "snowboard toe"  triangles
  in  writeStlToFile stlFile
      --putStrLn "file written"


{-
read in the Multidegree json file, which has valid Radii,
and process it into stl using whatever function required for the current shape. -}
loadMDRAndPassToProcessor :: IO ()
loadMDRAndPassToProcessor = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  let takeTopRows :: MultiDegreeRadii -> MultiDegreeRadii
      takeTopRows (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' [(SingleDegreeRadii degree'' (take 10 $ drop 1500 radii''))  | (SingleDegreeRadii degree'' radii'') <- degrees']
      rowReductionFactor = 100
      --extensionHeight = 30
      --plateRadius = 30
      --power = 2.5
      --lengthenYFactor = 20
      --extensionFaceBuilder :: (Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]
      --extensionFaceBuilder leftFace emptyFaces rightFace fillerFaces =
      --  [leftFace] ++ [emptyFaces | x <- [2..30]] ++ [rightFace]   ++  [fillerFaces | x <- [31..]]
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --enlarge it to fit over the socket already printed with WalkerSocket. 1st attempt at +2 was not quite big enough, trying 3.
            --rotate it to line up better with the riser
            --innerSleeveMDR = rotateMDR $ rotateMDR $ rotateMDR $ transpose (+3) $ reduceScan rowReductionFactor $ removeDefectiveTopRow (MultiDegreeRadii name' degrees')
            --give it a thickness of 3 mm
            --outerSleeveMDR = transpose (+3) innerSleeveMDR
            --plateRadius = 24
            
        in  --mainSocketStl innerSleeveMDR outerSleeveMDR extensionFaceBuilder extensionHeight rowReductionFactor pixelsPerMM
            --pushPlate plateRadius power lengthenYFactor
            --hosePlate plateRadius power lengthenYFactor
            toeStl 1 $  takeTopRows  (MultiDegreeRadii name' degrees')
            
      Nothing                                ->
        putStrLn "File not decoded"

-- ================================================ support functions =====================================
convertPixelsToRadiiForASingleDegreeRadii :: SingleDegreeRadii -> SingleDegreeRadii
convertPixelsToRadiiForASingleDegreeRadii    (SingleDegreeRadii degree' radii')    =
  let adjustForCenter = getThePixelsRightOfCenter $ andThen  (removeLeftOfCenterPixels btmCenterIndex topCenterIndex imageHeightPx)
  in
      SingleDegreeRadii degree'
         [
          let currLaserLineIndex = round $ radius currDegree
              rightOfCenterPixelDistanceOfLaser = adjustForCenter  (forThe currLaserLineIndex ) (ofThe currRow)
              radius'' = calculateRadiusFrom  rightOfCenterPixelDistanceOfLaser (adjustedFor pixelsPerMM) $ andThe cameraAngle
              
          in radius''
         | currDegree <- radii'
         | currRow <- [0..]
         ]




type TargetValue = Word8
type RowReductionFactor = Int

redLaserLine :: TargetValue
redLaserLine = 175

cameraAngle = 30 --but was it 30.
--The red laser line index from center.jpg as found by Scan.ParseJuicy.showFirstAndLastCenterOfRedLaser
topCenterIndex = 1293.5
btmCenterIndex = 1403.3


imageHeightPx = 1944

pixelsPerMM = calculatePixelsPerMillmeter 2592 390 37 107


