{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ParallelListComp #-}
module Scan.ParseJuicy(process10DegreeImagesToMultiDegreeRadii,  TargetValueIndex(..), 
                        getThePixelsRightOfCenter, removeLeftOfCenterPixels, getRedLaserLineSingleImage, convertPixelsToMillmeters,
                        calculateRadiusFrom, reduceScanRows, reduceRows, reduceScan, averageValueOf, calculatePixelsPerMillmeter,
                        process10DegreeImagesToMultiDegreeRadiiBaseUsingMap
                        ) where
import Codec.Picture.Jpg
import Codec.Picture
import Codec.Picture.Types
--import Scan.Transform(pixelIndicesOfPixelValuesLTE)
--import Scan.Parse (PixelValue(..))
--import Data.Convertible.Instances.Num(safeConvert)
import qualified Data.List as L
import Data.Word(Word8)
import qualified Data.ByteString as BS
import CornerPoints.Radius(SingleDegreeRadii(..), Radius(..), MultiDegreeRadii(..), resetMultiDegreeRadiiIfNullWithPreviousValue)
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import Scan.Json
import Data.Aeson
import Math.Trigonometry(sinDegrees )
import Helpers.DSL (ofThe, forThe, andThen, adjustedFor, andThe,)
import Control.Parallel
import Control.Parallel.Strategies(evalList, parList,rseq, using )
import Control.Monad
import qualified Data.Map as Map

--to make signatures more readable
type RedValue = Word8
type AvgIndexPosition = Double
type TargetValueIndex = Double
type CenterIndex = Double
type RowIndex = Int
type TargetValue = Word8
type ColumnIndex = Int
type AdjustmentFactor = Double
type NumberOfPixels = Double
type PixelsPerMillimeter = Double
type Millimeters = Double
type CameraAngle = Double
type PixelOffset = Double
type Degree = Int
type FileName = String
type FileExtension = String
--type FileNamePrefix = String


{- |Calculate the Radius, which is the hypotenuse, of angle created by a CameraAngle, and the number of pixels right of center.
This is the actual radius of the scanned object for the current degree.
-}
calculateRadiusFrom :: PixelOffset ->   PixelsPerMillimeter -> CameraAngle ->  Radius
calculateRadiusFrom    offsetFromCenter pixelsPerMillimeter    cameraAngle  =
  let convertPixelsToMillmetersForThe = convertPixelsToMillmeters pixelsPerMillimeter
      numberOfRadiusPixels = (offsetFromCenter / (sinDegrees cameraAngle))
  in  Radius $ convertPixelsToMillmetersForThe  numberOfRadiusPixels


{- | Convert pixels to millimeters given the  pixels/millimeter and the number of pixels.-}
convertPixelsToMillmeters ::   PixelsPerMillimeter  -> NumberOfPixels -> Millimeters
convertPixelsToMillmeters      pixelsPerMillimeter     pixels         =
  pixels / pixelsPerMillimeter


{- |Calculate the pixels/mm of an image based on a calibration image, of an object of know dimensions.
Can calculate this based on either the width or the height, as they will give the same value.

-}
calculatePixelsPerMillmeter :: NumberOfPixels ->  Millimeters -> Millimeters          -> Millimeters          -> PixelsPerMillimeter 
calculatePixelsPerMillmeter    imageWidthPx       imageWidthMM   objectWidthReadWorldMM  objectWidthOnScreenMM =
  ((objectWidthOnScreenMM * imageWidthPx) / imageWidthMM) / objectWidthReadWorldMM 

removeLeftOfCenterPixels :: CenterIndex -> CenterIndex -> RowIndex -> RowIndex -> AdjustmentFactor
removeLeftOfCenterPixels    btmCenterIndex topCenterIndex totalRows   currentRow =
  let pixelOffset = btmCenterIndex - topCenterIndex
      
  in
    topCenterIndex +  ((pixelOffset * (fromIntegral currentRow))  /(fromIntegral totalRows))



getThePixelsRightOfCenter :: (RowIndex ->  AdjustmentFactor ) -> ColumnIndex ->        RowIndex ->   TargetValueIndex
getThePixelsRightOfCenter    centerAdjustment                    colIndexOfTargeValue  rowIndex         =
  (fromIntegral colIndexOfTargeValue) - (centerAdjustment rowIndex)

{- | Shows the index of the red laser line for an image, at the 1st and last row
Take a picture with something flat, facing the camera, on the origin.
Used during processing of a scan, to correct for mis-alignment of the laser and the camera.-}
--TargetValue
showFirstAndLastCenterOfRedLaser = do
  jpegImage <-   readImage "src/Data/scanImages/center.JPG"
  let
    showAverageIndexOfTargetValuesForRow =
          showAverageIndexOfTargetValuesForRowBase jpegImage indicesOfThePixelValuesGTE redLaserLine

    lastRowIndex = 
      case jpegImage of
        Right (ImageYCbCr8 jpegImage') -> (imageHeight jpegImage') - 2
 
  case (showAverageIndexOfTargetValuesForRow 0) of
     Right center -> putStrLn $ show center
     Left err     -> putStrLn err

  case (showAverageIndexOfTargetValuesForRow lastRowIndex) of
     Right center -> putStrLn $ show center
     Left err     -> putStrLn err

 
    

{-
For a single row of an image:
EdgeDetect a set of values, and return the average index.

Used by:
showFirstAndLastCenterOfRedLaser
-}

showAverageIndexOfTargetValuesForRowBase :: (Either String (DynamicImage )) -> (TargetValue -> [TargetValue] -> [ColumnIndex]) ->  TargetValue -> RowIndex ->  (Either String AvgIndexPosition)
showAverageIndexOfTargetValuesForRowBase    (Right(ImageYCbCr8 jpegImage'))    indicesOf                       targetValue    targetRow     = do
          Right $ averageValueOf $  indicesOf targetValue $ readAllcolumns jpegImage'
      where
         readAllcolumns img' =
           [extractCR $ pixelAt img' x targetRow |  x <- [0..(imageWidth img')]]

         
         extractCR (PixelYCbCr8 _ _ cr) =
           pixel8ToWord8 cr

showAverageIndexOfTargetValuesForRowBase    (Left err)  _ _ _ = do
  Left err

{- |

Used for:
Curry in the value for the red laser line, and pass it into process10DegreeImagesToMultiDegreeRadii
as the ((Image  PixelYCbCr8) -> [TargetValueIndex]) edgeDetector, when processing a scan using the red laser.

-}
getRedLaserLineSingleImage :: TargetValue -> (Image  PixelYCbCr8) -> [TargetValueIndex]
getRedLaserLineSingleImage    redLaserLine   jpegImage = 
  processSingleImageToReducedEachRowToTargetValueIndex jpegImage (extractCR) (averageValueOf) (indicesOfThePixelValuesGTE) redLaserLine

--just for testing. Wrapper around processSingleDegreeToMultiDegreeRadii
processSingleImageToMultiDegreeRadiiJsonFileWrapper = processSingleImageToMultiDegreeRadiiJsonFile (getRedLaserLineSingleImage redLaserLine)
{-
Just for testing.
Process and write to json, a single image into a red laser line.
Clean out any Radius null values before saving.
-}
processSingleImageToMultiDegreeRadiiJsonFile :: ((Image  PixelYCbCr8) -> [TargetValueIndex]) -> IO ()
processSingleImageToMultiDegreeRadiiJsonFile     edgeDetector   = do

  jpegImage0 <-   readImage' "0"
  
  let
      singleDegreeRadiiList :: [SingleDegreeRadii]
      singleDegreeRadiiList =
        [
          case jpegImage0 of
            Left err -> (SingleDegreeRadii 360 [Radius 0])
            --Do without cleaning out the Radius null values.
            Right (ImageYCbCr8 jpegImage0') -> (SingleDegreeRadii 360 (map (Radius) (edgeDetector jpegImage0')))
            
        ]
       
      --Clean out any Radius null values and save to json.  
      multiDegreeRadii = resetMultiDegreeRadiiIfNullWithPreviousValue 10 $ MultiDegreeRadii "theName" singleDegreeRadiiList
  BL.writeFile "src/Data/scanFullData.json" $ encode $ multiDegreeRadii
  putStrLn "done"
  
  where 

        readImage' fileName = readImage $ filePathBuilder fileName




{-
Work through each row of the image running
  (toASingleIndexValueFrom (pixelsThatQualifyAsA) targetValue)
over each row of the image.
-}
processSingleImageToReducedEachRowToTargetValueIndex:: (Image  PixelYCbCr8) -> ((PixelYCbCr8) -> Word8) ->  ([Int] -> Double)   ->         ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  [TargetValueIndex]
processSingleImageToReducedEachRowToTargetValueIndex   jpegImage                extractColor                 reduceIndexesOfTargetPixels     pixelsThatQualifyAsA           targetValue  = 
      reduceEachRowOfThe jpegImage  (toASingleIndexValueFrom (pixelsThatQualifyAsA) targetValue)  
        
      where
         toASingleIndexValueFrom :: ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  RowIndex -> (Image  PixelYCbCr8) -> TargetValueIndex
         toASingleIndexValueFrom    pixelsThatQualifyAsA'                  targetValue     row         img'  =
           reduceIndexesOfTargetPixels $  pixelsThatQualifyAsA'  targetValue  [ extractColor $ pixelAt img' x row |  x <- [0..((imageWidth img')-1)]]

         reduceEachRowOfThe :: (Image  PixelYCbCr8) ->  ( RowIndex -> (Image  PixelYCbCr8) ->  TargetValueIndex) ->  [TargetValueIndex]
         reduceEachRowOfThe    img''                    reducer                                                       =
           [reducer  y img'' | y <-  [0..((imageHeight img'')-1)]]



         

showAverageOfIndicesOfLaserLineValuesForEachRow:: IO ()
showAverageOfIndicesOfLaserLineValuesForEachRow  = do
      jpegImage <-   readImage filePath
      case  jpegImage of
        Left err -> putStrLn err
        Right (ImageYCbCr8 jpegImage') ->
          putStrLn $ show $ reduceEachRowOfThe jpegImage'  (toTheAvgOfThe (indicesOfThePixelValuesGTE) redLaserLine)  
        otherwise -> putStrLn "another format"
      where
         toTheAvgOfThe :: ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  RowIndex -> (Image  PixelYCbCr8) -> AvgIndexPosition
         toTheAvgOfThe    pixelValuesGTE                  targetValue     row         img'  =
           averageValueOf $  pixelValuesGTE  targetValue  [ extractCR $ pixelAt img' x row |  x <- [0..((imageWidth img')-1)]]

         reduceEachRowOfThe :: (Image  PixelYCbCr8) ->  ( RowIndex -> (Image  PixelYCbCr8) ->  AvgIndexPosition) ->  [AvgIndexPosition]
         reduceEachRowOfThe    img''                    reducer                                                       =
           [reducer  y img'' | y <-  [0..((imageHeight img'')-1)]]

         extractCR :: (PixelYCbCr8) -> RedValue
         extractCR (PixelYCbCr8 _ _ cr) =
           pixel8ToWord8 cr

         filePath :: FilePath
         filePath = "src/Data/scanImages/140.JPG"

         --redLaserLine :: TargetValue
         --redLaserLine = 160

showFullColorsFor1Row = do
      contents <-   readImage "src/Data/IMG_2457.JPG"
      case  contents of
        Left err -> putStrLn err
        Right (ImageYCbCr8 img) ->
          print $ show $ readRow img
        otherwise -> putStrLn "another format"
       where
         firstRow = 1
         readRow img' =
           [pixelAt img' currCol firstRow |  currCol <- [0..(imageWidth img')]]
   
{-
https://ocharles.org.uk/blog/posts/2013-12-16-24-days-of-hackage-repa.html
-read an image in as a bitmap, and put it into a repa array
-}


--type ThreshholdValue = Double
{- hoogle findIndices to see the word8 bytestring version.
   Instead of list comprehension, will be able to use findIndices alone.
-}
indicesOFPixelValuesLTE :: Word8 -> [Word8] -> [Int]
indicesOFPixelValuesLTE thresholdValue rawData  = ( L.findIndices) (<=thresholdValue) rawData

indicesOfThePixelValuesGTE :: Word8 -> [Word8] -> [Int]
indicesOfThePixelValuesGTE thresholdValue rawData  = ( L.findIndices) (>=thresholdValue) rawData



pixel8ToWord8 :: Pixel8 -> Word8
pixel8ToWord8 pixel = pixel

-- |Get the average value of a [Int].
--  Return NaN if the list was empty.
-- Used to  get the average pixels indices for target values such as the red laser line.
averageValueOf :: [Int] -> Double
averageValueOf list =
  (fromIntegral $ L.sum list)  / (fromIntegral $ length list)
  


redLaserLine :: TargetValue
redLaserLine = 175

extractCR :: (PixelYCbCr8) -> Word8
extractCR (PixelYCbCr8 _ _ cr) =  pixel8ToWord8 cr


reduceScanRows :: Int -> MultiDegreeRadii -> Either String MultiDegreeRadii
reduceScanRows 0 _ = Left "Can't use 0 for row reduction"
reduceScanRows reduceFactor scan
  | reduceFactor > (length $ radii $ head $ degrees scan)  = Left "reduction factor > number of rows"
  | otherwise =
     let degreesReduced = [ SingleDegreeRadii {degree=(degree x),  radii = (reduceRows reduceFactor $ radii x)} | x <- degrees scan]
     in  Right $  scan {degrees=degreesReduced}

reduceScan :: Int -> MultiDegreeRadii -> MultiDegreeRadii
reduceScan reduceFactor scan =
  
     let degreesReduced = [ SingleDegreeRadii {degree=(degree x),  radii = (reduceRows reduceFactor $ radii x)} | x <- degrees scan]
     in  scan {degrees=degreesReduced}

{- Take every xth element from a list.
Used for: reduce rows from the raw source data, as the usual 480 rows from an
image, is probably overkill for a 3D printer.

 (mod counter factor == 0) is saying: divide the current row index by the reduction factor, and if there is no
 remainder, include the row.

Starts of with a row index of 1, which is passed into reduceRows' to do the actual recursive work.

Must be something in Data.List such as scanl or foldl that would do this same thing.
-}
reduceRows :: RowReductionFactor -> [a] -> [a]
reduceRows factor x = reduceRows' factor 1 x

reduceRows' :: RowReductionFactor -> Int ->[a] -> [a]
reduceRows' _ _ [] = []
reduceRows' 0 _ _ = []
reduceRows' factor counter (x:xs)
        | (mod counter factor == 0) = x : reduceRows' factor (counter + 1) xs
        | otherwise            = reduceRows' factor (counter + 1) xs

-- |Factor by which to reduce rows.
type RowReductionFactor = Int







--ToDo: Dynamically build the image name so that an input of the firt file name can be put in and it will read the files
--without having to rename them all.
{- |
Process the images from src/Data/scanImages, into a json file of multiDegreeRadii.
Takes edgeDetector, such as detecting the red laser line.

Needs to be run from an executable so that multi processors can be used.
See the executable parallelProcess that is part of this cabal project, on how to run in parallel.
With parallel: ~15 seconds
Without      : ~3 minutes
-}
process10DegreeImagesToMultiDegreeRadii :: ((Image  PixelYCbCr8) -> [TargetValueIndex]) -> IO ()
process10DegreeImagesToMultiDegreeRadii     edgeDetector   = do
  jpegImage0 <-   readImage' "0"
  jpegImage10 <-  readImage' "10"
  jpegImage20 <-  readImage' "20"
  jpegImage30 <-  readImage' "30"
  jpegImage40 <-  readImage' "40"
  jpegImage50 <-  readImage' "50"
  jpegImage60 <-  readImage' "60"
  jpegImage70 <-  readImage' "70"
  jpegImage80 <-  readImage' "80"
  jpegImage90 <-  readImage' "90"
  jpegImage100 <-  readImage' "100"
  jpegImage110 <-  readImage' "110"
  jpegImage120 <-  readImage' "120"
  jpegImage130 <-  readImage' "130"
  jpegImage140 <-  readImage' "140"
  jpegImage150 <-  readImage' "150"
  jpegImage160 <-  readImage' "160"
  jpegImage170 <-  readImage' "170"
  jpegImage180 <-  readImage' "180"
  jpegImage190 <-  readImage' "190"
  jpegImage200 <-  readImage' "200"
  jpegImage210 <-  readImage' "210"
  jpegImage220 <-  readImage' "220"
  jpegImage230 <-  readImage' "230"
  jpegImage240 <-  readImage' "240"
  jpegImage250 <-  readImage' "250"
  jpegImage260 <-  readImage' "260"
  jpegImage270 <-  readImage' "270"
  jpegImage280 <-  readImage' "280"
  jpegImage290 <-  readImage' "290"
  jpegImage300 <-  readImage' "300"
  jpegImage310 <-  readImage' "310"
  jpegImage320 <-  readImage' "320"
  jpegImage330 <-  readImage' "330"
  jpegImage340 <-  readImage' "340"
  jpegImage350 <-  readImage' "350"
   
    
      
      --process the images in parallel.
  
      
      
  
  
  let

      --put images in a data structure along with the degree
      --so that they can processed in parallel.
      jpegImage0WithDegree = ImageWithDegree jpegImage0 0
      jpegImage10WithDegree = ImageWithDegree jpegImage10 10
      jpegImage20WithDegree = ImageWithDegree jpegImage20 20
      jpegImage30WithDegree = ImageWithDegree jpegImage30 30
      jpegImage40WithDegree = ImageWithDegree jpegImage40 40
      jpegImage50WithDegree = ImageWithDegree jpegImage50 50
      jpegImage60WithDegree = ImageWithDegree jpegImage60 60
      jpegImage70WithDegree = ImageWithDegree jpegImage70 70
      jpegImage80WithDegree = ImageWithDegree jpegImage80 80
      jpegImage90WithDegree = ImageWithDegree jpegImage90 90
      jpegImage100WithDegree = ImageWithDegree jpegImage100 100
      jpegImage110WithDegree = ImageWithDegree jpegImage110 110
      jpegImage120WithDegree = ImageWithDegree jpegImage120 120
      jpegImage130WithDegree = ImageWithDegree jpegImage130 130
      jpegImage140WithDegree = ImageWithDegree jpegImage140 140
      jpegImage150WithDegree = ImageWithDegree jpegImage150 150
      jpegImage160WithDegree = ImageWithDegree jpegImage160 160
      jpegImage170WithDegree = ImageWithDegree jpegImage170 170
      jpegImage180WithDegree = ImageWithDegree jpegImage180 180
      jpegImage190WithDegree = ImageWithDegree jpegImage190 190
      jpegImage200WithDegree = ImageWithDegree jpegImage200 200
      jpegImage210WithDegree = ImageWithDegree jpegImage210 210
      jpegImage220WithDegree = ImageWithDegree jpegImage220 220
      jpegImage230WithDegree = ImageWithDegree jpegImage230 230
      jpegImage240WithDegree = ImageWithDegree jpegImage240 240
      jpegImage250WithDegree = ImageWithDegree jpegImage250 250
      jpegImage260WithDegree = ImageWithDegree jpegImage260 260
      jpegImage270WithDegree = ImageWithDegree jpegImage270 270
      jpegImage280WithDegree = ImageWithDegree jpegImage280 280
      jpegImage290WithDegree = ImageWithDegree jpegImage290 290
      jpegImage300WithDegree = ImageWithDegree jpegImage300 300
      jpegImage310WithDegree = ImageWithDegree jpegImage310 310
      jpegImage320WithDegree = ImageWithDegree jpegImage320 320
      jpegImage330WithDegree = ImageWithDegree jpegImage330 330
      jpegImage340WithDegree = ImageWithDegree jpegImage340 340
      jpegImage350WithDegree = ImageWithDegree jpegImage350 350
      jpegImage360WithDegree = ImageWithDegree jpegImage0 360 --this last degree is a copy of the first degree.

      multiDegreeRadii =
        map createSingleDegreeRadiiUingTheEdgeDetector 
         [currImage | currImage <- [(jpegImage0WithDegree), (jpegImage10WithDegree), (jpegImage20WithDegree), (jpegImage30WithDegree),(jpegImage40WithDegree),
                                    (jpegImage50WithDegree), (jpegImage60WithDegree), (jpegImage70WithDegree), (jpegImage80WithDegree), (jpegImage90WithDegree),
                                    (jpegImage100WithDegree), (jpegImage110WithDegree), (jpegImage120WithDegree), (jpegImage130WithDegree), (jpegImage140WithDegree),
                                    (jpegImage150WithDegree), (jpegImage160WithDegree), (jpegImage170WithDegree), (jpegImage180WithDegree), (jpegImage190WithDegree),
                                    (jpegImage200WithDegree), (jpegImage210WithDegree), (jpegImage220WithDegree), (jpegImage230WithDegree), (jpegImage240WithDegree),
                                    (jpegImage250WithDegree), (jpegImage260WithDegree), (jpegImage270WithDegree), (jpegImage280WithDegree), (jpegImage290WithDegree),
                                    (jpegImage300WithDegree), (jpegImage310WithDegree), (jpegImage320WithDegree), (jpegImage330WithDegree), (jpegImage340WithDegree),
                                    (jpegImage350WithDegree), (jpegImage360WithDegree)]]
          `using` parList rseq
      
      
      createSingleDegreeRadiiUingTheEdgeDetector imageWithDegree  =
           case imageWithDegree of
             (ImageWithDegree (Left(err))  deg') ->  (SingleDegreeRadii deg' [Radius 0])
             (ImageWithDegree (Right(ImageYCbCr8 jpegImage))  deg') -> (SingleDegreeRadii deg' (map (Radius) (edgeDetector jpegImage)))
             
        
      

      
  BL.writeFile "src/Data/scanFullData.json" $ encode $ multiDegreeRadii
  putStrLn "done"

  where 
    readImage' fileName = readImage $ filePathBuilder fileName
  
--Used by process10DegreeImagesToMultiDegreeRadiiMultiCore to associate an image with a degree.
data ImageWithDegree = ImageWithDegree {image :: (Either String DynamicImage), deg :: Double}









































------------------------------------------------- warning warning warning-----------------------------------------
--as soon as I export this function, compiler runs out of memory and locks up.


--try new filepath system.
--will replace process10DegreeImagesToMultiDegreeRadiiBase when done

process10DegreeImagesToMultiDegreeRadiiBaseUsingMap :: ((Image  PixelYCbCr8) -> [TargetValueIndex]) -> String ->       String -> String ->    Int            ->  IO ()
process10DegreeImagesToMultiDegreeRadiiBaseUsingMap     (edgeDetector)                                 fileNamePrefix  fileExtension filePath starterNumber  = do

  let
    filePathWithTrailingSlash 
      |  last filePath == '/' =  filePath
      |  otherwise            =  '/' : filePath

    readImage' currNumber = readImage $ filePathWithTrailingSlash ++ fileNamePrefix ++ (fileNameStarterNumber currNumber) ++ "." ++ fileExtension
    fileNameStarterNumber currNumber = show $ starterNumber + currNumber
    
                                       
    --fileMap = filePathBuilderBase "src/Data/scanImages/"  [2508..2543] [0,10..360]
    createSingleDegreeRadiiUingTheEdgeDetector imageWithDegree  =
           case imageWithDegree of
             (ImageWithDegree (Left(err))  deg') ->  (SingleDegreeRadii deg' [Radius 0])
             (ImageWithDegree (Right(ImageYCbCr8 jpegImage))  deg') -> (SingleDegreeRadii deg' (map (Radius) (edgeDetector jpegImage)))
  
  jpegImage0 <-   readImage' 0
  jpegImage10 <-  readImage' 1 
  jpegImage20 <-  readImage' 2
  jpegImage30 <-  readImage' 3
  jpegImage40 <-  readImage' 4
  jpegImage50 <-  readImage' 5
  jpegImage60 <-  readImage' 6
  jpegImage70 <-  readImage' 7
  jpegImage80 <-  readImage' 8
  jpegImage90 <-  readImage' 9
  jpegImage100 <-  readImage' 10
  jpegImage110 <-  readImage' 11
  jpegImage120 <-  readImage' 12
  jpegImage130 <-  readImage' 13
  jpegImage140 <-  readImage' 14
  jpegImage150 <-  readImage' 15
  jpegImage160 <-  readImage' 16
  jpegImage170 <-  readImage' 17
  jpegImage180 <-  readImage' 18
  jpegImage190 <-  readImage' 19
  jpegImage200 <-  readImage' 20
  jpegImage210 <-  readImage' 21
  jpegImage220 <-  readImage' 22
  jpegImage230 <-  readImage' 23
  jpegImage240 <-  readImage' 24
  jpegImage250 <-  readImage' 25
  jpegImage260 <-  readImage' 26
  jpegImage270 <-  readImage' 27
  jpegImage280 <-  readImage' 28
  jpegImage290 <-  readImage' 29
  jpegImage300 <-  readImage' 30
  jpegImage310 <-  readImage' 31
  jpegImage320 <-  readImage' 32
  jpegImage330 <-  readImage' 33
  jpegImage340 <-  readImage' 34
  jpegImage350 <-  readImage' 35
   
  
      
  BL.writeFile "src/Data/scanFullData.json" $ encode
     (map createSingleDegreeRadiiUingTheEdgeDetector 
         [currImage | currImage <- [(ImageWithDegree jpegImage0 0), (ImageWithDegree jpegImage10 10), (ImageWithDegree jpegImage20 20), (ImageWithDegree jpegImage30 30)
                                    , (ImageWithDegree jpegImage40 40), (ImageWithDegree jpegImage50 50), (ImageWithDegree jpegImage60 60), (ImageWithDegree jpegImage70 70)
                                    , (ImageWithDegree jpegImage80 80), (ImageWithDegree jpegImage90 90), (ImageWithDegree jpegImage100 100), (ImageWithDegree jpegImage110 110)
                                    , (ImageWithDegree jpegImage120 120), (ImageWithDegree jpegImage130 130), (ImageWithDegree jpegImage140 140), (ImageWithDegree jpegImage150 150)
                                    , (ImageWithDegree jpegImage160 160), (ImageWithDegree jpegImage170 170), (ImageWithDegree jpegImage180 180), (ImageWithDegree jpegImage190 190)
                                    , (ImageWithDegree jpegImage200 200), (ImageWithDegree jpegImage200 200), (ImageWithDegree jpegImage210 210), (ImageWithDegree jpegImage220 220)
                                    , (ImageWithDegree jpegImage230 230), (ImageWithDegree jpegImage240 240), (ImageWithDegree jpegImage250 250), (ImageWithDegree jpegImage260 260)
                                    , (ImageWithDegree jpegImage270 270), (ImageWithDegree jpegImage280 280), (ImageWithDegree jpegImage290 290), (ImageWithDegree jpegImage300 300)
                                    , (ImageWithDegree jpegImage310 310), (ImageWithDegree jpegImage320 320), (ImageWithDegree jpegImage330 330), (ImageWithDegree jpegImage340 340)
                                    , (ImageWithDegree jpegImage350 350), (ImageWithDegree jpegImage350 360)]]
                                    --360 uses image 0 so they are the same
     `using` parList rseq
     )
  putStrLn "done"
  
  where
   a = 'a'
       








--the original. Leave it alone for now, so everything compiles
filePathBuilder :: FilePath -> FilePath
filePathBuilder fileName = "src/Data/scanImages/" ++ fileName ++ ".JPG"
