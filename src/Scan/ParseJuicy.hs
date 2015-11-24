{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ParallelListComp #-}
module Scan.ParseJuicy(process10DegreeImagesToMultiDegreeRadii,  TargetValueIndex(..), ofThe, forThe, andThen, adjustedFor, andThe,
                        getThePixelsRightOfCenter, removeLeftOfCenterPixels, getRedLaserLineSingleImage, convertPixelsToMillmeters,
                        calculateRadiusFrom, reduceScanRows, reduceRows, reduceScan, averageValueOf) where
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

--ToDo: Create a DSL module, perhaps in a helper folder, for DSL helpers like this.
ofThe = id
forThe = id
andThen = id
adjustedFor = id
andThe = id

{- |Calculate the Radius, which is the hypotenuse, of angle created by a CameraAngle, and the number of pixels right of center.
This is the actual radius of the scanned object for the current degree.
-}
calculateRadiusFrom :: PixelOffset ->   PixelsPerMillimeter -> CameraAngle ->  Radius
calculateRadiusFrom    offsetFromCenter pixelsPerMillimeter    cameraAngle  =
  let convertPixelsToMillmetersForThe = convertPixelsToMillmeters pixelsPerMillimeter
      numberOfRadiusPixels = (offsetFromCenter / (sinDegrees cameraAngle))
  in  Radius $ convertPixelsToMillmetersForThe  numberOfRadiusPixels

{-
calculateRadiusFromPixelsRightOfCenter :: PixelsPerMillimeter ->  PixelOffset -> CameraAngle ->  Radius
calculateRadiusFromPixelsRightOfCenter    pixelsPerMillimeter     numberOfPixels    cameraAngle  =
  let convertPixelsToMillmetersForThe = convertPixelsToMillmeters pixelsPerMillimeter
      numberOfRadiusPixels = (numberOfPixels / (sinDegrees cameraAngle))
  in  Radius $ convertPixelsToMillmetersForThe  numberOfRadiusPixels
-}



{- | Convert pixels to millimeters given the  pixels/millimeter and the number of pixels.-}
convertPixelsToMillmeters ::   PixelsPerMillimeter  -> NumberOfPixels -> Millimeters
convertPixelsToMillmeters      pixelsPerMillimeter     pixels         =
  pixels / pixelsPerMillimeter


removeLeftOfCenterPixels :: CenterIndex -> CenterIndex -> RowIndex -> RowIndex -> AdjustmentFactor
removeLeftOfCenterPixels    btmCenterIndex topCenterIndex totalRows   currentRow =
  let pixelOffset = btmCenterIndex - topCenterIndex
      
  in
    topCenterIndex +  ((pixelOffset * (fromIntegral currentRow))  /(fromIntegral totalRows))



getThePixelsRightOfCenter :: (RowIndex ->  AdjustmentFactor ) -> ColumnIndex ->        RowIndex ->   TargetValueIndex
getThePixelsRightOfCenter    centerAdjustment                    colIndexOfTargeValue  rowIndex         =
  (fromIntegral colIndexOfTargeValue) - (centerAdjustment rowIndex)

{- | Shows the index of the red laser line for an image.

Handy to have a look at the alignment of the camera and laser.
Take a picture with something flat, facing the camera, on the origin.
Not used during processing of a scan.-}
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


{- |
Process the images from src/Data/scanImages, into a json file of multiDegreeRadii.
Needs an edgeDetector passed in, such as detecting the red laser line.
-}
--processScanImagesToMultiDegreeRadiiOfRedLaserLine = process10DegreeImagesToMultiDegreeRadii (getRedLaserLineSingleImage redLaserLine)
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
  

  let
      singleDegreeRadiiList :: [SingleDegreeRadii]
      singleDegreeRadiiList =
        [
          setSingleDegreeRadii currImage currDegree
          | currDegree <- [0,10..350]
          | currImage <- [jpegImage0, jpegImage10, jpegImage20, jpegImage30, jpegImage40, jpegImage50, jpegImage60, jpegImage70, jpegImage80, jpegImage90, jpegImage100, jpegImage110,
                          jpegImage120, jpegImage130, jpegImage140, jpegImage150, jpegImage160, jpegImage170, jpegImage180, jpegImage190, jpegImage200, jpegImage210, jpegImage220, jpegImage230,
                          jpegImage240, jpegImage250, jpegImage260, jpegImage270, jpegImage280, jpegImage290, jpegImage300, jpegImage310, jpegImage320, jpegImage340,
                          jpegImage340, jpegImage350] --, jpegImage10] Removed the last one, as it must be an error. Did not test it out yet.
       ]
       ++
       --read in the 0 degree image again as the 360 degree image, to ensure they are exactly the same.
       case jpegImage0 of
            Left err -> [(SingleDegreeRadii 360 [Radius 0])] 
            Right (ImageYCbCr8 jpegImage0') -> [(SingleDegreeRadii 360 (map (Radius) (edgeDetector jpegImage0')))]
            --Right (ImageYCbCr8 jpegImage0') -> [(SingleDegreeRadii 360 (map (setRadius) (edgeDetector jpegImage0')))]
        
      multiDegreeRadii = MultiDegreeRadii "theName" singleDegreeRadiiList
  BL.writeFile "src/Data/scanFullData.json" $ encode $ multiDegreeRadii
  putStrLn "done"
  
  where setSingleDegreeRadii imageAsRead currDegree =
           case imageAsRead of
             Left err -> (SingleDegreeRadii currDegree [Radius 0]) 
             Right (ImageYCbCr8 jpegImage) -> (SingleDegreeRadii currDegree (map (Radius) (edgeDetector jpegImage)))
             --Right (ImageYCbCr8 jpegImage) -> (SingleDegreeRadii currDegree (map (setRadius) (edgeDetector jpegImage)))

        readImage' fileName = readImage $ filePathBuilder fileName

{-
==============================================================================================
This runs ok, but has not implemented the new calculateRadiusFromPixelsRightOfCenter
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
  

  let
      singleDegreeRadiiList :: [SingleDegreeRadii]
      singleDegreeRadiiList =
        [
          setSingleDegreeRadii currImage currDegree
          | currDegree <- [0,10..350]
          | currImage <- [jpegImage0, jpegImage10, jpegImage20, jpegImage30, jpegImage40, jpegImage50, jpegImage60, jpegImage70, jpegImage80, jpegImage90, jpegImage100, jpegImage110,
                          jpegImage120, jpegImage130, jpegImage140, jpegImage150, jpegImage160, jpegImage170, jpegImage180, jpegImage190, jpegImage200, jpegImage210, jpegImage220, jpegImage230,
                          jpegImage240, jpegImage250, jpegImage260, jpegImage270, jpegImage280, jpegImage290, jpegImage300, jpegImage310, jpegImage320, jpegImage340,
                          jpegImage340, jpegImage350] --, jpegImage10] Removed the last one, as it must be an error. Did not test it out yet.
       ]
       ++
       --read in the 0 degree image again as the 360 degree image, to ensure they are exactly the same.
       case jpegImage0 of
            Left err -> [(SingleDegreeRadii 360 [Radius 0])] 
            Right (ImageYCbCr8 jpegImage0') -> [(SingleDegreeRadii 360 (map (Radius) (edgeDetector jpegImage0')))]
        
      multiDegreeRadii = MultiDegreeRadii "theName" singleDegreeRadiiList
  BL.writeFile "src/Data/scanFullData.json" $ encode $ multiDegreeRadii
  putStrLn "done"
  
  where setSingleDegreeRadii imageAsRead currDegree =
           case imageAsRead of
             Left err -> (SingleDegreeRadii currDegree [Radius 0]) 
             Right (ImageYCbCr8 jpegImage) -> (SingleDegreeRadii currDegree (map (Radius) (edgeDetector jpegImage)))

        readImage' fileName = readImage $ filePathBuilder fileName

===========================================================================================================
This version uses Control.Monad to try to automate things, but always get the Left err msg out of the monad.
Should try to write the json from within the monad, to see it it works ok.
Keep this around to try it out.
Probably obsolete as I should try using parallel monad as per parallel haskell book.

See http://learnyouahaskell.com/input-and-output for info about using Control.Monad

processMultiDegreeImagesToRedLaserLine = do
  --degree list must end with the last degree read, prior to the 360 degree. Must start with 0
  singleDegreeRadiis  <- forM [0,10..350] (\currDegree -> do
    jpegImage <-  readImage' $ show currDegree
    case jpegImage of
            Left err -> return  (SingleDegreeRadii currDegree [Radius 0]) 
            Right (ImageYCbCr8 jpegImage') -> return (SingleDegreeRadii currDegree (map (Radius) (getRedLaserLineSingleImage jpegImage'))))
    

  jpegImage360 <- readImage' "0"
  let
    lastSingleDegreeRadii =
      case jpegImage360 of
            Left err -> [(SingleDegreeRadii 360 [Radius 0])] 
            Right (ImageYCbCr8 jpegImage360') -> [(SingleDegreeRadii 360 (map (Radius) (getRedLaserLineSingleImage jpegImage360')))]
     
    multiDegreeRadii = MultiDegreeRadii "theName" (singleDegreeRadiis ++ lastSingleDegreeRadii)

  BL.writeFile "src/Data/scanFullData.json" $ encode $ multiDegreeRadii
  putStrLn "done"
  
  where setSingleDegreeRadii imageAsRead currDegree =
           case imageAsRead of
             Left err -> (SingleDegreeRadii currDegree [Radius 0]) 
             Right (ImageYCbCr8 jpegImage) -> (SingleDegreeRadii currDegree (map (Radius) (getRedLaserLineSingleImage jpegImage)))

        readImage' fileName = readImage $ filePathBuilder fileName

-}



    
    


filePathBuilder :: FilePath -> FilePath
filePathBuilder fileName = "src/Data/scanImages/" ++ fileName ++ ".JPG"
{-

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
{-
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
         filePath = "src/Data/IMG_2457.JPG"

         --redLaserLine :: TargetValue
         --redLaserLine = 160
-}


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
