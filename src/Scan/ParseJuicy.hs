{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ParallelListComp #-}
module Scan.ParseJuicy(process10DegreeImagesToMultiDegreeRadii, getRedLaserLineSingleImage) where
import Codec.Picture.Jpg
import Codec.Picture
import Codec.Picture.Types
--import Scan.Transform(pixelIndicesOfPixelValuesLTE)
import Scan.Parse (PixelValue(..))
--import Data.Convertible.Instances.Num(safeConvert)
import qualified Data.List as L
import Data.Word(Word8)
import qualified Data.ByteString as BS
import CornerPoints.VerticalFaces(SingleDegreeRadii(..), MultiDegreeRadii(..))
import CornerPoints.Create(Radius(..))
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import Scan.Json
import Data.Aeson

hello = "hello"

--https://www.reddit.com/r/haskellquestions/comments/2p0tk0/juicy_pixels_simple_example_code/
useReadImage :: IO ()
useReadImage = do
  contents <- readImage "src/Data/line.jpg"
  case contents of
   Left err -> putStrLn err
   Right _  -> putStrLn "decoded"


{-
shows the color info about a pixel
see:
  https://en.wikipedia.org/wiki/YCbCr
  about how to decode that data, to find my laser line.
-}
readJpegPixelValue = do
  contents <-   readImage "src/Data/line.jpg"
  case  contents of
   Left err -> putStrLn err
   Right (ImageYCbCr8 img) ->
     --convertImage img
     --putStrLn $ show $ imageWidth img
     putStrLn $ show $ pixelAt img 10 10 
   otherwise -> putStrLn "another format"


{-
get the greyscale Y values from the first row of the image.
-}
showBlackPixelValues = do
      contents <-   readImage "src/Data/line.jpg"
      case  contents of
        Left err -> putStrLn err
        Right (ImageYCbCr8 img) ->
          putStrLn $ show $ readAllcolumns img
        otherwise -> putStrLn "another format"
      where
         readAllcolumns img' =
           [extractY $ pixelAt img' x 10 |  x <- [0..(imageWidth img')]]

         extractY (PixelYCbCr8 y _ _) =
           y




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
showFirstAndLastCenterOfRedLaser = do
  jpegImage <-   readImage "src/Data/scanImages/center.JPG"
  case (showAverageIndexOfTargetValuesFor1RowBase jpegImage 0 redLaserLine) of
   Right center -> putStrLn $ show center
   Left err     -> putStrLn err

  case (showAverageIndexOfTargetValuesFor1RowBase jpegImage 10 redLaserLine) of
   Right center -> putStrLn $ show center
   Left err     -> putStrLn err
-}

--Word8 -> [Word8] -> [Int]
{-
For a single row of an image:
EdgeDetect a set of values, and return the average index.
-}
--TargetValue
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
{-
showAverageIndexOfTargetValuesForRowBase :: (Either String (DynamicImage )) -> (Word8 -> [Word8] -> [Int]) ->  TargetValue -> RowIndex ->  (Either String AvgIndexPosition)
showAverageIndexOfTargetValuesForRowBase    (Right(ImageYCbCr8 jpegImage'))    indicesOf                       targetValue    targetRow     = do
          Right $ averageValueOf $  indicesOf targetValue $ readAllcolumns jpegImage'
      where
         readAllcolumns img' =
           [extractCR $ pixelAt img' x targetRow |  x <- [0..(imageWidth img')]]

         
         extractCR (PixelYCbCr8 _ _ cr) =
           pixel8ToWord8 cr

showAverageIndexOfTargetValuesForRowBase    (Left err)  _ _ _ = do
  Left err
-}

{-
Shows that using Control.Monad, reads the image ok as it only returns success instead of error msg.

So why does the monad version of process10DegreeImagesToRedLaserLine return the err value.
-}
showMeTheErrorsForRedLaserLine = do
  --degree list must end with the last degree read, prior to the 360 degree. Must start with 0
  singleDegreeRadiis  <- forM [0,10..350] (\currDegree -> do
    jpegImage <-  readImage' $ show currDegree
    case jpegImage of
            Left err -> return  err
            Right (ImageYCbCr8 jpegImage') -> return "success"
    )

  
  print $ map (show) singleDegreeRadiis
  
  where setSingleDegreeRadii imageAsRead currDegree =
           case imageAsRead of
             Left err -> (SingleDegreeRadii currDegree [Radius 0]) 
             Right (ImageYCbCr8 jpegImage) -> (SingleDegreeRadii currDegree (map (Radius) (getRedLaserLineSingleImage redLaserLine jpegImage)))
        
        readImage' fileName = readImage $ filePathBuilder fileName


--showTopAndBottomTargetValue =
  


{- |
Process the images from src/Data/scanImages, into a json file of multiDegreeRadii.
Needs an edgeDetector passed, such as detecting the red laser line.
-}
--
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

{-
has getRedLaserLineSingleImage built right in, need another version where this could be brought in as a base function.
process10DegreeImagesToMultiDegreeRadiiOfRedLaserLine  = do
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
                          jpegImage340, jpegImage350, jpegImage10]
       ]
       ++
       --read in the 0 degree image again as the 360 degree image, to ensure they are exactly the same.
       case jpegImage0 of
            Left err -> [(SingleDegreeRadii 360 [Radius 0])] 
            Right (ImageYCbCr8 jpegImage0') -> [(SingleDegreeRadii 360 (map (Radius) (getRedLaserLineSingleImage jpegImage0')))]
        
      multiDegreeRadii = MultiDegreeRadii "theName" singleDegreeRadiiList
  BL.writeFile "src/Data/scanFullData.json" $ encode $ multiDegreeRadii
  putStrLn "done"
  
  where setSingleDegreeRadii imageAsRead currDegree =
           case imageAsRead of
             Left err -> (SingleDegreeRadii currDegree [Radius 0]) 
             Right (ImageYCbCr8 jpegImage) -> (SingleDegreeRadii currDegree (map (Radius) (getRedLaserLineSingleImage jpegImage)))

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
--TargetValue
getRedLaserLineSingleImage :: TargetValue ->       (Image  PixelYCbCr8) -> [TargetValueIndex]
getRedLaserLineSingleImage    laserLineTargetValue jpegImage = 
  processSingleImageToReducedEachRowToTargetValueIndex jpegImage (extractCR) (averageValueOf) (indicesOfThePixelValuesGTE) laserLineTargetValue
    

{-
Works, but it should have the redLaserLIne TargetValue passed in.
getRedLaserLineSingleImage :: (Image  PixelYCbCr8) -> [TargetValueIndex]
getRedLaserLineSingleImage jpegImage = 
  processSingleImageToReducedEachRowToTargetValueIndex jpegImage (extractCR) (averageValueOf) (indicesOfThePixelValuesGTE) redLaserLine
===========================================================================
getRedLaserLineSingleImage :: (Image  PixelYCbCr8) -> [TargetValueIndex]
getRedLaserLineSingleImage jpegImage = 
  processSingleImageToReducedEachRowToTargetValueIndex jpegImage (extractCR) (averageValueOf) (indicesOfThePixelValuesGTE) redLaserLine
====================================================================================
getRedLaserLineSingleImage :: FilePath -> IO ()
getRedLaserLineSingleImage filePath = do
  jpegImage <-   readImage filePath
  case jpegImage of
    Left err -> putStrLn err
    Right (ImageYCbCr8 jpegImage') -> print $ show $ processSingleImageToReducedEachRowToTargetValueIndex jpegImage' (extractCR) (averageValueOf) (indicesOfThePixelValuesGTE) redLaserLine
    otherwise -> putStrLn "another format"
===========================================
getRedLaserLineSingleImage :: FilePath -> IO ()
getRedLaserLineSingleImage filePath =
  print $ show $ processSingleImageToReducedEachRowToTargetValueIndex filePath (extractCR) (averageValueOf) (indicesOfThePixelValuesGTE) redLaserLine
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
         filePath = "src/Data/scanImages/90.JPG"

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


averageValueOf :: [Int] -> Double
averageValueOf list =
  (fromIntegral $ L.sum list)  / (fromIntegral $ length list)
  
type RedValue = Word8
type AvgIndexPosition = Double
type TargetValueIndex = Double
type RowIndex = Int
type TargetValue = Word8
type ColumnIndex = Int

redLaserLine :: TargetValue
redLaserLine = 190

extractCR :: (PixelYCbCr8) -> Word8
extractCR (PixelYCbCr8 _ _ cr) =  pixel8ToWord8 cr
