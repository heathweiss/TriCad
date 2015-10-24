module Scan.ParseJuicy() where
import Codec.Picture.Jpg
import Codec.Picture
import Codec.Picture.Types
--import Scan.Transform(pixelIndicesOfPixelValuesLTE)
import Scan.Parse (PixelValue(..))
--import Data.Convertible.Instances.Num(safeConvert)
import qualified Data.List as L
import Data.Word(Word8)
import qualified Data.ByteString as BS

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
         

showAverageLTE100RedPixelValuesFor1Row = do
      contents <-   readImage "src/Data/IMG_2457.JPG"
      case  contents of
        Left err -> putStrLn err
        Right (ImageYCbCr8 img) ->
          putStrLn $ show $ averageIndexValueOf $  pixelIndicesOfPixelValuesLTE  100 $ readAllcolumns img
          --putStrLn $ show $ readAllcolumns img
        otherwise -> putStrLn "another format"
      where
         readAllcolumns img' =
           [extractCR $ pixelAt img' x 10 |  x <- [0..(imageWidth img')]]

         
         extractCR (PixelYCbCr8 _ _ cr) =
           pixel8ToWord8 cr



showAverageOfIndicesOfRedValuesForEachRow:: IO ()
showAverageOfIndicesOfRedValuesForEachRow  = do
      jpegImage <-   readImage filePath
      case  jpegImage of
        Left err -> putStrLn err
        Right (ImageYCbCr8 img) ->
          putStrLn $ show $ reduceEachRowOfThe img  (toAvgOf (indicesOfPixelValuesGTE) redLaserLine)  
        otherwise -> putStrLn "another format"
      where
         toAvgOf :: ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  RowIndex -> (Image  PixelYCbCr8) -> AvgIndexPosition
         toAvgOf    pixelValuesGTE                  targetValue     row         img'  =
           averageIndexValueOf $  pixelValuesGTE  targetValue  [ extractCR $ pixelAt img' x row |  x <- [0..((imageWidth img')-1)]]

         reduceEachRowOfThe :: (Image  PixelYCbCr8) ->  ( RowIndex -> (Image  PixelYCbCr8) ->  AvgIndexPosition) ->  [AvgIndexPosition]
         reduceEachRowOfThe    img''                    reducer  = [reducer  y img'' | y <-  [0..((imageHeight img'')-1)]]

         extractCR :: (PixelYCbCr8) -> RedValue
         extractCR (PixelYCbCr8 _ _ cr) =
           pixel8ToWord8 cr

         filePath :: FilePath
         filePath = "src/Data/IMG_2457.JPG"

         redLaserLine :: TargetValue
         redLaserLine = 160



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
pixelIndicesOfPixelValuesLTE :: Word8 -> [Word8] -> [Int]
pixelIndicesOfPixelValuesLTE thresholdValue rawData  = ( L.findIndices) (<=thresholdValue) rawData

indicesOfPixelValuesGTE :: Word8 -> [Word8] -> [Int]
indicesOfPixelValuesGTE thresholdValue rawData  = ( L.findIndices) (>=thresholdValue) rawData



pixel8ToWord8 :: Pixel8 -> Word8
pixel8ToWord8 pixel = pixel


averageIndexValueOf :: [Int] -> Double
averageIndexValueOf list =
  (fromIntegral $ L.sum list)  / (fromIntegral $ length list)
  
type RedValue = Word8
type AvgIndexPosition = Double
type RowIndex = Int
type TargetValue = Word8
