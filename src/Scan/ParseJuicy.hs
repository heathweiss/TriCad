{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ParallelListComp #-}
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
         


showAverageLTE100RedPixelValuesFor1Row = do
      jpegImage <-   readImage "src/Data/IMG_2457.JPG"
      case  jpegImage of
        Left err -> putStrLn err
        Right (ImageYCbCr8 jpegImage') ->
          putStrLn $ show $ averageValueOf $  indicesOfThePixelValuesGTE  redLaserLine $ readAllcolumns jpegImage'
          --putStrLn $ show $ readAllcolumns img
        otherwise -> putStrLn "another format"
      where
         readAllcolumns img' =
           [extractCR $ pixelAt img' x 10 |  x <- [0..(imageWidth img')]]

         
         extractCR (PixelYCbCr8 _ _ cr) =
           pixel8ToWord8 cr
           
         redLaserLine :: TargetValue
         redLaserLine = 160


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
             Right (ImageYCbCr8 jpegImage) -> (SingleDegreeRadii currDegree (map (Radius) (getRedLaserLineSingleImage jpegImage)))
        
        readImage' fileName = readImage $ filePathBuilder fileName


-- see http://learnyouahaskell.com/input-and-output to get rid of big list of jpegImage0..
--by using Control.Monad
process10DegreeImagesToRedLaserLine  = do
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

{-
BL.writeFile "src/Data/scanFullData.json" $ encode $ multiDegreeRadii
[jpegImage0, jpegImage10, jpegImage20, jpegImage30, jpegImage40, jpegImage50, jpegImage60, jpegImage70, jpegImage80, jpegImage90, jpegImage100, jpegImage110,
                          jpegImage120, jpegImage130, jpegImage140, jpegImage150, jpegImage160, jpegImage170, jpegImage180, jpegImage190, jpegImage200, jpegImage210, jpegImage220, jpegImage230,
                          jpegImage240, jpegImage250, jpegImage260, jpegImage270, jpegImage280, jpegImage290, jpegImage300, jpegImage310, jpegImage320, jpegImage340,
                          jpegImage340, jpegImage350, jpegImage10]
=======================================================================
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

==============================================================================================
processMultiDegreeImagesToRedLaserLine degreeList  = do
  --degree list must end with the last degree read, prior to the 360 degree. Must start with 0
  singleDegreeRadiis  <- forM degreeList (\currDegree -> do
    jpegImage <-  readImage' $ show currDegree
    return $ setSingleDegreeRadii jpegImage currDegree)

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
==================================================================
processMultiDegreeImagesToRedLaserLine degreeList  = do
  --degree list must end with the last degree read, prior to the 360 degree. Must start with 0
  singleDegreeRadiis  <- forM degreeList (\currDegree -> do
    jpegImage <-  readImage' $ show currDegree
    return $ setSingleDegreeRadii jpegImage currDegree)

  jpegImage360 <- readImage' "0"
  let
    lastSingleDegreeRadii =
      case jpegImage360 of
            Left err -> [(SingleDegreeRadii 360 [Radius 0])] 
            Right (ImageYCbCr8 jpegImage360') -> [(SingleDegreeRadii 360 (map (Radius) (getRedLaserLineSingleImage jpegImage360')))]
     
    multiDegreeRadii = MultiDegreeRadii "theName" (singleDegreeRadiis ++ lastSingleDegreeRadii)
    
  print $ show $ multiDegreeRadii
  
  where setSingleDegreeRadii imageAsRead currDegree =
           case imageAsRead of
             Left err -> (SingleDegreeRadii currDegree [Radius 0]) 
             Right (ImageYCbCr8 jpegImage) -> (SingleDegreeRadii currDegree (map (Radius) (getRedLaserLineSingleImage jpegImage)))

        readImage' fileName = readImage $ filePathBuilder fileName
======================================================================
process10DegreeImagesToRedLaserLine  = do
  
  singleDegreeRadiis  <- forM [0,10..350] (\currDegree -> do
    jpegImage <-  readImage' $ show currDegree
    return $ setSingleDegreeRadii jpegImage currDegree)

  jpegImage360 <- readImage' "0"
  let
    lastSingleDegreeRadii =
      case jpegImage360 of
            Left err -> [(SingleDegreeRadii 360 [Radius 0])] 
            Right (ImageYCbCr8 jpegImage360') -> [(SingleDegreeRadii 360 (map (Radius) (getRedLaserLineSingleImage jpegImage360')))]
     
    multiDegreeRadii = MultiDegreeRadii "theName" (singleDegreeRadiis ++ lastSingleDegreeRadii)
    
  print $ show $ multiDegreeRadii
  
  where setSingleDegreeRadii imageAsRead currDegree =
           case imageAsRead of
             Left err -> (SingleDegreeRadii currDegree [Radius 0]) 
             Right (ImageYCbCr8 jpegImage) -> (SingleDegreeRadii currDegree (map (Radius) (getRedLaserLineSingleImage jpegImage)))

        readImage' fileName = readImage $ filePathBuilder fileName
=================================================================================
process10DegreeImagesToRedLaserLine  = do
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
          | currDegree <- [350,340..0]
          | currImage <- [jpegImage350, jpegImage340, jpegImage330, jpegImage320, jpegImage310, jpegImage300, jpegImage290, jpegImage280, jpegImage270, jpegImage260, jpegImage250,
                         jpegImage240, jpegImage230, jpegImage220, jpegImage210, jpegImage200, jpegImage190, jpegImage180, jpegImage170, jpegImage160, jpegImage150, jpegImage140, jpegImage130,
                         jpegImage120, jpegImage110, jpegImage100, jpegImage90, jpegImage80, jpegImage70, jpegImage60, jpegImage50, jpegImage40, jpegImage30, jpegImage20, jpegImage10,
                         jpegImage0]
       ]
       ++
       --read in the 0 degree image again as the 360 degree image, to ensure they are exactly the same.
       case jpegImage0 of
            Left err -> [(SingleDegreeRadii 360 [Radius 0])] 
            Right (ImageYCbCr8 jpegImage0') -> [(SingleDegreeRadii 360 (map (Radius) (getRedLaserLineSingleImage jpegImage0')))]
        
      multiDegreeRadii = MultiDegreeRadii "theName" singleDegreeRadiiList
  print $ show multiDegreeRadii 
  
  where setSingleDegreeRadii imageAsRead currDegree =
           case imageAsRead of
             Left err -> (SingleDegreeRadii currDegree [Radius 0]) 
             Right (ImageYCbCr8 jpegImage) -> (SingleDegreeRadii currDegree (map (Radius) (getRedLaserLineSingleImage jpegImage)))

        readImage' fileName = readImage $ filePathBuilder fileName

============================================
process10DegreeImagesToRedLaserLine  = do
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
        setSingleDegreeRadii jpegImage0 0
        : setSingleDegreeRadii jpegImage10 10
        : setSingleDegreeRadii jpegImage20 20
        : setSingleDegreeRadii jpegImage30 30
        : setSingleDegreeRadii jpegImage40 40
        : setSingleDegreeRadii jpegImage50 50
        : setSingleDegreeRadii jpegImage60 60
        : setSingleDegreeRadii jpegImage70 70
        : setSingleDegreeRadii jpegImage80 80
        : setSingleDegreeRadii jpegImage90 90
        : setSingleDegreeRadii jpegImage100 100
        : setSingleDegreeRadii jpegImage110 110
        : setSingleDegreeRadii jpegImage120 120
        : setSingleDegreeRadii jpegImage130 130
        : setSingleDegreeRadii jpegImage140 140
        : setSingleDegreeRadii jpegImage150 150
        : setSingleDegreeRadii jpegImage160 160
        : setSingleDegreeRadii jpegImage170 170
        : setSingleDegreeRadii jpegImage180 180
        : setSingleDegreeRadii jpegImage190 190
        : setSingleDegreeRadii jpegImage200 200
        : setSingleDegreeRadii jpegImage210 210
        : setSingleDegreeRadii jpegImage220 220
        : setSingleDegreeRadii jpegImage230 230
        : setSingleDegreeRadii jpegImage240 240
        : setSingleDegreeRadii jpegImage250 250
        : setSingleDegreeRadii jpegImage260 260
        : setSingleDegreeRadii jpegImage270 270
        : setSingleDegreeRadii jpegImage280 280
        : setSingleDegreeRadii jpegImage290 290
        : setSingleDegreeRadii jpegImage300 300
        : setSingleDegreeRadii jpegImage310 310
        : setSingleDegreeRadii jpegImage320 320
        : setSingleDegreeRadii jpegImage330 340
        : setSingleDegreeRadii jpegImage340 340
        : setSingleDegreeRadii jpegImage350 350
        --read in 0 degree image again as 360 degree image, 
        : case jpegImage0 of
            Left err -> (SingleDegreeRadii 360 [Radius 0]) 
            Right (ImageYCbCr8 jpegImage0') -> (SingleDegreeRadii 360 (map (Radius) (getRedLaserLineSingleImage jpegImage0')))
        :  []

  print $ show singleDegreeRadiiList 
  
  where setSingleDegreeRadii imageAsRead currDegree =
           case imageAsRead of
             Left err -> (SingleDegreeRadii currDegree [Radius 0]) 
             Right (ImageYCbCr8 jpegImage) -> (SingleDegreeRadii currDegree (map (Radius) (getRedLaserLineSingleImage jpegImage)))

        readImage' fileName = readImage $ filePathBuilder fileName

=====================================================================
compiles
process10DegreeImagesToRedLaserLine  = do
  jpegImage0 <-   readImage $ filePathBuilder "0"
  jpegImage10 <-  readImage $ filePathBuilder "10"
  jpegImage20 <-  readImage $ filePathBuilder "20"
  jpegImage30 <-  readImage $ filePathBuilder "30"
  jpegImage40 <-  readImage $ filePathBuilder "40"
  jpegImage50 <-  readImage $ filePathBuilder "50"
  jpegImage60 <-  readImage $ filePathBuilder "60"
  jpegImage70 <-  readImage $ filePathBuilder "70"
  jpegImage80 <-  readImage $ filePathBuilder "80"
  jpegImage90 <-  readImage $ filePathBuilder "90"
  jpegImage100 <-  readImage $ filePathBuilder "100"
  jpegImage110 <-  readImage $ filePathBuilder "110"
  jpegImage120 <-  readImage $ filePathBuilder "120"
  jpegImage130 <-  readImage $ filePathBuilder "130"
  jpegImage140 <-  readImage $ filePathBuilder "140"
  jpegImage150 <-  readImage $ filePathBuilder "150"
  jpegImage160 <-  readImage $ filePathBuilder "160"
  jpegImage170 <-  readImage $ filePathBuilder "170"
  jpegImage180 <-  readImage $ filePathBuilder "180"
  jpegImage190 <-  readImage $ filePathBuilder "190"
  jpegImage200 <-  readImage $ filePathBuilder "200"
  jpegImage210 <-  readImage $ filePathBuilder "210"
  jpegImage220 <-  readImage $ filePathBuilder "220"
  jpegImage230 <-  readImage $ filePathBuilder "230"
  jpegImage240 <-  readImage $ filePathBuilder "240"
  jpegImage250 <-  readImage $ filePathBuilder "250"
  jpegImage260 <-  readImage $ filePathBuilder "260"
  jpegImage270 <-  readImage $ filePathBuilder "270"
  jpegImage280 <-  readImage $ filePathBuilder "280"
  jpegImage290 <-  readImage $ filePathBuilder "290"
  jpegImage300 <-  readImage $ filePathBuilder "300"
  jpegImage310 <-  readImage $ filePathBuilder "310"
  jpegImage320 <-  readImage $ filePathBuilder "320"
  jpegImage330 <-  readImage $ filePathBuilder "330"
  jpegImage340 <-  readImage $ filePathBuilder "340"
  jpegImage350 <-  readImage $ filePathBuilder "350"

  let
      singleDegreeRadiiList :: [SingleDegreeRadii]
      singleDegreeRadiiList = singleDegreeRadii0 : singleDegreeRadii10 : singleDegreeRadii10 :  []
      singleDegreeRadii0 = 
       case jpegImage0 of
        Left err -> (SingleDegreeRadii 0 [Radius 0]) 
        Right (ImageYCbCr8 jpegImage0') -> (SingleDegreeRadii 0 (map (Radius) (getRedLaserLineSingleImage jpegImage0')))

      singleDegreeRadii10 = 
       case jpegImage10 of
        Left err -> (SingleDegreeRadii 10 [Radius 0]) 
        Right (ImageYCbCr8 jpegImage10') -> (SingleDegreeRadii 0 (map (Radius) (getRedLaserLineSingleImage jpegImage10'))) 
  
      singleDegreeRadii20 = 
       case jpegImage20 of
        Left err -> (SingleDegreeRadii 20 [Radius 0]) 
        Right (ImageYCbCr8 jpegImage20') -> (SingleDegreeRadii 0 (map (Radius) (getRedLaserLineSingleImage jpegImage20')))

      singleDegreeRadii30 = 
       case jpegImage30 of
        Left err -> (SingleDegreeRadii 30  [Radius 0]) 
        Right (ImageYCbCr8 jpegImage30') -> (SingleDegreeRadii 40 (map (Radius) (getRedLaserLineSingleImage jpegImage30')))

      singleDegreeRadii40 = 
       case jpegImage40 of
        Left err -> (SingleDegreeRadii 40 [Radius 0]) 
        Right (ImageYCbCr8 jpegImage40') -> (SingleDegreeRadii 40 (map (Radius) (getRedLaserLineSingleImage jpegImage40'))) 

  

  print $ show singleDegreeRadiiList 
  
  where tmp = 1

===========================================================================================
compiles good
process10DegreeImagesToRedLaserLine  = do
  jpegImage0 <-   readImage $ filePathBuilder "0"
  jpegImage10 <-  readImage $ filePathBuilder "0"
  let
      singleDegreeRadiiList :: [SingleDegreeRadii]
      singleDegreeRadiiList = singleDegreeRadii0 : singleDegreeRadii10 :  []
      singleDegreeRadii0 = 
       case jpegImage0 of
        Left err -> (SingleDegreeRadii 0 [Radius 0]) 
        Right (ImageYCbCr8 jpegImage0') -> (SingleDegreeRadii 0 (map (Radius) (getRedLaserLineSingleImage jpegImage0')))

      singleDegreeRadii10 = 
       case jpegImage10 of
        Left err -> (SingleDegreeRadii 0 [Radius 0]) 
        Right (ImageYCbCr8 jpegImage10') -> (SingleDegreeRadii 0 (map (Radius) (getRedLaserLineSingleImage jpegImage10'))) 
  
      
  print $ show singleDegreeRadiiList 
=============================================================================================
compiles good
process10DegreeImagesToRedLaserLine  = do
  jpegImage0 <-   readImage $ filePathBuilder "0"
  let singleDegreeRadiiList = 
       case jpegImage0 of
        Left err -> (SingleDegreeRadii 0 [Radius 0]) 
        Right (ImageYCbCr8 jpegImage0') -> (SingleDegreeRadii 0 (map (Radius) (getRedLaserLineSingleImage jpegImage0'))) 
  print $ show singleDegreeRadiiList
=========================================
process10DegreeImagesToRedLaserLine  = 
  [getRedLaserLineSingleImageMaybe (filePathBuilder $ show currDegree) currDegree  | currDegree <- [0,10..350]]
  where
    getRedLaserLineSingleImageMaybe filePath degree'' = do
      jpegImage <-   readImage filePath
      case jpegImage of
        Left err -> print $ show err
        Right (ImageYCbCr8 jpegImage') -> print $ show $ SingleDegreeRadii degree''  (map (Radius) $ getRedLaserLineSingleImage jpegImage')
=================================================================================
process10DegreeImagesToRedLaserLine = 
  [getRedLaserLineSingleImageMaybe $ filePathBuilder $ show currDegree  | currDegree <- [0,10..350]]
  where
    getRedLaserLineSingleImageMaybe filePath = do
      jpegImage <-   readImage filePath
      case jpegImage of
        Left err -> print $ show err
        Right (ImageYCbCr8 jpegImage') -> print $ show $ getRedLaserLineSingleImage jpegImage'
========================================================================
process10DegreeImagesToRedLaserLine :: IO ( [Either String [TargetValueIndex]])
process10DegreeImagesToRedLaserLine = do
  [getRedLaserLineSingleImageMaybe $ filePathBuilder $ show currDegree  | currDegree <- [0,10..350]]
  where
    getRedLaserLineSingleImageMaybe filePath = do
      jpegImage <-   readImage filePath
      case jpegImage of
        Left err -> Left err
        Right (ImageYCbCr8 jpegImage') -> Right $ getRedLaserLineSingleImage jpegImage
=================================================================
process10DegreeImagesToRedLaserLine :: IO ( [Either String [TargetValueIndex]] )
process10DegreeImagesToRedLaserLine = do
  [getRedLaserLineSingleImageMaybe $ filePathBuilder $ show currDegree  | currDegree <- [0,10..350]]
  where
    getRedLaserLineSingleImageMaybe filePath = do
      jpegImage <-   readImage filePath
      case jpegImage of
        Left err -> Left err
        Right (ImageYCbCr8 jpegImage') -> Right $ getRedLaserLineSingleImage jpegImage'
-}

getRedLaserLineSingleImage :: (Image  PixelYCbCr8) -> [TargetValueIndex]
getRedLaserLineSingleImage jpegImage = 
  processSingleImageToReducedEachRowToTargetValueIndex jpegImage (extractCR) (averageValueOf) (indicesOfThePixelValuesGTE) redLaserLine
    

{-
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


{-
processSingleImageToReducedEachRowToTargetValueIndex:: FilePath -> ((PixelYCbCr8) -> Word8) ->  ([Int] -> Double)   ->         ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  IO ()
processSingleImageToReducedEachRowToTargetValueIndex   filePath    extractColor                 reduceIndexesOfTargetPixels     pixelsThatQualifyAsA           targetValue  = do
      jpegImage <-   readImage filePath
      case  jpegImage of
        Left err -> putStrLn err
        Right (ImageYCbCr8 jpegImage') ->
          putStrLn $ show $ reduceEachRowOfThe jpegImage'  (toASingleIndexValueFrom (pixelsThatQualifyAsA) targetValue)  
        otherwise -> putStrLn "another format"
      where
         toASingleIndexValueFrom :: ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  RowIndex -> (Image  PixelYCbCr8) -> TargetValueIndex
         toASingleIndexValueFrom    pixelsThatQualifyAsA'                  targetValue     row         img'  =
           reduceIndexesOfTargetPixels $  pixelsThatQualifyAsA'  targetValue  [ extractColor $ pixelAt img' x row |  x <- [0..((imageWidth img')-1)]]

         reduceEachRowOfThe :: (Image  PixelYCbCr8) ->  ( RowIndex -> (Image  PixelYCbCr8) ->  TargetValueIndex) ->  [TargetValueIndex]
         reduceEachRowOfThe    img''                    reducer                                                       =
           [reducer  y img'' | y <-  [0..((imageHeight img'')-1)]]

=============================================================================================================
processSingleImageToReducedRowValuesBase:: FilePath -> ((PixelYCbCr8) -> Word8) ->  ([Int] -> Double)   ->         ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  IO ()
processSingleImageToReducedRowValuesBase   filePath    extractColor                 reduceIndexesOfTargetPixels     pixelsThatQualifyAsA           targetValue  = do
      jpegImage <-   readImage filePath
      case  jpegImage of
        Left err -> putStrLn err
        Right (ImageYCbCr8 jpegImage') ->
          putStrLn $ show $ reduceEachRowOfThe jpegImage'  (toASingleIndexValueFrom (pixelsThatQualifyAsA) targetValue)  
        otherwise -> putStrLn "another format"
      where
         toASingleIndexValueFrom :: ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  RowIndex -> (Image  PixelYCbCr8) -> TargetValueIndex
         toASingleIndexValueFrom    pixelsThatQualifyAsA'                  targetValue     row         img'  =
           reduceIndexesOfTargetPixels $  pixelsThatQualifyAsA'  targetValue  [ extractColor $ pixelAt img' x row |  x <- [0..((imageWidth img')-1)]]

         reduceEachRowOfThe :: (Image  PixelYCbCr8) ->  ( RowIndex -> (Image  PixelYCbCr8) ->  TargetValueIndex) ->  [TargetValueIndex]
         reduceEachRowOfThe    img''                    reducer                                                       =
           [reducer  y img'' | y <-  [0..((imageHeight img'')-1)]]
===============================================================================
processSingleImageToReducedRowValuesBase:: FilePath -> ((PixelYCbCr8) -> Word8) ->  ([Int] -> Double)   ->         ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  IO ()
processSingleImageToReducedRowValuesBase   filePath    extractColor                 reduceIndexesOfTargetPixels     pixelsThatQualifyAsA           targetValue  = do
      jpegImage <-   readImage filePath
      case  jpegImage of
        Left err -> putStrLn err
        Right (ImageYCbCr8 jpegImage') ->
          putStrLn $ show $ reduceEachRowOfThe jpegImage'  (toASingleIndexValueFrom (pixelsThatQualifyAsA) targetValue)  
        otherwise -> putStrLn "another format"
      where
         toASingleIndexValueFrom :: ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  RowIndex -> (Image  PixelYCbCr8) -> AvgIndexPosition
         toASingleIndexValueFrom    pixelsThatQualifyAsA'                  targetValue     row         img'  =
           reduceIndexesOfTargetPixels $  pixelsThatQualifyAsA'  targetValue  [ extractColor $ pixelAt img' x row |  x <- [0..((imageWidth img')-1)]]

         reduceEachRowOfThe :: (Image  PixelYCbCr8) ->  ( RowIndex -> (Image  PixelYCbCr8) ->  AvgIndexPosition) ->  [AvgIndexPosition]
         reduceEachRowOfThe    img''                    reducer                                                       =
           [reducer  y img'' | y <-  [0..((imageHeight img'')-1)]]

=================================================================================
processSingleImageToReducedRowValuesBase:: FilePath -> ((PixelYCbCr8) -> Word8) ->  ([Int] -> Double)   ->         ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  IO ()
processSingleImageToReducedRowValuesBase   filePath    extractColor                 reduceIndexesOfTargetPixels     pixelsThatQualifyAsA              targetValue  = do
      jpegImage <-   readImage filePath
      case  jpegImage of
        Left err -> putStrLn err
        Right (ImageYCbCr8 jpegImage') ->
          putStrLn $ show $ reduceEachRowOfThe jpegImage'  (toASingleIndexValueFrom (pixelsThatQualifyAsA) targetValue)  
        otherwise -> putStrLn "another format"
      where
         toASingleIndexValueFrom :: ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  RowIndex -> (Image  PixelYCbCr8) -> AvgIndexPosition
         toASingleIndexValueFrom    pixelValuesGTE                  targetValue     row         img'  =
           reduceIndexesOfTargetPixels $  pixelValuesGTE  targetValue  [ extractColor $ pixelAt img' x row |  x <- [0..((imageWidth img')-1)]]

         reduceEachRowOfThe :: (Image  PixelYCbCr8) ->  ( RowIndex -> (Image  PixelYCbCr8) ->  AvgIndexPosition) ->  [AvgIndexPosition]
         reduceEachRowOfThe    img''                    reducer                                                       =
           [reducer  y img'' | y <-  [0..((imageHeight img'')-1)]] 
===============================================================================================================
processSingleImageToReducedRowValuesBase:: FilePath -> ((PixelYCbCr8) -> Word8) ->  ([Int] -> Int)   ->  ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  IO ()
processSingleImageToReducedRowValuesBase   filePath    extractColor                reduceTargetPixels     pixelsThatQualifyAs              targetValue  = do
      jpegImage <-   readImage filePath
      case  jpegImage of
        Left err -> putStrLn err
        Right (ImageYCbCr8 jpegImage') ->
          putStrLn $ show $ reduceEachRowOfThe jpegImage'  (toASingleValueFrom (pixelsThatQualifyAs) targetValue)  
        otherwise -> putStrLn "another format"
      where
         toASingleValueFrom :: ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  RowIndex -> (Image  PixelYCbCr8) -> AvgIndexPosition
         toASingleValueFrom    pixelValuesGTE                  targetValue     row         img'  =
           averageValueOf $  pixelValuesGTE  targetValue  [ extractColor $ pixelAt img' x row |  x <- [0..((imageWidth img')-1)]]

         reduceEachRowOfThe :: (Image  PixelYCbCr8) ->  ( RowIndex -> (Image  PixelYCbCr8) ->  AvgIndexPosition) ->  [AvgIndexPosition]
         reduceEachRowOfThe    img''                    reducer                                                       =
           [reducer  y img'' | y <-  [0..((imageHeight img'')-1)]]
=========================================================================================================
processSingleImageToReducedRowValuesBase:: FilePath -> ((PixelYCbCr8) -> Word8) -> ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  IO ()
processSingleImageToReducedRowValuesBase   filePath    extractColor                pixelsThatQualifyAs              targetValue  = do
      jpegImage <-   readImage filePath
      case  jpegImage of
        Left err -> putStrLn err
        Right (ImageYCbCr8 jpegImage') ->
          putStrLn $ show $ reduceEachRowOfThe jpegImage'  (toTheAvgOfThe (pixelsThatQualifyAs) targetValue)  
        otherwise -> putStrLn "another format"
      where
         toTheAvgOfThe :: ( Word8 -> [Word8] -> [Int]) -> TargetValue ->  RowIndex -> (Image  PixelYCbCr8) -> AvgIndexPosition
         toTheAvgOfThe    pixelValuesGTE                  targetValue     row         img'  =
           averageValueOf $  pixelValuesGTE  targetValue  [ extractColor $ pixelAt img' x row |  x <- [0..((imageWidth img')-1)]]

         reduceEachRowOfThe :: (Image  PixelYCbCr8) ->  ( RowIndex -> (Image  PixelYCbCr8) ->  AvgIndexPosition) ->  [AvgIndexPosition]
         reduceEachRowOfThe    img''                    reducer                                                       =
           [reducer  y img'' | y <-  [0..((imageHeight img'')-1)]]
-}         

         

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

redLaserLine :: TargetValue
redLaserLine = 160

extractCR :: (PixelYCbCr8) -> Word8
extractCR (PixelYCbCr8 _ _ cr) =  pixel8ToWord8 cr
