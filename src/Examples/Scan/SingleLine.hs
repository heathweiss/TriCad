module Examples.Scan.SingleLine() where
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.Split as LS
import CornerPoints.Create(Slope(..), flatXSlope, flatYSlope)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (++>), (+++), (++++), (++:), Faces(..))
import Stl.StlCornerPoints((+++^))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Scan.Transform(pixelIndicesOfPixelValuesLTE, pixelIndicesAverageToRadius, reduceRows, reduceScanRows, multiDegreePixelValuesToMultiDegreeRadii)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as B
import qualified  Data.ByteString.Char8 as BC (pack)
import qualified  Data.ByteString.Internal as BI (unpackBytes)
import GHC.Word (Word8)
import Data.Aeson
import Scan.Json()
import qualified Scan.Parse as PA  ( MultiDegreePixelValues(..), parseCSVPixelValues)
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns, createHorizontallyAlignedCubes)
import CornerPoints.Radius(SingleDegreeRadii(..), MultiDegreeRadii(..))
import Scan.Transform (reduceScanRows)
--create a [Word8] for: Right(B.pack $ strToWord8s)
--which gets a bytstring of word8
strToWord8s :: String -> [Word8]
strToWord8s = BI.unpackBytes . BC.pack

--------------------------------------------- raw data with degree data attached -----------------------------
   
      
parseRawDataToScanAndSaveToJson  = do
   contents <- B.readFile "src/Data/scanRawDataWitDegrees.raww"
   let multiDegreePixelValues = PA.parseCSVPixelValues contents
       multiDegreeRadii = multiDegreePixelValuesToMultiDegreeRadii "myScan" (pixelIndicesAverageToRadius . pixelIndicesOfPixelValuesLTE 75) multiDegreePixelValues  >>= reduceScanRows 10
   case (multiDegreeRadii) of
    Left msg -> putStrLn msg
    Right (MultiDegreeRadii name degrees_) -> BL.writeFile "src/Data/scanFullData.json" $ encode $ MultiDegreeRadii name degrees_

   putStrLn "done" 

canItParseOnly = do
 contents <- B.readFile "src/Data/scanRawDataWithDegrees.raww"
 let multiDegreePixelValues = PA.parseCSVPixelValues contents
 print $ show  multiDegreePixelValues
{-
Create an stl file from raw data using ParseAtto.
Note that I can't use json yet, as it has not been changed for the new Scan datatype changes.

For some reason the shape is not write in netfabb.
-}
parseRawDataToScanWriteStlFileWithParseAtto =   do
   contents <- B.readFile "src/Data/scanRawDataWithDegrees.raww"
   let multiDegreePixelValues = PA.parseCSVPixelValues contents
       multiDegreeRadii = multiDegreePixelValuesToMultiDegreeRadii "myScan" (pixelIndicesAverageToRadius . pixelIndicesOfPixelValuesLTE 75) multiDegreePixelValues  >>= reduceScanRows 10
   case (multiDegreeRadii) of
    Left msg -> putStrLn msg
    Right (MultiDegreeRadii name degrees_) -> writeStlFileFromScan $ MultiDegreeRadii name degrees_
    
   putStrLn "done"


{-
Use some canned data with ParseAtto to create an stl file.
-} 
parseCannedRawDataToScanWriteStlFileWithParseAtto = do  
   let multiDegreePixelValues = (Right(B.pack $strToWord8s "0 1 2 3;1 2 3;1 2 3$90 1 2 3;1 2 3;1 2 3$180 1 2 3;1 2 3;1 2 3$270 1 2 3;1 2 3;1 2 3$360 1 2 3;1 2 3;1 2 3")  >>=  PA.parseCSVPixelValues)
       multiDegreeRadii = multiDegreePixelValuesToMultiDegreeRadii "myScan" (pixelIndicesAverageToRadius . pixelIndicesOfPixelValuesLTE 2) multiDegreePixelValues  >>= reduceScanRows 1

   case (multiDegreeRadii) of
    Left msg -> putStrLn msg
    Right (MultiDegreeRadii name degrees_) -> writeStlFileFromScan $ MultiDegreeRadii name degrees_

   putStrLn "done"
  

readTrianglesFromJsonFileAndWriteToStlFile = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  case (decode contents) of 
      Just (MultiDegreeRadii name degrees) -> writeStlFileFromScan (MultiDegreeRadii name degrees)
      Nothing                  -> putStrLn "Nothing"

{-
reduceScanRows :: Int -> MultiDegreeRadii -> Either String MultiDegreeRadii
-}
readMultiDegreeRadiiFromJsonReduceAndWriteBackToJson = do 
  contents <- BL.readFile "src/Data/scanFullData.json"
  case (decode contents) of 
      Just (MultiDegreeRadii name degrees) ->
        let reduced = (Right (MultiDegreeRadii name degrees) ) >>= reduceScanRows 20
        in  case reduced of
              Left err -> putStrLn err
              Right (MultiDegreeRadii name degrees) -> BL.writeFile "src/Data/scanFullData.json" $ encode $ MultiDegreeRadii name degrees
                                                      
      Nothing                  -> putStrLn "json file not read"
{-
parseRawDataToScanAndSaveToJson  = do
   contents <- B.readFile "src/Data/scanRawDataWitDegrees.raww"
   let multiDegreePixelValues = PA.parseCSVPixelValues contents
       multiDegreeRadii = multiDegreePixelValuesToMultiDegreeRadii "myScan" (pixelIndicesAverageToRadius . pixelIndicesOfPixelValuesLTE 75) multiDegreePixelValues  >>= reduceScanRows 10
   case (multiDegreeRadii) of
    Left msg -> putStrLn msg
    Right (MultiDegreeRadii name degrees_) -> BL.writeFile "src/Data/scanFullData.json" $ encode $ MultiDegreeRadii name degrees_

   putStrLn "done"

======================================================
readMultiDegreeRadiiFromJsonReduceAndWriteBackToJson = do 
  contents <- BL.readFile "src/Data/scanFullData.json"
  case (decode contents) of 
      Just (MultiDegreeRadii name degrees) ->
        writeStlFileFromScanMonadic $ (Right (MultiDegreeRadii name degrees) ) >>= reduceScanRows 1200 
                                                      
      Nothing                  -> putStrLn "json file not read"
=====================================================
readMultiDegreeRadiiFromJsonReduceAndWriteBackToJson = do 
  contents <- BL.readFile "src/Data/scanFullData.json"
  case (decode contents) of 
      Just (MultiDegreeRadii name degrees) ->
        writeStlFileFromScanMonadic $ reduceScanRows 120 (MultiDegreeRadii name degrees)
                                                      
      Nothing                  -> putStrLn "json file not read"
=================================================
compiles
readMultiDegreeRadiiFromJsonReduceAndWriteBackToJson = do 
  contents <- BL.readFile "src/Data/scanFullData.json"
  case (decode contents) of 
      Just (MultiDegreeRadii name degrees) ->
             let temp = reduceScanRows 120 (MultiDegreeRadii name degrees)
             in 
                 case temp of
                   Right (MultiDegreeRadii name degrees) ->   writeStlFileFromScan (MultiDegreeRadii name degrees)
                   -- reduceScanRows 120 (MultiDegreeRadii name degrees) >>= writeStlFileFromScan
                                                              
                   Left err -> putStrLn err
      Nothing  -> putStrLn "json file not read"
================================================================
readMultiDegreeRadiiFromJsonReduceAndWriteBackToJson = do 
  contents <- BL.readFile "src/Data/scanFullData.json"
  case (decode contents) of 
      Just (MultiDegreeRadii name degrees) ->
        case (MultiDegreeRadii name degrees) of
          Left err -> putStrLn "MultiDegreeRadii: " ++ err
          Right (MultiDegreeRadii name degrees) ->  writeStlFileFromScan $  reduceScanRows 120 (MultiDegreeRadii name degrees)
      Nothing                  -> putStrLn "json file not read"

=================================
readMultiDegreeRadiiFromJsonReduceAndWriteBackToJson = do 
  contents <- BL.readFile "src/Data/scanFullData.json"
  case (decode contents) of 
      Just (MultiDegreeRadii name degrees) ->
        case (MultiDegreeRadii name degrees) of
          Left err -> putStrLn "MultiDegreeRadii: " ++ err
          Right (MultiDegreeRadii name degrees) ->  writeStlFileFromScan $  reduceScanRows 120 (MultiDegreeRadii name degrees)
      Nothing                  -> putStrLn "json file not read"
-}

--this is set up to have rows reduced by 10
--This is a support function, not to be called directly.
--Used by parseCannedRawDataToScanWriteStlFileWithParseAtto
writeStlFileFromScan :: MultiDegreeRadii -> IO ()
writeStlFileFromScan scan = 
  let origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      heightPerPixel = 10
      leftFaces = createLeftFacesMultiColumns origin (tail $ degrees scan) flatXSlope flatYSlope [0,heightPerPixel..]
      rightFaces = createRightFaces origin (head $ degrees scan) flatXSlope flatYSlope [0,heightPerPixel..]
      frontTriangles = [FaceFront | x <- [1..]]
      triangles =
       zipWith  (+++^)
        ([FacesFrontTop | x <- [1..]] :   [frontTriangles | x <- [5..65]] ++:  [FacesBottomFront | x <- [66..]] 

        ) 
        --  +++^
        (createHorizontallyAlignedCubes rightFaces leftFaces)
      stlFile = newStlShape "backscratcher" $ concat triangles
  in
      writeStlToFile stlFile
      --print origin




writeStlFileFromScanMonadic :: (Either String MultiDegreeRadii) -> IO ()
writeStlFileFromScanMonadic scan =
  let origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      heightPerPixel = 10
      scan' =
        case scan of
                Right (MultiDegreeRadii name' degrees') -> (MultiDegreeRadii name' degrees')
      leftFaces = createLeftFacesMultiColumns origin (tail $ degrees scan') flatXSlope flatYSlope [0,heightPerPixel..]
      rightFaces = createRightFaces origin (head $ degrees scan') flatXSlope flatYSlope [0,heightPerPixel..]
      frontTriangles = [FaceFront | x <- [1..]]
      triangles =
       zipWith  (+++^)
        ([FacesFrontTop | x <- [1..]] :   [frontTriangles | x <- [5..65]] ++:  [FacesBottomFront | x <- [66..]] 
      
        ) 
        --  +++^
        (createHorizontallyAlignedCubes rightFaces leftFaces)
      stlFile = newStlShape "backscratcher" $ concat triangles
  in
      case scan of
        Right scan'' -> writeStlToFile stlFile
                        
        Left err -> putStrLn err
{-


-}     


