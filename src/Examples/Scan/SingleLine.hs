module Examples.Scan.SingleLine() where
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.Split as LS
import TriCad.MathPolar(
  Slope(..),
  Radius(..),
  flatXSlope,
  flatYSlope,
  )
import CornerPoints.Points(Point(..))
import TriCad.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
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
import TriCad.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns, createVerticalCubes,
                           SingleDegreeRadii(..), MultiDegreeRadii(..))

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


{-
Create an stl file from raw data using ParseAtto.
Note that I can't use json yet, as it has not been changed for the new Scan datatype changes.

For some reason the shape is not write in netfabb.
-}
parseRawDataToScanWriteStlFileWithParseAtto =   do
   contents <- B.readFile "src/Data/scanRawDataWitDegrees.raww"
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




--this is set up to have rows reduced by 10
writeStlFileFromScan scan = 
  let origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      heightPerPixel = 10
      leftFaces = createLeftFacesMultiColumns origin (tail $ degrees scan) flatXSlope flatYSlope [0,heightPerPixel..]
      rightFaces = createRightFaces origin (head $ degrees scan) flatXSlope flatYSlope [0,heightPerPixel..]
      triangles =  concat
        [ [FacesFrontTop | x <- [1..4]],
          [FaceFront | x <- [5..224]],
          [FacesBottomFront | x <- [1,2,3,4]]
        ]
        +++^
        createVerticalCubes rightFaces leftFaces
      stlFile = newStlShape "backscratcher" triangles
  in
      writeStlToFile stlFile
      --putStrLn "done"
  
      
     


