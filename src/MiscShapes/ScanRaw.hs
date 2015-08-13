module MiscShapes.ScanRaw() where
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.Split as LS
import TriCad.MathPolar(
  createRightFaces,
  createLeftFaces,
  createRightFacesFromScan,
  createLeftFacesFromScan ,
  Slope(..),
  Radius(..),
  Scan(..),
  Degree(..),
  flatXSlope,
  flatYSlope,
  createLeftFacesMultiColumns,
  createLeftFacesMultiColumnsFromScan,
  createVerticalCubes,
  createVerticalCubesFromScan,
  )
import TriCad.Points(Point(..))
import TriCad.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import TriCad.StlCornerPoints((+++^))
import TriCad.StlBase (StlShape(..), newStlShape)
import TriCad.StlFileWriter(writeStlToFile)
import Scan.Transform(minValueIndices, average, reduceRows)
import Scan.Parse.RawToADT(parseToScan)
import Scan.Transform(minValueIndices, average)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
--import Control.Applicative
--import Control.Monad
--import Data.Text as T
import Scan.Json()




--------------------------------------------- raw data with degree data attached -----------------------------
--This has been done. The file has been created.
parseRawDataAndSaveToJson = do
  contents <- BL.readFile "src/Data/scanRawDataWitDegrees.raww"
  let tempContents = BL.unpack contents
      --parsedData = (parseToScan (average . minValueIndices 75 ) tempContents )
      scan1 = (parseToScan  ((*1.35) .  average . minValueIndices 75 ) tempContents )
      --need to reduce rows with a Radius instead of an int
      degrees2 = [ Degree {degree=(degree x),  radii = (reduceRows 10 $ radii x)}      | x <- degrees scan1]
      scan2 = (Scan {name=(name scan1), degrees=degrees2} )
      parsedDataJson = encode scan2
  BL.writeFile "src/Data/scanFullData.json" parsedDataJson

{- bypasses json
   uses the new MathPolar fromScan functions
-}
parseRawDataToScanWriteStlFile = do
  contents <- BL.readFile "src/Data/scanRawDataWitDegrees.raww"
  let tempContents = BL.unpack contents
      --parsedData = (parseToScan (average . minValueIndices 75 ) tempContents )
      parsedData = (parseToScan  ((*1.35) .  average . minValueIndices 75 ) tempContents )
  writeStlFileFromScan parsedData
{-
parseRawDataToScanUseOldCreateCubeFunctionToWriteStl = do
  contents <- BL.readFile "src/Data/scanRawDataWitDegrees.raww"
  let tempContents = BL.unpack contents
      parsedData = (parseToScan  ((*1.35) .  average . minValueIndices 75 ) tempContents )
-}      

readTrianglesFromJsonFileAndWriteToStlFile = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  case (decode contents) of 
      Just (Scan name degrees) -> writeStlFileFromScan (Scan name degrees)
      Nothing                  -> putStrLn "Nothing"


writeStlFileFromScan scan = 
  let origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      heightPerPixel = 10
      leftFaces = createLeftFacesMultiColumnsFromScan origin (tail $ degrees scan) flatXSlope flatYSlope [0,heightPerPixel..]
      rightFaces = createRightFacesFromScan origin (head $ degrees scan) flatXSlope flatYSlope [0,heightPerPixel..]
      triangles =  concat
        [ [FacesFrontTop | x <- [1..4]],
          [FaceFront | x <- [5..224]],
          [FacesBottomFront | x <- [1,2,3,4]]
        ]
        +++^
        createVerticalCubesFromScan rightFaces leftFaces
      stlFile = newStlShape "backscratcher" triangles
  in
      writeStlToFile stlFile
      --putStrLn "done"
  
      
     


