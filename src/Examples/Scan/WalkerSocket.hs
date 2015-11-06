{-# LANGUAGE ParallelListComp #-}
module Examples.Scan.WalkerSocket() where
import Scan.ParseJuicy(process10DegreeImagesToMultiDegreeRadii, getRedLaserLineSingleImage, removeLeftOfCenterPixels, getThePixelsRightOfCenter,
                      andThen, forThe, ofThe, adjustedFor, andThe, calculateRadiusFrom)
import Data.Word(Word8)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import CornerPoints.Radius(Radius(..))
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns, createHorizontallyAlignedCubes,
                           SingleDegreeRadii(..), MultiDegreeRadii(..), transpose)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (++>), (+++), (++++), (++:), Faces(..))
import CornerPoints.Create(Slope(..), flatXSlope, flatYSlope)
import Stl.StlCornerPoints((+++^))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Scan.Filter(runningAverage, runningAvgSingleDegreeRadii)
import Scan.Transform(reduceScanRows, reduceRows, reduceScan )
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace)
import CornerPoints.FaceConversions(backFaceFromFrontFace)
import CornerPoints.Transpose (transposeZ)
{-
read in the Multidegree json file, which has valid Radii,
and process it into stl -}
reducedMultiDegreeRadiiRowsAndWriteStlWrapper :: IO ()
reducedMultiDegreeRadiiRowsAndWriteStlWrapper = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        reducedMultiDegreeRadiiRowsAndWriteMainSocketCubesStl (MultiDegreeRadii name' degrees')
        --getAnExtension (MultiDegreeRadii name' degrees')
      Nothing                                ->
        putStrLn "File not decoded"
getAnExtension :: MultiDegreeRadii -> IO ()
getAnExtension multiDegreeRadii =
  let frontAndBackTriangles = [FacesBackFront | x <- [1..]]
      reducedScanBack = reduceScan rowReductionFactor multiDegreeRadii
      reducedScanFront = transpose (+2) reducedScanBack
      rowReductionFactor = 100
      getTopCubes =  (head cubes) ++++ (map (transposeZ (+500). extractTopFace)  (head cubes)) 
      cubes = createMainSocketCubesFromFrontAndBackFaces reducedScanFront reducedScanBack
      
      
  in
      print $ show $ getTopCubes
        
{-
reduce the rows of the MulitiDegreeRadii by a factor of 100
chop of the top layer as it has an error,
output to stl

--do not call directly.
--called via reducedRowsMultiDegreeScanWrapper as it reads the json file-}
reducedMultiDegreeRadiiRowsAndWriteMainSocketCubesStl :: MultiDegreeRadii -> IO ()
reducedMultiDegreeRadiiRowsAndWriteMainSocketCubesStl (MultiDegreeRadii name' degrees') =
  let multiDegreeRadii = MultiDegreeRadii name' [(SingleDegreeRadii degree'' (tail radii''))  | (SingleDegreeRadii degree'' radii'') <- degrees']
      frontAndBackTriangles = [FacesBackFront | x <- [1..]]
      getRidOfDefectiveTopRow = [FacesNada | x <- [1..]]
      --topOfSocket = [FacesBackFrontTop | x <- [1..]]
      --topOfSocket =  [FacesBackFrontTop | x <- [0..1]] ++ [FacesNada | x <- [1..26]] ++ [FacesBackFrontTop | x <- [1..]]
      topOfSocket = [FacesBackFrontLeftTop] ++ [FacesNada | x <- [2..30]] ++ [FacesBackFrontRightTop]   ++  [FacesBackFrontTop | x <- [31..]]
      --openUpOneSide =  [frontAndBackTriangles | x <- [1..5]]
      openUpOneSide =  [openOneSideSingleLayer| x <- [1..6]]
      --openOneSideSingleLayer = [FacesBackFrontRight]   [FacesBackFront | x <- [0..1]] ++ [FacesNada | x <- [1..24]] ] -- ++ [FacesBackFrontLeft | x <- [1..]
      openOneSideSingleLayer = [FacesBackFrontLeft] ++ [FacesNada | x <- [2..30]] ++ [FacesBackFrontRight]   ++  [FacesBackFront | x <- [31..]]
      topOfMainBodyOfSocket =  (FacesBackFront : [FacesBackFrontTop | x <- [2..30]]) ++ [FacesBackFront] ++ [FacesBackFront | x <- [32..]]
      mainBodyOfSocket = [frontAndBackTriangles | x <- [1..8]]
      btmOfSocket = [FacesBackBottomFront | x <- [1..]]
      reducedScanBack = reduceScan rowReductionFactor multiDegreeRadii
      reducedScanFront = transpose (+2) reducedScanBack
      rowReductionFactor = 100
      --getTopCubes =  (map (transposeZ (+30). extractTopFace)  (head  cubes)) ++++ (head cubes)
      extendTopCubes = (map (transposeZ (+30). extractTopFace)  (head  cubes)) ++++ (map ( extractBottomFace)  (head  cubes))
      cubes = createMainSocketCubesFromFrontAndBackFaces reducedScanFront reducedScanBack
      
      triangles =
       zipWith  (+++^)
        (   topOfSocket : (  mainBodyOfSocket ++: topOfMainBodyOfSocket ++ openUpOneSide )   ++:  btmOfSocket) 
        ( extendTopCubes : tail cubes)
      stlFile = newStlShape "walker socket" $ concat triangles
  in
      writeStlToFile stlFile
{-
reducedMultiDegreeRadiiRowsAndWriteMainSocketCubesStl :: MultiDegreeRadii -> IO ()
reducedMultiDegreeRadiiRowsAndWriteMainSocketCubesStl multiDegreeRadii =
  let frontAndBackTriangles = [FacesBackFront | x <- [1..]]
      
      getRidOfDefectiveTopRow = [FacesNada | x <- [1..]]
      --topOfSocket = [FacesBackFrontTop | x <- [1..]]
      topOfSocket =  [FacesBackFrontTop | x <- [0..1]] ++ [FacesNada | x <- [1..26]] ++ [FacesBackFrontTop | x <- [1..]]
      --openUpOneSide =  [frontAndBackTriangles | x <- [1..5]]
      openUpOneSide =  [openOneSideSingleLayer| x <- [1..5]]
      openOneSideSingleLayer = [FacesBackFront | x <- [0..1]] ++ [FacesNada | x <- [1..26]] ++ [FacesBackFront | x <- [1..]]
      mainBodyOfSocket = [frontAndBackTriangles | x <- [1..10]]
      btmOfSocket = [FacesBackBottomFront | x <- [1..]]
      reducedScanBack = reduceScan rowReductionFactor multiDegreeRadii
      reducedScanFront = transpose (+2) reducedScanBack
      rowReductionFactor = 100
      
      cubes = createMainSocketCubesFromFrontAndBackFaces reducedScanFront reducedScanBack
      triangles =
       zipWith  (+++^)
        (getRidOfDefectiveTopRow :  topOfSocket : (  mainBodyOfSocket ++ openUpOneSide) ++:  btmOfSocket) 
        (cubes)
      stlFile = newStlShape "walker socket" $ concat triangles
  in
      writeStlToFile stlFile

-}
createMainSocketCubesFromFrontAndBackFaces ::  MultiDegreeRadii ->   MultiDegreeRadii -> [[CornerPoints]]
createMainSocketCubesFromFrontAndBackFaces multiDegreeRadiiOuter multiDegreeRadiiInner =
  [
    currBackFace ++++ currFrontFace
    | currFrontFace <- createMainSocketFrontFaces 100 multiDegreeRadiiOuter
    | currBackFace <- createMainSocketBackFaces 100 multiDegreeRadiiInner
  ]
  

createMainSocketFrontFacesTestWrapper = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let rowReductionFactor = 100
            reducedScanBack = reduceScan rowReductionFactor (MultiDegreeRadii name' degrees')
            reducedScanFront = transpose (+2) reducedScanBack
        --in  print $ show $ length $ createMainSocketFrontFaces rowReductionFactor reducedScanFront
        --in  print $ show $ length $ createMainSocketBackFaces rowReductionFactor reducedScanBack
        in print $ show $ length $ createMainSocketCubesFromFrontAndBackFaces reducedScanFront reducedScanBack
      Nothing                                ->
        putStrLn "nothing"




createMainSocketFrontFaces :: Int         ->  MultiDegreeRadii -> [[CornerPoints]]
createMainSocketFrontFaces    rowReductionFactor multiDegreeRadii =
  let leftFaces = createLeftFacesMultiColumns origin (tail $ degrees reducedScan) flatXSlope flatYSlope [0,heightPerPixel..]
      rightFaces = createRightFaces origin (head $ degrees reducedScan) flatXSlope flatYSlope [0,heightPerPixel..]
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      pixelsPerMM' = pixelsPerMM 
      reducedScan = transpose (+2)  multiDegreeRadii
      heightPerPixel = 1/pixelsPerMM' * (fromIntegral rowReductionFactor)
  in 
      [map (extractFrontFace) currRow  | currRow  <- (createHorizontallyAlignedCubes rightFaces leftFaces)]

createMainSocketBackFaces :: Int ->          MultiDegreeRadii -> [[CornerPoints]]
createMainSocketBackFaces    rowReductionFactor multiDegreeRadii  =
  let --outerFacesMultiDegreeRaddi = transpose (+2) multiDegreeRadii
      leftFaces = createLeftFacesMultiColumns origin (tail $ degrees multiDegreeRadii) flatXSlope flatYSlope [0,heightPerPixel..]
      rightFaces = createRightFaces origin (head $ degrees multiDegreeRadii) flatXSlope flatYSlope [0,heightPerPixel..]
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      pixelsPerMM' = pixelsPerMM 
      heightPerPixel = 1/pixelsPerMM' * (fromIntegral rowReductionFactor)
  in 
      [map (backFaceFromFrontFace . extractFrontFace) currRow  | currRow  <- (createHorizontallyAlignedCubes rightFaces leftFaces)]


writeStlFileFromRawScanWrapper :: IO ()
writeStlFileFromRawScanWrapper = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        writeStlFileFromRawScan (MultiDegreeRadii name' degrees')
      Nothing                                ->
        putStrLn "File not decoded"
{-
This was orignally is set up to have rows reduced by 10, and from a smaller jpeg image so
will need to change the number of of center front triangles

This is a support function, not to be called directly.
-}
writeStlFileFromRawScan :: MultiDegreeRadii -> IO ()
writeStlFileFromRawScan (MultiDegreeRadii name' degrees') = 
  let smoothedScan = MultiDegreeRadii name' (map (runningAvgSingleDegreeRadii 10) degrees')
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      heightPerPixel = 1/pixelsPerMM
      leftFaces = createLeftFacesMultiColumns origin (tail $ degrees smoothedScan) flatXSlope flatYSlope [0,heightPerPixel..]
      rightFaces = createRightFaces origin (head $ degrees smoothedScan) flatXSlope flatYSlope [0,heightPerPixel..]
      frontTriangles = [FaceFront | x <- [1..]]
      triangles =
       zipWith  (+++^)
        ([FacesFrontTop | x <- [1..]] :   [frontTriangles | x <- [1..1940]] ++:  [FacesBottomFront | x <- [1..]] 

        ) 
        --  +++^
        (createHorizontallyAlignedCubes rightFaces leftFaces)
      stlFile = newStlShape "backscratcher" $ concat triangles
  in
      writeStlToFile stlFile
      --print origin
{-
as copied from singleLine.hs
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
-}
{-
process the raw MultiDegreeRadii into radius.
-}
processMultiDegreeRadii :: IO ()
processMultiDegreeRadii = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let multiDegreeRadii =
              MultiDegreeRadii
                name'
                [ processSingleDegreeRadii currSingleDegreeRadii
                  | currSingleDegreeRadii <- degrees'
                ]
               
        in
           BL.writeFile "src/Data/scanFullData.json" $ encode $ multiDegreeRadii
           
      Nothing                              -> putStrLn "Nothing"

{-
compiles and writes json file.
processMultiDegreeRadii = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let multiDegreeRadii =
              MultiDegreeRadii
                name'
                [ processSingleDegreeRadii currSingleDegreeRadii
                  | currSingleDegreeRadii <- degrees'
                ]
               
        in
           BL.writeFile "src/Data/scanFullData.json" $ encode $ multiDegreeRadii
           
      Nothing                              -> putStrLn "Nothing"

-}


{-process a SingleDegreeRadii, to go from raw data to a Radius of the scan.-}
processSingleDegreeRadii :: SingleDegreeRadii -> SingleDegreeRadii
processSingleDegreeRadii    (SingleDegreeRadii degree' radii')    =
  let adjustForCenter = getThePixelsRightOfCenter $ andThen  (removeLeftOfCenterPixels btmCenterIndex topCenterIndex imageHeight)
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

--look at the results of processSingleDegreeRadii
--can be deleted later.
showSingleDegreeRadii = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  case (decode contents) of
      Just (MultiDegreeRadii name degrees) -> print $ show $ processSingleDegreeRadii $ head degrees
      Nothing                              -> putStrLn "Nothing"


{- |Process all 36 images, reducing down to MultiDegreeRadii of the red laser line.
This is the red laser line in pixels Right of Left hand edge.
It is written out to json as a MultiDegreeRadii, even though the data is
in the form of pixel indices RightOfLHS, in other words, not processed at all beyond finding the laser line.
-}
processImagesToRedLaserLineAsPixelsRightOfLHS = process10DegreeImagesToMultiDegreeRadii (getRedLaserLineSingleImage redLaserLine)



--as calculated by the calibration picture center.JPG
pixelsPerMM = 696/38 -- 620 pixels / 38 mm
--pixelsPerMMVertical = 704.2/38   -- 520 pixels / 38 mm

--ToDo: Find these 2 values from a centering image using
topCenterIndex = 1591
btmCenterIndex = 1563.5
imageWidth = 2592
imageHeight = 1944
--pixelsPerMillimeter = 38/623 -- 623/38
cameraAngle = 30


--180 fails at degree 340
type TargetValue = Word8
redLaserLine :: TargetValue
redLaserLine = 175
