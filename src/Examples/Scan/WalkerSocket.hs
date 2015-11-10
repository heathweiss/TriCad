{-# LANGUAGE ParallelListComp #-}
module Examples.Scan.WalkerSocket() where
import Scan.ParseJuicy(process10DegreeImagesToMultiDegreeRadii, getRedLaserLineSingleImage, removeLeftOfCenterPixels, getThePixelsRightOfCenter,  reduceScanRows, reduceRows, reduceScan,
                      andThen, forThe, ofThe, adjustedFor, andThe, calculateRadiusFrom)
import Data.Word(Word8)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList)
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns, createHorizontallyAlignedCubes,
                             transpose)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..), (+++>>), (++++>>))
import CornerPoints.Create(Slope(..), flatXSlope, flatYSlope, Angle(..))
import Stl.StlCornerPoints((+++^), (++++^))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Scan.Filter(runningAverage, runningAvgSingleDegreeRadii)
--import Scan.Transform(reduceScanRows, reduceRows, reduceScan )
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.Transpose (transposeZ)
import Helpers.List((++:))
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces)
{-
read in the Multidegree json file, which has valid Radii,
and process it into stl using whatever function required for the current shape. -}
loadMDRAndPassToProcessor :: IO ()
loadMDRAndPassToProcessor = do
  contents <- BL.readFile "src/Data/scanFullData.json"
  let removeDefectiveTopRow :: MultiDegreeRadii -> MultiDegreeRadii
      removeDefectiveTopRow (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' [(SingleDegreeRadii degree'' (tail radii''))  | (SingleDegreeRadii degree'' radii'') <- degrees']
      rowReductionFactor = 100
      extensionHeight = 30

      createMainSocketBackFaces :: Origin -> MultiDegreeRadii -> [[CornerPoints]]
      createMainSocketBackFaces    origin    multiDegreeRadii  =
        let leftFaces = createLeftFacesMultiColumns origin (tail $ degrees multiDegreeRadii) flatXSlope flatYSlope [0,heightPerPixel..]
            rightFaces = createRightFaces origin (head $ degrees multiDegreeRadii) flatXSlope flatYSlope [0,heightPerPixel..]
            pixelsPerMM' = pixelsPerMM 
            heightPerPixel = 1/pixelsPerMM' * (fromIntegral rowReductionFactor)
        in  [map (backFaceFromFrontFace . extractFrontFace) currRow  | currRow  <- (createHorizontallyAlignedCubes rightFaces leftFaces)]

      createMainSocketFrontFaces :: Origin ->  MultiDegreeRadii -> [[CornerPoints]]
      createMainSocketFrontFaces    origin     multiDegreeRadii =
        let leftFaces = createLeftFacesMultiColumns origin (tail $ degrees reducedScan) flatXSlope flatYSlope [0,heightPerPixel..]
            rightFaces = createRightFaces origin (head $ degrees reducedScan) flatXSlope flatYSlope [0,heightPerPixel..]
            --origin = (Point{x_axis=0, y_axis=0, z_axis=50})
            pixelsPerMM' = pixelsPerMM 
            reducedScan = transpose (+2)  multiDegreeRadii
            heightPerPixel = 1/pixelsPerMM' * (fromIntegral rowReductionFactor)
        in  [map (extractFrontFace) currRow  | currRow  <- (createHorizontallyAlignedCubes rightFaces leftFaces)]   
         
      extensionFaceBuilder :: (Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]
      extensionFaceBuilder leftFace emptyFaces rightFace fillerFaces =
        [leftFace] ++ [emptyFaces | x <- [2..30]] ++ [rightFace]   ++  [fillerFaces | x <- [31..]]
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let innerSleeveMDR = reduceScan rowReductionFactor $ removeDefectiveTopRow (MultiDegreeRadii name' degrees')
            outerSleeveMDR = transpose (+2) innerSleeveMDR
        in  mainSocketStl innerSleeveMDR outerSleeveMDR createMainSocketBackFaces createMainSocketFrontFaces extensionFaceBuilder extensionHeight
            --basePlateStl outerSleeveMDR createMainSocketBackFaces createMainSocketFrontFaces extensionFaceBuilder extensionHeight
      Nothing                                ->
        putStrLn "File not decoded"
{-

-}

basePlateStl :: MultiDegreeRadii -> (Origin -> MultiDegreeRadii -> [[CornerPoints]]) -> (Origin -> MultiDegreeRadii -> [[CornerPoints]])
                -> ((Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]) -> ExtensionHeight -> IO ()
basePlateStl outerSleeveMDR createMainSocketBackFaces createMainSocketFrontFaces extensionFaceBuilder extensionHeight  =
  let plateRadius = 30
      conOuterMDR = transpose (+2) outerSleeveMDR
      conTopOrigin = (Point (-10) (-15) 20)
      topRowOfSleeveCubes = concat
       [
        currBackFace ++++ currFrontFace
        | currFrontFace <- createMainSocketFrontFaces conTopOrigin $  extractList (take 2)  conOuterMDR
        | currBackFace <- createMainSocketBackFaces conTopOrigin  $  extractList (take 2) outerSleeveMDR
       ]

      conCubes = plateToConCubes ++++>> ((transposeZ (+extensionHeight)) . extractTopFace )
      conTriangles = (extensionFaceBuilder (FacesBackFrontLeftTop) (FacesNada) (FacesBackFrontRightTop) (FacesBackFrontTop)) +++^  conCubes

      plateToConTopFaces = [extractTopFace currCube | currCube <- topRowOfSleeveCubes]
      plateToConCubes = plateToConTopFaces ++++ (map (lowerFaceFromUpperFace . extractTopFace) plateUpperCubes)
      plateToConTriangles = (extensionFaceBuilder (FacesBackFrontLeft) (FacesNada) (FacesBackFrontRight) (FacesBackFront)) +++^  plateToConCubes
      
      plateUpperCubes =  map (+++>> ((transposeZ (+5)) . extractTopFace)) plateOuterCubes
      plateUpperTriangles = (extensionFaceBuilder (FacesBackFrontLeft) (FacesNada) (FacesBackFrontRight) (FacesBackFront)) +++^  plateUpperCubes

      plateOuterBtmFaces =  createBottomFaces (Point 0 0 0) (map (Radius) [(plateRadius + 2),(plateRadius+2)..]) (map (Angle) [0,10..360]) flatXSlope flatYSlope
      plateOuterCubesTemp = plateOuterBtmFaces ++++>> (upperFaceFromLowerFace . (transposeZ (+3)))
      plateOuterFrontFaces = [ (extractFrontFace) currCube    |currCube <- plateOuterCubesTemp]
      plateOuterBackFaces = [(backFaceFromFrontFace . extractFrontFace) currCube  |currCube <- plateInnerCubes]
      plateOuterCubes = plateOuterBackFaces ++++ plateOuterFrontFaces
      plateOuterTriangles = (extensionFaceBuilder (FacesBottomFront) (FacesBottomFrontTop) (FacesBottomFront) (FacesBottomFront)) +++^  plateOuterCubes

      plateInnerBtmFaces = createBottomFaces (Point 0 0 0) (map (Radius) [plateRadius,plateRadius..]) (map (Angle) [0,10..360]) flatXSlope flatYSlope
      plateInnerCubes = plateInnerBtmFaces ++++>> (upperFaceFromLowerFace . (transposeZ (+3)))
      plateInnerTriangles = (extensionFaceBuilder (FacesBottomTop) (FacesBottomTop) (FacesBottomTop) (FacesBottomTop)) +++^  plateInnerCubes

      
      
      plateCubesStl = newStlShape "walker base plate" $    plateInnerTriangles ++ plateOuterTriangles ++
                                                           plateUpperTriangles ++ plateToConTriangles ++
                                                           conTriangles 
      
  in  --putStrLn "temp"
      --writeStlToFile conCubesStl
      writeStlToFile plateCubesStl
      
{-
basePlateStl :: MultiDegreeRadii -> (Origin -> MultiDegreeRadii -> [[CornerPoints]]) -> (Origin -> MultiDegreeRadii -> [[CornerPoints]])
                -> ((Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]) -> ExtensionHeight -> IO ()
basePlateStl outerSleeveMDR createMainSocketBackFaces createMainSocketFrontFaces extensionFaceBuilder extensionHeight  =
  let plateRadius = 30
      conOuterMDR = transpose (+2) outerSleeveMDR
      conTopOrigin = (Point (-10) (-15) 20)
      topRowOfSleeveCubes = concat
       [
        currBackFace ++++ currFrontFace
        | currFrontFace <- createMainSocketFrontFaces conTopOrigin $  extractList (take 2)  conOuterMDR
        | currBackFace <- createMainSocketBackFaces conTopOrigin  $  extractList (take 2) outerSleeveMDR
       ]

      conCubes = plateToConCubes ++++>> ((transposeZ (+extensionHeight)) . extractTopFace )
      conTriangles = (extensionFaceBuilder (FacesBackFrontLeftTop) (FacesNada) (FacesBackFrontRightTop) (FacesBackFrontTop)) +++^  conCubes

      plateToConTopFaces = [extractTopFace currCube | currCube <- topRowOfSleeveCubes]
      --temp just too look at it.
      --need to add conTopFaces to the plate top faces
      --conCubes = conTopFaces ++++>> ((transposeZ (+(-10))) . lowerFaceFromUpperFace )
      plateToConCubes = plateToConTopFaces ++++ (map (lowerFaceFromUpperFace . extractTopFace) plateUpperCubes)
      plateToConTriangles = (extensionFaceBuilder (FacesBackFrontLeft) (FacesNada) (FacesBackFrontRight) (FacesBackFront)) +++^  plateToConCubes
      --conCubesStl = newStlShape "walker base plate" $ [FacesBackFrontTop | x <- [1..]] +++^  conCubes

      plateUpperCubes =  map (+++>> ((transposeZ (+5)) . extractTopFace)) plateOuterCubes
      plateUpperTriangles = (extensionFaceBuilder (FacesBackFrontLeft) (FacesNada) (FacesBackFrontRight) (FacesBackFront)) +++^  plateUpperCubes

      

      plateOuterBtmFaces =  createBottomFaces (Point 0 0 0) (map (Radius) [(plateRadius + 2),(plateRadius+2)..]) (map (Angle) [0,10..360]) flatXSlope flatYSlope
      plateOuterCubesTemp = plateOuterBtmFaces ++++>> (upperFaceFromLowerFace . (transposeZ (+3)))
      plateOuterFrontFaces = [ (extractFrontFace) currCube    |currCube <- plateOuterCubesTemp]
      plateOuterBackFaces = [(backFaceFromFrontFace . extractFrontFace) currCube  |currCube <- plateInnerCubes]
      plateOuterCubes = plateOuterBackFaces ++++ plateOuterFrontFaces
      plateOuterTriangles = (extensionFaceBuilder (FacesBottomFront) (FacesBottomFrontTop) (FacesBottomFront) (FacesBottomFront)) +++^  plateOuterCubes

      plateInnerBtmFaces = createBottomFaces (Point 0 0 0) (map (Radius) [plateRadius,plateRadius..]) (map (Angle) [0,10..360]) flatXSlope flatYSlope
      plateInnerCubes = plateInnerBtmFaces ++++>> (upperFaceFromLowerFace . (transposeZ (+3)))
      plateInnerTriangles = (extensionFaceBuilder (FacesBottomTop) (FacesBottomTop) (FacesBottomTop) (FacesBottomTop)) +++^  plateInnerCubes
      --plateCubesStl = newStlShape "walker base plate" plateInnerTriangles

      
      plateCubesStl = newStlShape "walker base plate" $    plateInnerTriangles ++ plateOuterTriangles ++
                                                           plateUpperTriangles ++ plateToConTriangles ++
                                                           conTriangles
      
  in  --putStrLn "temp"
      --writeStlToFile conCubesStl
      writeStlToFile plateCubesStl

-}
{-
reduce the rows of the MulitiDegreeRadii by a factor of 100
chop of the top layer as it has an error,
output to stl

--do not call directly.
--called via reducedRowsMultiDegreeScanWrapper as it reads the json file-}
mainSocketStl :: MultiDegreeRadii -> MultiDegreeRadii -> (Origin -> MultiDegreeRadii -> [[CornerPoints]]) -> (Origin -> MultiDegreeRadii -> [[CornerPoints]])
                 -> ((Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]) -> ExtensionHeight  -> IO ()
mainSocketStl    innerSleeveMDR      outerSleeveMDR      createMainSocketBackFaces  createMainSocketFrontFaces extensionFaceBuilder extensionHeight                                            =
  let 
      
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      createMainSocketCubesFromFrontAndBackFaces ::  MultiDegreeRadii ->   MultiDegreeRadii -> [[CornerPoints]]
      createMainSocketCubesFromFrontAndBackFaces multiDegreeRadiiOuter multiDegreeRadiiInner =
       [
        currBackFace ++++ currFrontFace
        | currFrontFace <- createMainSocketFrontFaces origin multiDegreeRadiiOuter
        | currBackFace <- createMainSocketBackFaces origin multiDegreeRadiiInner
       ]

      topOfExtension = extensionFaceBuilder (FacesBackFrontLeftTop) (FacesNada) (FacesBackFrontRightTop) (FacesBackFrontTop)
      bodyOfExtension = [
                        extensionFaceBuilder (FacesBackFrontLeft)(FacesNada)(FacesBackFrontRight)(FacesBackFront)
                        | x <- [1..6]
                      ]
      topOfMainBody = extensionFaceBuilder (FacesBackFront) (FacesBackFrontTop) (FacesBackFront) (FacesBackFront)
      mainBody = [
                           [FacesBackFront | x <- [1..]]
                           | x <- [1..9]
                 ] 
      --btmOfMainBody = [FacesBackBottomFront | x <- [1..10]]  ++  [FacesBackBottom | x <- [1..]]
      btmOfMainBody = [FacesBackBottomFront | x <- [1..3]]   ++
                      [FacesBackBottom | x <- [1..5]] ++
                      [FacesBackBottomFront | x <- [1..15]] ++
                      [FacesBackBottom | x <- [1..5 ]] ++
                      [FacesBackBottomFront | x <- [1.. ]]
      
      tieDownMDR = transpose (+20) outerSleeveMDR
      tieDownCubes = tail $ createMainSocketCubesFromFrontAndBackFaces tieDownMDR outerSleeveMDR
      noShowTieDownFaces = [
                           [FacesNada | x <- [1..]]
                           | x <- [1..16]
                 ]
      showTieDownFaces = [ [FacesNada | x <- [1..3]] ++
                           [FacesBottomFrontRightTop] ++
                           [FacesBottomFrontTop | x <- [1..3]] ++
                           [FacesBottomFrontLeftTop] ++
                           [FacesNada | x <- [1..15]] ++
                           [FacesBottomFrontRightTop] ++
                           [FacesBottomFrontTop | x <- [1..3]] ++
                           [FacesBottomFrontLeftTop] ++
                           [FacesNada | x <- [1..]]
                           | x <- [1..]
                 ]
      tieDownTriangles = (noShowTieDownFaces ++ showTieDownFaces) ++++^ tieDownCubes
      -- ((topOfExtension : bodyOfExtension) ++  (topOfMainBody : mainBody ))   ++:  btmOfMainBody
                  
      mainBodyCubes = createMainSocketCubesFromFrontAndBackFaces outerSleeveMDR innerSleeveMDR
      extensionCubes = (map (transposeZ (+extensionHeight). extractTopFace)  (head  mainBodyCubes)) ++++ (map ( extractBottomFace)  (head  mainBodyCubes))
      

      triangles =
        (
          ((topOfExtension : bodyOfExtension) ++  (topOfMainBody : mainBody ))   ++:  btmOfMainBody
        )
        ++++^
        ( extensionCubes : tail mainBodyCubes)

      sleeveStlFile = newStlShape "walker sleeve" $ triangles ++ tieDownTriangles
  in
      writeStlToFile sleeveStlFile
{-
mainSocketStl :: MultiDegreeRadii -> MultiDegreeRadii -> (Origin -> MultiDegreeRadii -> [[CornerPoints]]) -> (Origin -> MultiDegreeRadii -> [[CornerPoints]])
                 -> ((Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]) -> ExtensionHeight  -> IO ()
mainSocketStl    innerSleeveMDR      outerSleeveMDR      createMainSocketBackFaces  createMainSocketFrontFaces extensionFaceBuilder extensionHeight                                            =
  let 
      
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      createMainSocketCubesFromFrontAndBackFaces ::  MultiDegreeRadii ->   MultiDegreeRadii -> [[CornerPoints]]
      createMainSocketCubesFromFrontAndBackFaces multiDegreeRadiiOuter multiDegreeRadiiInner =
       [
        currBackFace ++++ currFrontFace
        | currFrontFace <- createMainSocketFrontFaces origin multiDegreeRadiiOuter
        | currBackFace <- createMainSocketBackFaces origin multiDegreeRadiiInner
       ]

      topOfExtension = extensionFaceBuilder (FacesBackFrontLeftTop) (FacesNada) (FacesBackFrontRightTop) (FacesBackFrontTop)
      bodyOfExtension = [
                        extensionFaceBuilder (FacesBackFrontLeft)(FacesNada)(FacesBackFrontRight)(FacesBackFront)
                        | x <- [1..6]
                      ]
      topOfMainBody = extensionFaceBuilder (FacesBackFront) (FacesBackFrontTop) (FacesBackFront) (FacesBackFront)
      mainBody = [
                           [FacesBackFront | x <- [1..]]
                           | x <- [1..8]
                         ] 
      btmOfMainBody = [FacesBackBottomFront | x <- [1..]]

      
      mainBodyCubes = createMainSocketCubesFromFrontAndBackFaces outerSleeveMDR innerSleeveMDR
      extensionCubes = (map (transposeZ (+extensionHeight). extractTopFace)  (head  mainBodyCubes)) ++++ (map ( extractBottomFace)  (head  mainBodyCubes))
      

      triangles =
        (
          ((topOfExtension : bodyOfExtension) ++  (topOfMainBody : mainBody ))   ++:  btmOfMainBody
        )
        ++++^
        ( extensionCubes : tail mainBodyCubes)

      sleeveStlFile = newStlShape "walker sleeve" triangles
  in
      writeStlToFile sleeveStlFile


-}





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

type RowReductionFactor = Int
type Origin = Point
type ExtensionHeight = Double
