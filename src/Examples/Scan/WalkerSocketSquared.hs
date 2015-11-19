module Examples.Scan.WalkerSocketSquared where

import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateMDR)
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns, createVerticalWalls,
                                  createHorizontallyAlignedCubesNoSlope, createHorizontallyAlignedCubes)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), Faces(..), (|@+++#@|), (&+++#@), CornerPointsBuilder(..))
import CornerPoints.Create(Slope(..), flatXSlope, flatYSlope, Angle(..), Origin(..))
import Stl.StlCornerPoints((|+++^|), (||+++^||))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Scan.Filter(runningAverage, runningAvgSingleDegreeRadii)
--import Scan.Transform(reduceScanRows, reduceRows, reduceScan )
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace, extractBackFace)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import Helpers.List((++:))
import Primitives.Cylindrical(cylinderWallsNoSlopeSquaredOff, cylinderSolidNoSlopeSquaredOff,
                                   cylinderWallsNoSlopeSquaredOffLengthenY, cylinderSolidNoSlopeSquaredOffLengthenY)
import Primitives.Cylindrical(cylinderWallsNoSlope)
import CornerPoints.Transposable(transpose)
import Data.Word(Word8)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Scan.ParseJuicy(process10DegreeImagesToMultiDegreeRadii, getRedLaserLineSingleImage, removeLeftOfCenterPixels, getThePixelsRightOfCenter,  reduceScanRows, reduceRows, reduceScan,
                      andThen, forThe, ofThe, adjustedFor, andThe, calculateRadiusFrom)

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
      plateRadius = 30
      power = 2.5
      lengthenYFactor = 20
      extensionFaceBuilder :: (Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]
      extensionFaceBuilder leftFace emptyFaces rightFace fillerFaces =
        [leftFace] ++ [emptyFaces | x <- [2..30]] ++ [rightFace]   ++  [fillerFaces | x <- [31..]]
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --enlarge it to fit over the socket already printed with WalkerSocket. 1st attempt at +2 was not quite big enough, trying 3.
            --rotate it to line up better with the riser
            innerSleeveMDR = rotateMDR $ rotateMDR $ transpose (+3) $ reduceScan rowReductionFactor $ removeDefectiveTopRow (MultiDegreeRadii name' degrees')
            --give it a thickness of 3 mm
            outerSleeveMDR = transpose (+3) innerSleeveMDR
            plateRadius = 24
        in  --mainSocketStl innerSleeveMDR outerSleeveMDR extensionFaceBuilder extensionHeight rowReductionFactor pixelsPerMM
            pushPlate plateRadius power lengthenYFactor
            --hosePlate plateRadius power lengthenYFactor
            
      Nothing                                ->
        putStrLn "File not decoded"

{-
The attaches to the pushPlate and the hose
                     ||  ||   hose
                     ||  ||   
                    |||  |||  riser
              |||||||||||||||||||  innerHose riserBase outsideScrews
-}
hosePlate ::  PlateRadius -> Power -> LengthenYFactor -> IO ()
hosePlate plateRadius power lengthenYFactor =
  let
    
    wallThickness = 3
    hoseInnerRadius = 7.5
    hoseThickness = wallThickness
    hoseOuterRadius = hoseThickness + hoseInnerRadius
    riserInnerRadius = hoseOuterRadius
    riserThickness = 5 --10
    riserOuterRadius = riserThickness + riserInnerRadius
    screwInsideRadius = riserOuterRadius
    screwsThickness = plateRadius - screwInsideRadius
    
    

    baseHeight = 3
    riserHeight = 17
    hoseHeight = 20
      
    baseOrigin = Point 0 0 0
    riserOrigin = transposeZ ( + baseHeight) baseOrigin
      
    angles = (map (Angle) [0,10..360])
    stlFile = newStlShape "walker socket hose plate" $
                [
                    [FacesBackFrontTop | x <- [1..]],    --hose
                    [FacesBackFront | x <- [1..]],       --riser
                    [FaceBottom | x <- [1..]],       --riserBase
                    [FacesBottomFrontTop | x <- [1..]], --ousideScrews
                    [FacesBottomTop | x <- [1..]]      --innerHose
                    
                ]
                ||+++^||
                (
                 getCornerPoints $
                  (CornerPointsBuilder
                   [ 
                    (cylinderWallsNoSlopeSquaredOffLengthenY  (Radius riserInnerRadius) baseOrigin angles baseHeight riserThickness power lengthenYFactor),--riser base
                    (cylinderWallsNoSlopeSquaredOffLengthenY  (Radius screwInsideRadius) baseOrigin angles baseHeight screwsThickness power lengthenYFactor),--outer screw ring
                    (cylinderSolidNoSlopeSquaredOffLengthenY  (Radius hoseOuterRadius)   baseOrigin    angles     baseHeight  power    lengthenYFactor) --innerHose
                   ]
                   &+++#@ (|+++|  [extractTopFace x | x <- (cylinderWallsNoSlope (Radius hoseInnerRadius) hoseThickness   riserOrigin angles riserHeight)]) -- riser
                   &+++#@ (|@+++#@| ((transposeZ (+20)) . extractTopFace) ) --hose
                  )
                )
    
  
  in
    writeStlToFile stlFile      
  
     

{-
Attaches directly to the socket
-}
pushPlate :: PlateRadius -> Power -> LengthenYFactor -> IO ()
pushPlate    plateRadius    power    lengthenYFactor  = 
  let
    origin = Point 0 0 0 :: Origin
    angles = (map (Angle) [0,10..360])
    plateHeight = 3
    --power = 2.5
    riserThickness = 3
    --lengthenFactor = 20
    
    stlFile = newStlShape "walker socket push plate" $
      [
        [FacesBackBottomTop | x <- [1..]], --center of plate
        (riserFaceBuilder FacesBottomFront  FacesBottomFront      FacesBottomFrontTop FacesBottomFront       FacesBottomFront), --outer ring
        (riserFaceBuilder FacesBackFrontTop FacesBackFrontLeftTop FacesNada           FacesBackFrontRightTop FacesBackFrontTop)--riser
      ]
      ||+++^||
      [ --the center of the plate
        (cylinderSolidNoSlopeSquaredOffLengthenY  (Radius 21)   origin    angles     plateHeight  power    lengthenYFactor),
        --the outer ring, which has an inner radius = outer radius of riser on socket
        (cylinderWallsNoSlopeSquaredOffLengthenY  (Radius (plateRadius - riserThickness))  origin angles plateHeight riserThickness power lengthenYFactor),
        --the riser that takes off from the outer ring
        (cylinderWallsNoSlopeSquaredOffLengthenY  (Radius (plateRadius - riserThickness))   (transposeZ (+plateHeight)origin)  angles (30 :: Height) riserThickness  power  lengthenYFactor)
      ]

    
  in
    writeStlToFile stlFile


{-
reduce the rows of the MulitiDegreeRadii by a factor of 100
chop of the top layer as it has an error,
output to stl

--do not call directly.
--called via reducedRowsMultiDegreeScanWrapper as it reads the json file-}
mainSocketStl :: MultiDegreeRadii -> MultiDegreeRadii -> ((Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]) ->
                 ExtensionHeight -> RowReductionFactor -> PixelsPerMillimeter ->  IO ()
mainSocketStl    innerSleeveMDR      outerSleeveMDR      extensionFaceBuilder extensionHeight    rowReductionFactor pixelsPerMM  =
  let transposeFactors = [0,heightPerPixel.. ]
      heightPerPixel = 1/pixelsPerMM * (fromIntegral rowReductionFactor)
      origin = (Point{x_axis=0, y_axis=0, z_axis=50})
      angles = (map (Angle) [0,10..360])

      riser = cylinderWallsNoSlopeSquaredOff  (Radius 18)  (transposeX (+0)(transposeY (+(-15))(transposeZ (+(-15))origin)))    angles (20::Height) (3::Thickness) (2.5::Power)
      riserTriangles =
        (riserFaceBuilder FacesBackFrontTop FacesBackFrontLeftTop FacesNada FacesBackFrontRightTop FacesBackFrontTop)
        |+++^|
        riser   
          
      mainBodyCubes =  getCornerPoints $
        (CornerPointsBuilder $
          drop 6  (createVerticalWalls  innerSleeveMDR outerSleeveMDR origin transposeFactors) --main body of the socket, with head removed. 
          
        )
        &+++#@ (|+++| [upperFaceFromLowerFace $ extractBottomFace x | x <- riser ] ) --socketToRiser
      
      socketTriangles =
        (
          [riserFaceBuilder FacesBackFront FacesBackFrontLeft FacesNada FacesBackFrontRight FacesBackFront] ++  --the socket to riser transition
          [riserFaceBuilder FacesBackFront FacesBackFront (FacesBackFrontTop) FacesBackFront FacesBackFront]  ++ --top layer of the socket
          [[FacesBackFront | x <- [1..]] | x <- []] ++  --get rid of a most layers, so that just the top is printed out, for fixing 1st print of socket.
                                                           --leave empty to get rid of all mid-layers. Use 1..9 for all layers printed
          [ [FacesBackBottomFront | x <- [1..]]   ]         --bottom of the socket      
        )
        ||+++^|| 
        ( getCornerPoints $
          (CornerPointsBuilder $
            drop 6  (createVerticalWalls  innerSleeveMDR outerSleeveMDR origin transposeFactors) --main body of the socket, with head removed. 
          )
          &+++#@ (|+++| [upperFaceFromLowerFace $ extractBottomFace x | x <- riser ] ) --socketToRiser
        )

                  
      
      sleeveStlFile = newStlShape "walker sleeve" $ socketTriangles ++ riserTriangles
  in
      writeStlToFile sleeveStlFile


--build the faces for the squared riser and assoc'd layers
riserFaceBuilder :: (Faces) -> (Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]
riserFaceBuilder faces1 faces2 faces3 faces4 faces5 =
 [faces1| x <- [1..8]] ++ [faces2] ++ [faces3| x <- [10..28]] ++ [faces4]   ++  [faces5 | x <- [11..]]

pixelsPerMM = 696/38 

type RowReductionFactor = Int
--type Origin = Point
type ExtensionHeight = Double
type PlateRadius = Double
type PixelsPerMillimeter = Double
type TransposeFactor = Double
type Thickness = Double
type Height = Double
type Power = Double
type LengthenYFactor = Double

