module Examples.Scan.WalkerSocketSquared where

import CornerPoints.Radius(MultiDegreeRadii(..), SingleDegreeRadii(..), Radius(..),extractSingle, extractList, rotateMDR)
import CornerPoints.VerticalFaces(createRightFaces, createLeftFaces, createLeftFacesMultiColumns, createVerticalWalls,
                                  createHorizontallyAlignedCubesNoSlope, createHorizontallyAlignedCubes)
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPoints(CornerPoints(..), (+++), (|+++|), Faces(..), (|@+++#@|), (&+++#@), CornerPointsBuilder(..))
import CornerPoints.Create(Slope(..), flatXSlope, flatYSlope, Angle(..))
import Stl.StlCornerPoints((|+++^|), (||+++^||))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import Scan.Filter(runningAverage, runningAvgSingleDegreeRadii)
--import Scan.Transform(reduceScanRows, reduceRows, reduceScan )
import CornerPoints.FaceExtraction (extractFrontFace, extractTopFace,extractBottomFace)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.Transpose (transposeZ, transposeY, transposeX)
import Helpers.List((++:))
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces, cylinderWallsNoSlope, cylinderSolidNoSlope, cylinderWallsNoSlopeSquaredOff )
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
            
      extensionFaceBuilder :: (Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]
      extensionFaceBuilder leftFace emptyFaces rightFace fillerFaces =
        [leftFace] ++ [emptyFaces | x <- [2..30]] ++ [rightFace]   ++  [fillerFaces | x <- [31..]]
  case (decode contents) of
   
      Just (MultiDegreeRadii name' degrees') ->
        let --enlarge it to fit over the socket already printed with WalkerSocket.
            --rotate it to line up better with the riser
            innerSleeveMDR = rotateMDR $ rotateMDR $ transpose (+2) $ reduceScan rowReductionFactor $ removeDefectiveTopRow (MultiDegreeRadii name' degrees')
            --give it a thickness of 3 mm
            outerSleeveMDR = transpose (+3) innerSleeveMDR
        in  mainSocketStl innerSleeveMDR outerSleeveMDR extensionFaceBuilder extensionHeight rowReductionFactor pixelsPerMM
            
      Nothing                                ->
        putStrLn "File not decoded"


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

      
      --topOfMainBody = extensionFaceBuilder (FacesBackFront) (FacesBackFrontTop) (FacesBackFront) (FacesBackFront)
      --topOfMainBody = extensionFaceBuilder (FaceBack) (FacesBackFrontTop) (FaceBack) (FaceBack)
      --topOfMainBody = extensionFaceBuilder (FacesNada) (FacesBackFrontTop) (FacesBackFront) (FacesBackFront)
      {-
       extensionFaceBuilder :: (Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]
      extensionFaceBuilder leftFace emptyFaces rightFace fillerFaces =
        [leftFace] ++ [emptyFaces | x <- [2..30]] ++ [rightFace]   ++  [fillerFaces | x <- [31..]]


      riserFaces =  ( [FacesBackFront | x <- [1..9]] ++
                         [FacesNada | x <- [10..27]]    ++
                         [FacesBackFront | x <- [28..]]
                       )
      -}
      --riserFaceBuilder = extensionFaceBuilder FacesBackFront FacesNada FacesBackFront FacesBackFront
      riserFaceBuilder :: (Faces) -> (Faces) -> (Faces) -> (Faces) -> (Faces) -> [Faces]
      riserFaceBuilder faces1 faces2 faces3 faces4 faces5 =
        [faces1| x <- [1..8]] ++ [faces2] ++ [faces3| x <- [10..27]] ++ [faces4]   ++  [faces5 | x <- [11..]]
      
      riserFaces =  riserFaceBuilder FacesBackFrontTop FacesBackFrontLeftTop FacesNada FacesBackFrontRightTop FacesBackFrontTop
      adaptorFaces = riserFaceBuilder FacesBackFront FacesBackFrontLeft FacesNada FacesBackFrontRight FacesBackFront
      topOfSocketFaces = riserFaceBuilder FacesBackFront FacesBackFront (FacesBackFrontTop) FacesBackFront FacesBackFront
                    
      socketAndAdaptor = -- ++
                           [adaptorFaces] ++ [topOfSocketFaces]  ++
                           [[FacesBackFront | x <- [1..]] | x <- [1..9]] ++  --get rid of a most layers, so that just the top is printed out, for fixing 1st print of socket.
                           [ [FacesBackBottomFront | x <- [1..]]   ]

                         

      --cut off the top 6 layers of the socket
      --removeTopOfSocket = [[FacesBackFront | x <- [1..]] | x <- [1..6] ]

          
      --mainBodyCubes = createVerticalWalls  innerSleeveMDR outerSleeveMDR origin transposeFactors
      mainBodyCubes =  getCornerPoints $
        (CornerPointsBuilder $
          drop 6  (createVerticalWalls  innerSleeveMDR outerSleeveMDR origin transposeFactors) --main body of the socket, with head removed. 
          
        )
        &+++#@ (|+++| [upperFaceFromLowerFace $ extractBottomFace x | x <- riser ] ) --socketToRiser
      
      socketTriangles =
        (
          --((topOfMainBody : mainBody ))
          --((removeTopOfSocket ++ theNext10TopLayers ))
          socketAndAdaptor
          
           
        )
        ||+++^||
        ( mainBodyCubes)

      --riser = cylinderWallsNoSlopeSquaredOff  (Radius 22) 3            (transposeX (+4)(transposeY (+(-10))(transposeZ (+(-5))origin)))    angles     100       2.5
      riser =   cylinderWallsNoSlopeSquaredOff  (Radius 18) 3            (transposeX (+0)(transposeY (+(-15))(transposeZ (+(-15))origin)))    angles     20       2.5
      riserTriangles =  riserFaces  |+++^| riser
      
      sleeveStlFile = newStlShape "walker sleeve" $ socketTriangles ++ riserTriangles
  in
      writeStlToFile sleeveStlFile




pixelsPerMM = 696/38 

type RowReductionFactor = Int
type Origin = Point
type ExtensionHeight = Double
type PlateRadius = Double
type PixelsPerMillimeter = Double
type TransposeFactor = Double
type Thickness = Double
type Height = Double
