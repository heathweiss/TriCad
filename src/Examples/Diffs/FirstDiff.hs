{-# LANGUAGE TemplateHaskell #-}
module Examples.Diffs.FirstDiff where
import CornerPoints.Radius(Radius(..))
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces)
import CornerPoints.Create(Angle(..), flatXSlope, flatYSlope, Origin(..))
import CornerPoints.CornerPoints((|@+++#@|), (|+++|),  CornerPoints(..), (+++))
import CornerPoints.Points(Point(..))
import CornerPoints.CornerPointsWithDegrees(DegreeRange(..), CornerPointsWithDegrees(..), newCornerPointsWithDegreesList )
import CornerPoints.FaceExtraction(extractFrontFace, extractFrontLeftLine, extractFrontRightLine, extractLeftFace,
                                  extractRightFace, extractBackRightLine)
import CornerPoints.FaceConversions(toBackFace, invertFace, toFrontFace, backFaceFromFrontFace, getBackFaceFromRightFace, getBackFaceFromRightFaceFromCube)


import Stl.StlCornerPointsWithDegrees(FacesWithRange(..), FacesWithRange(..), (|@~?+++^|) )
import Stl.StlCornerPoints(Faces(..), (+++^))
import Stl.StlFileWriter(writeStlToFile)
import Stl.StlBase (StlShape(..), newStlShape)

import qualified Builder.Sequence as S (newCornerPointsWithDegreesBuilder, (||@~+++^||), (@~+++#@|>), (@~+++@|>))
import qualified Builder.List as L ((||@~+++^||))

import Test.HUnit

import qualified Data.Foldable as F
import qualified Data.Sequence as S

--imports for arrows
import Control.Arrow hiding ((+++))
import Control.Category
import Prelude hiding ((.), id)
import qualified Data.Map as M
import Control.Lens
{-------------------------------------- overview ---------------------------------------
Create a simple radial shape with 40-45-90...360 angles.
Create at origin.

Create as second smaller simple radial shape that is offeset from the origin,
such that it will be located withing 1 triangle of large shape.

Subtract the small shape from the triangle of large shape:

so far:
Have removed the faces of large triangle
Have moved small triangle over to large triangle space, and displayed it as a self contained shape.

next: ??????
-}




angles = map Angle [0,45..360]
largeRadius = [(Radius x) | x <- [20,20..]]

largeShape =
  createBottomFaces (Point 0 0 0) largeRadius angles flatXSlope flatYSlope
  |+++|
  createTopFaces (Point 0 0 10) largeRadius angles flatXSlope flatYSlope
  
largeShapeBuilder = S.newCornerPointsWithDegreesBuilder 45 largeShape

largeTriangles =
  largeShapeBuilder
  S.||@~+++^||
  [[(FacesWithRange FacesNada (DegreeRange 0 45)), (FacesWithRange FacesBottomFrontTop (DegreeRange 45 360))]]
  

writeLargeShape =  writeStlToFile $ newStlShape "largeShape" largeTriangles

{-Create a smaller radial shape with same angles.
Change the origin so that it is moved inside of a single cube of large shape.-}
smallRadius = [(Radius x) | x <- [2,2..]]

smallShape =
  createBottomFaces (Point (5) (-10) 0) smallRadius angles flatXSlope flatYSlope
  |+++|
  createTopFaces (Point (5) (-10) 10) smallRadius angles flatXSlope flatYSlope

smallShapeBuilder = S.newCornerPointsWithDegreesBuilder 45 smallShape

smallShapeWithDegrees = newCornerPointsWithDegreesList 45 smallShape

smallShapeMap = M.fromList $ cornerPointsMap smallShapeWithDegrees

smallTriangles =
  smallShapeBuilder
  S.||@~+++^||
  [[(FacesWithRange FacesAll (DegreeRange 0 360))]]
  

writeSmallShape =  writeStlToFile $ newStlShape "smallShape" smallTriangles
{--}

writeBothShapes = writeStlToFile $ newStlShape "smallShape" $ smallTriangles ++ largeTriangles

tests = do
  let seeAngles = TestCase $ assertEqual
        "seeAngles"
        ([Angle 0])
        (angles)
  runTestTT seeAngles

-- =================================================== arrow:: build a single layer ==================================
--create the structure from [CornerPointsWithDegrees] required to make a map
cornerPointsMap = map (\(CubesWithStartEndDegrees cube degreeRange) -> (degreeRange,cube))

--make the CornerPointsWithDegreesList: 
largeShapeWithDegrees = newCornerPointsWithDegreesList 45 largeShape
--cornerPointsWithDegreesSeq =  S.newCornerPointsWithDegreesBuilder 45 largeShape

--make a map of the largeShape with DegreeRange as key and CornerPoints as value
largeShapeMap  = M.fromList $ cornerPointsMap  largeShapeWithDegrees

writeLargeShapeWithMap = do
  let piePiece =  largeShapeMap^.at (DegreeRange 0 45)
      extractFromMap (Just cpt) = cpt
      piePieceTriangle = FacesAll +++^ (extractFromMap piePiece)
  writeStlToFile $ newStlShape "largeShape" piePieceTriangle

{-Extract the CornerPoint from the map Maybe result. Gives CornerPointsError on Nothing -}
maybeMapResult :: Maybe CornerPoints ->  CornerPoints
maybeMapResult (Just a) = a
maybeMapResult Nothing = CornerPointsError "error" -- F1 (Point 1 2 3)
{-
Using arrows and map and lense:
Create 2 pieces of the large shape and output then w/o errors.
The Faces are added as it is processed.

Note the use of <<< makes it backwards
-}
writeLargeShapeWithMapAndArrowsReversed = do
  let 
      f =  Kleisli  (\triangles -> writeStlToFile $ newStlShape "largeShape" triangles)
           --create faces for 135-360
           <<< arr (\triangles ->
                    
                    ( [largeShapeWithDegrees]
                      L.||@~+++^||
                      [[FacesWithRange FacesBottomFrontTop (DegreeRange 135 360)]]
                    )
                    ++ triangles
                   )
           --right of gap triangle
           <<< arr (\triangles -> (FacesBottomFrontRightTop +++^ (maybeMapResult (largeShapeMap^.at  (DegreeRange 90 135))))  ++ triangles)
           --left of gap cube
           <<< arr (\(Just cube) -> FacesBottomFrontLeftTop +++^ cube)
           <<< arr Just 
  runKleisli f (maybeMapResult $ largeShapeMap^.at  (DegreeRange 0 45)) 

{-same as writeLargeShapeWithMapAndArrowsReverse except with >>>
The first call of arr Just: gets its params from the last line, as params to f.
Maybe get around this by passing in () to a lambda, and ignore it. -}
writeLargeShapeWithMapAndArrowsForward = do
  let f = arr (\_ -> Just (maybeMapResult $ largeShapeMap^.at  (DegreeRange 0 45))) 
          >>> arr (\(Just cube) -> S.fromList (FacesBottomFrontLeftTop +++^ cube))
          >>> arr (\triangles -> (S.fromList(FacesBottomFrontRightTop +++^
                                 (maybeMapResult (largeShapeMap^.at  (DegreeRange 90 135))))) S.>< triangles)
          >>> arr (\triangles ->
                   (S.fromList
                    ( [largeShapeWithDegrees]
                      L.||@~+++^|| 
                      [[FacesWithRange FacesBottomFrontTop (DegreeRange 135 360)]]
                    )
                   )
                    S.>< triangles
                   )
          >>> Kleisli (\triangles -> writeStlToFile $ newStlShape "largeShape" (F.toList triangles))
  runKleisli f () 


{-The only way to have it compile is to have the f 4. If 4 or Just is moved, no compile-}
testMaybe = do
  let f = Kleisli print <<<  arr Just
  runKleisli f 4

testMaybeForwards = do
  let f = arr Just >>> Kleisli print
  runKleisli f 4

{-
Strange: This compiled until I built buildHoleInSingleTriangle, then it fails with:
No instance for (Num a0) arising from the literal ‘4’
    The type variable ‘a0’ is ambiguous
It's like the arrows are somehow attached.

--First arr needs a param supplied, so pass int (), but ignore it.
testMaybeForwardsWithLambda = do
  let f = arr (\_ -> Just 4) >>> Kleisli print
  runKleisli f ()
-}
-- ======================================== lens: extract a face =====================================
data Extractor a = Extract {_getFrontFace :: a -> CornerPoints,
                            _getFrontFaceAsBackFace :: a -> CornerPoints,
                            _getFrontLeftLineAsBackFace :: a -> CornerPoints,
                            _getLeftFaceAsBackFace :: a -> CornerPoints,
                            _getFrontRightLineAsBackFace :: a -> CornerPoints,
                            _getRightFaceAsBackFace :: a -> CornerPoints,
                            _getBackRightLineAsBackFace :: a -> CornerPoints}
makeLenses ''Extractor

extractor :: Extractor CornerPoints
extractor = Extract extractFrontFace {-getFrontFace-}
            (toBackFace . extractFrontFace) {-getFrontFaceAsBackFace-}
            (toBackFace . extractFrontLeftLine){-getFrontLeftLineAsBackFace-}
            (toBackFace . extractLeftFace){-getLeftFaceAsBackFace-}
            (toBackFace . extractFrontRightLine){-getFrontRightLineAsBackFace-}
            (toBackFace . extractRightFace){-getRightFaceAsBackFace-}
            (toBackFace . extractBackRightLine)--getBackRighLineAsBackFace

            
getCpt (Just cpt) = cpt
getCpt Nothing = CornerPointsError "error"

rightOfGapCpnt = getCpt $ largeShapeMap^.at  (DegreeRange 0 45)

--the '$' is req'd to make it compile. Not sure why.
myExtract =  extractor^.getFrontLeftLineAsBackFace $ rightOfGapCpnt 


{- ========================= the big one ============================
Build the large shape with the small shape removed from the 0-45 degree cube.
-}
buildHoleInSingleTriangle = do
  let frontRightLineOfLargeRightFaceOf45To90ToBackFace =
          (\triangles -> (triangles,
                          extractor^.getFrontRightLineAsBackFace $ (maybeMapResult $ largeShapeMap^.at  (DegreeRange 45 90))
                         )
          )
        
      backRightLineOfLargeRightFaceOf45To90ToBackFace =
         (\triangles -> (triangles,
                          extractor^.getBackRightLineAsBackFace $ (maybeMapResult $ largeShapeMap^.at  (DegreeRange 45 90))
                         )
          )

      frontLeftLineOfLargeLeftFaceOf315To360ToBackFace =
          (\triangles -> (triangles,
                          extractor^.getFrontLeftLineAsBackFace $ (maybeMapResult $ largeShapeMap^.at  (DegreeRange 315 360))
                         )
          )

      
      rightFaceOfLargeRightFaceOf45To90ToBackFace =
       let
          getTriangle = maybeMapResult $ largeShapeMap^.at  (DegreeRange 45 90)
       in
          (\triangles -> (triangles,
                          (extractor^.getRightFaceAsBackFace $ getTriangle
                          )
                         )
          )


      leftFaceOfLargeRightFaceOf315To360ToBackFace =
       let
          getTriangle = maybeMapResult $ largeShapeMap^.at  (DegreeRange 315 360)
       in
          (\triangles -> (triangles,
                          (invertFace $ extractor^.getLeftFaceAsBackFace $ getTriangle
                          )
                         )
          )
         
      
      invertedFrontFaceOfSmall degreeStart degreeEnd faces =
        (\(triangles, largeCube) ->
              triangles
              S.><
              (S.fromList  (faces +++^
                            (largeCube +++ (invertFace $ extractFrontFace $ maybeMapResult $ smallShapeMap^.at  (DegreeRange degreeStart degreeEnd))))
              )
        )

      
      f = -- =========================== embed hole===========================
          -- ========= 0-45 degrees of small shape===============
          --get front face of large shape for 0'
          arr (\_ -> S.fromList[])
          >>> arr (\triangles ->
                    let largeCube = extractor^.getFrontFaceAsBackFace $ (maybeMapResult $ largeShapeMap^.at  (DegreeRange 0 45))
                        smallCube = invertFace $ extractFrontFace $ maybeMapResult $ smallShapeMap^.at  (DegreeRange 0 45)
                        
                    in   triangles S.>< (S.fromList (FacesBackBottomFrontTop +++^ (largeCube +++ smallCube)))
                  )

          -- ============= 45-90 degrees of small triangle=================
          >>>  arr frontRightLineOfLargeRightFaceOf45To90ToBackFace
          --get the small cube at 45'
          >>> arr (invertedFrontFaceOfSmall 45 90 FacesBackBottomFrontTop)
          
          -- =========================== 90-135 degrees small cube ===========================================
          >>>  arr frontRightLineOfLargeRightFaceOf45To90ToBackFace
          --get the small cube at 45'
          >>> arr (invertedFrontFaceOfSmall 90 135 FacesBackBottomFrontTop)
          
          -- =========================== 135-180 degrees small cube ==================================
          
          >>> arr rightFaceOfLargeRightFaceOf45To90ToBackFace
          >>> arr ( (invertedFrontFaceOfSmall 135 180 FacesBottomFrontTop))
          {-
          >>> arr frontRightLineOfLargeRightFaceOf45To90ToBackFace
          >>> arr (invertedFrontFaceOfSmall 135 180 FacesBottomFrontTop)
          -}
          -- ============================ 180-225 degrees small cube==========================================
          
          >>> arr backRightLineOfLargeRightFaceOf45To90ToBackFace
          >>> arr (invertedFrontFaceOfSmall 180 225 FacesBottomFrontTop)
          {-
          >>> arr rightFaceOfLargeRightFaceOf45To90ToBackFace
          >>> arr (invertedFrontFaceOfSmall 180 225 FacesBottomFrontTop)
          -}
          -- ============================ 225-270 degrees small cube==========================================
          >>> arr backRightLineOfLargeRightFaceOf45To90ToBackFace
          >>> arr (invertedFrontFaceOfSmall 225 270 FacesBottomFrontTop)

          -- =========================== 270 315 degrees small cube ==================================
          >>> arr leftFaceOfLargeRightFaceOf315To360ToBackFace
          >>> arr (invertedFrontFaceOfSmall 270 315 FacesBottomFrontTop)

          -- =========================== 315-360 degrees small cube ===========================================
          
          --get right face of large shape for 45-90'
          >>>  arr frontLeftLineOfLargeLeftFaceOf315To360ToBackFace
          --get the small cube at 45'
          >>> arr (invertedFrontFaceOfSmall 315 360 FacesBackBottomFrontTop)
          
          -- =============================== show the large triangle==========================================
          >>> arr (\triangles -> (S.fromList(FacesBottomFrontTop +++^
                                 (maybeMapResult (largeShapeMap^.at  (DegreeRange 45 90))))) S.>< triangles)


          >>> arr (\triangles ->
                   (S.fromList
                    ( [largeShapeWithDegrees]
                      L.||@~+++^|| 
                      [[FacesWithRange FacesBottomFrontTop (DegreeRange 90 315)]]
                    )
                   )
                    S.>< triangles
                   )

          >>> arr (\triangles -> (triangles, Just (maybeMapResult $ largeShapeMap^.at  (DegreeRange 315 360)))) 
          >>> arr (\(triangles, Just cube) -> triangles S.>< (S.fromList (FacesBottomFrontTop +++^ cube)))
          
          
          
          --print them out as stl
          >>> Kleisli (\triangles -> writeStlToFile $ newStlShape "hole in the wall" (F.toList triangles))
          
  runKleisli f ()
  
  
buildHoleInSingleTriangleTest = do
  let largeRightFaceTest = TestCase $ assertEqual
        "largeRightFaceTest"
        (RightFace {b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                    f3 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 10.0},
                    f4 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 0.0}})
        (extractRightFace $ maybeMapResult $ largeShapeMap^.at  (DegreeRange 45 90)
        )
  runTestTT largeRightFaceTest

  let largeRightFaceAsBackFaceTest = TestCase $ assertEqual
        "largeRightFaceAsBackFaceTest"
        (BackFace {b1 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 0.0},
                   b2 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 10.0},
                   b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                   b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}})
        (let largeTriangle = maybeMapResult $ largeShapeMap^.at  (DegreeRange 45 90)
         in  extractor^.getRightFaceAsBackFace $ largeTriangle
        )
  runTestTT largeRightFaceAsBackFaceTest

  let smallCubeFrontFaceTest = TestCase $ assertEqual
       "smallCubeFrontFaceTest"
       (FrontFace {f1 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 0.0},
                   f2 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 10.0},
                   f3 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 10.0},
                   f4 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 0.0}})
       (extractFrontFace $ maybeMapResult $ smallShapeMap^.at  (DegreeRange 135 180))
  runTestTT smallCubeFrontFaceTest

  let smallCubeFrontFaceInvertedTest = TestCase $ assertEqual
       "smallCubeFrontFaceInvertedTest"
       (FrontFace {f1 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 0.0},
                   f2 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 10.0},
                   f3 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 10.0},
                   f4 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 0.0}})
       (invertFace $ extractFrontFace $ maybeMapResult $ smallShapeMap^.at  (DegreeRange 135 180))
  runTestTT smallCubeFrontFaceInvertedTest


  let addTheFacesTest = TestCase $ assertEqual
       "addTheFacesTest"
       (CubePoints {f1 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 0.0},
                    f2 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 10.0},
                    f3 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 10.0},
                    f4 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 0.0},
                    b1 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 0.0},
                    b2 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 10.0},
                    b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                    b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}})
       (let
           invertedFrontFace = invertFace $ extractFrontFace $ maybeMapResult $ smallShapeMap^.at  (DegreeRange 135 180)
           largeTriangle = maybeMapResult $ largeShapeMap^.at  (DegreeRange 45 90)
           rightFaceAsBackFace =  extractor^.getRightFaceAsBackFace $ largeTriangle
        in  rightFaceAsBackFace +++ invertedFrontFace
       )
  runTestTT addTheFacesTest

writeNFGCubeFromTest = do
  writeStlToFile $ newStlShape "hole in the wall"
    (FaceBottom +++^

    (CubePoints {f1 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 0.0},
                    f2 = Point {x_axis = 6.414213562373095, y_axis = -8.585786437626904, z_axis = 10.0},
                    f3 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 10.0},
                    f4 = Point {x_axis = 5.0, y_axis = -8.0, z_axis = 0.0},
                    b1 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 0.0},
                    b2 = Point {x_axis = 14.14213562373095, y_axis = -14.142135623730951, z_axis = 10.0},
                    b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                    b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}})


    )
