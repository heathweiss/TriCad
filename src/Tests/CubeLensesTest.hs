{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ParallelListComp #-}
{- | This is the same as CornerPoints.VerticalFaces, except it uses CornerPointsWithDegreeInfo instead of CornerPoints.
     That is to say that all the CornerPoints have Degree information attached to them.
     The purpose is to make it much cleaner to create stl from CornerPoints.-}
module Tests.CubeLensesTest where
import Test.HUnit
import Data.List as List
import Control.Lens
import Data.Map as Map
import CornerPoints.CornerPoints(CornerPoints(..),(@+++#@),(+++), (+++>))
import CornerPoints.Points (Point(..))
import CornerPoints.Transpose (transposeZ)
import CornerPoints.FaceConversions(backFaceFromFrontFace, upperFaceFromLowerFace, lowerFaceFromUpperFace )
import CornerPoints.FaceExtraction (extractFrontFace)
import CornerPoints.VerticalFaces(
  createRightFaces, createRightFacesNoSlope, createVerticalFaces,
  createLeftFaces, createLeftFacesNoSlope,
  createHorizontallyAlignedCubes, createHorizontallyAlignedCubesNoSlope,
  createLeftFacesMultiColumns, createLeftFacesMultiColumnsNoSlope, createVerticalWalls,
  TransposeFactor(..))
import CornerPoints.Radius(SingleDegreeRadii(..), Radius(..), MultiDegreeRadii(..))
import CornerPoints.Create(flatXSlope, flatYSlope, Slope(..), Origin(..))



data RadialRange = RadialRange {_startDegree::Double, _endDegree::Double}
 deriving (Show, Eq)
makeLenses ''RadialRange

{- See if the start and end Degrees of the RadialRange are within the given start/end range. -}
radialSliceWithinDegreeRange :: Double -> Double -> RadialRange -> Bool
radialSliceWithinDegreeRange    start     end (RadialRange startDegree' endDegree') = 
               startDegree' >= start && endDegree' <= end

{- See if the start and end Degrees of the CubesWithStartEndDegreeInfo are within the given start/end range. -}
areCubesWithStartEndDegreeInfoWithinDegreeRange :: Double -> Double -> CornerPointsWithDegreeInfo -> Bool
areCubesWithStartEndDegreeInfoWithinDegreeRange    start     end (CubesWithStartEndDegreeInfo _ startDegree' endDegree') = 
               startDegree' >= start && endDegree' <= end

data CornerPointsWithDegreeInfo =
    CubesWithStartEndDegreeInfo {_cube::CornerPoints, _startDeg::Double, _endDeg::Double}
  | FrontFaceWithStartEndDegreeInfo {_frontFace::CornerPoints, _startDeg::Double, _endDeg::Double}
  | BackFaceWithStartEndDegreeInfo {_backFace::CornerPoints, _startDeg::Double, _endDeg::Double}
  | LeftFaceWithDegreeInfo {_leftFace::CornerPoints, _degree::Double}
  | RightFaceWithDegreeInfo {_rightFace::CornerPoints, _degree::Double}
  deriving(Show, Eq)

testCube = (BottomFace
              {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0},
               b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}}
           )
           @+++#@
           ((transposeZ(+1)) . upperFaceFromLowerFace)

testRightFace = 
           (RightFace (Point 1 0 1) (Point 1 0 0) (Point 1 1 1) (Point 1 1 0))

testLeftFace = 
           (LeftFace  (Point 0 0 0)  (Point 0 0 1)  (Point 0 1 0) (Point 0 1 1) )

(+++~) :: CornerPointsWithDegreeInfo ->                CornerPointsWithDegreeInfo               -> CornerPointsWithDegreeInfo
(RightFaceWithDegreeInfo faceRight rightDeg) +++~ (LeftFaceWithDegreeInfo faceLeft leftDeg) =
                            CubesWithStartEndDegreeInfo (faceRight +++ faceLeft ) rightDeg leftDeg
(CubesWithStartEndDegreeInfo cube startDegree' endDegree') +++~ (LeftFaceWithDegreeInfo faceLeft leftDeg) =
                            CubesWithStartEndDegreeInfo (cube +++ faceLeft) endDegree' leftDeg

{-Same as +++> but with degree info attached.-}
(+++~>) :: CornerPointsWithDegreeInfo -> [CornerPointsWithDegreeInfo] -> [CornerPointsWithDegreeInfo]
x+++~> xs = tail $ List.scanl (+++~) x xs

{- |Add [CornerPointsWithDegreeInfo] to [CornerPointsWithDegreeInfo].
-}
(|+++~|) :: [CornerPointsWithDegreeInfo] -> [CornerPointsWithDegreeInfo] -> [CornerPointsWithDegreeInfo]
c1 |+++~| c2 = zipWith (+++~) c1 c2

type Degree = Double



createLeftFacesWithDegreeInfo :: Degree -> Origin -> SingleDegreeRadii -> Slope -> Slope -> [TransposeFactor] -> [CornerPointsWithDegreeInfo]
createLeftFacesWithDegreeInfo degreeInfo origin singleDegreeRadii xSlope ySlope zTransposeFactor =
  let leftFaces = createLeftFaces origin singleDegreeRadii xSlope ySlope zTransposeFactor
  in [LeftFaceWithDegreeInfo face degreeInfo | face <- leftFaces]


createLeftFacesWithDegreeInfoNoSlope :: Origin -> SingleDegreeRadii -> [TransposeFactor] -> [CornerPointsWithDegreeInfo]
createLeftFacesWithDegreeInfoNoSlope origin singleDegreeRadii zTransposeFactor  =
  createLeftVerticalFacesWithDegreeInfo origin singleDegreeRadii flatXSlope flatYSlope zTransposeFactor (F2) (B2) (F1) (B1)

createRightFacesWithDegreeInfo :: Degree -> Origin -> SingleDegreeRadii -> Slope -> Slope -> [TransposeFactor] -> [CornerPointsWithDegreeInfo]
createRightFacesWithDegreeInfo degreeInfo origin singleDegreeRadii xSlope ySlope zTransposeFactor =
  let rightFaces = createRightFaces origin singleDegreeRadii xSlope ySlope zTransposeFactor
  in [RightFaceWithDegreeInfo face degreeInfo | face <- rightFaces]

createRightFacesWithDegreeInfoNoSlope :: Origin -> SingleDegreeRadii -> [TransposeFactor] -> [CornerPointsWithDegreeInfo]
createRightFacesWithDegreeInfoNoSlope origin singleDegreeRadii zTransposeFactor  =
  createRightVerticalFacesWithDegreeInfo origin singleDegreeRadii flatXSlope flatYSlope zTransposeFactor (F3) (B3) (F4) (B4)

{-Same as createHorizontallyAlignedCubes, but with degree info attached. -}
createHorizontallyAlignedCubesWithDegreeInfo :: [CornerPointsWithDegreeInfo] -> [[CornerPointsWithDegreeInfo]] -> [[CornerPointsWithDegreeInfo]]
createHorizontallyAlignedCubesWithDegreeInfo ([]) _ = []
createHorizontallyAlignedCubesWithDegreeInfo (x:xs) (ys) =
  let headOfLeftFaces = List.map (head) ys
  in (x +++~> headOfLeftFaces) : (createHorizontallyAlignedCubesWithDegreeInfo xs (List.map (tail) ys) )

createHorizontallyAlignedCubesWithDegreeInfoNoSlope :: Origin -> MultiDegreeRadii -> [TransposeFactor] -> [[CornerPointsWithDegreeInfo]]
createHorizontallyAlignedCubesWithDegreeInfoNoSlope origin (MultiDegreeRadii name' degrees') transposeFactors =
  let leftFaces = createLeftFacesMultiColumnsWithDegreeInfoNoSlope origin (tail degrees') transposeFactors
      rightFaces = createRightFacesWithDegreeInfoNoSlope origin (head degrees' )  transposeFactors
  in  createHorizontallyAlignedCubesWithDegreeInfo rightFaces leftFaces



createLeftFacesMultiColumnsWithDegreeInfo ::  Origin -> [SingleDegreeRadii] -> Slope -> Slope -> [TransposeFactor] -> [[CornerPointsWithDegreeInfo]]
createLeftFacesMultiColumnsWithDegreeInfo _ [] _ _ _ = []
createLeftFacesMultiColumnsWithDegreeInfo topOrigin (d:ds) xSlope ySlope zTransposeFactor =
  (createLeftFacesWithDegreeInfo (degree d) topOrigin d xSlope ySlope zTransposeFactor ) :
    (createLeftFacesMultiColumnsWithDegreeInfo topOrigin ds xSlope ySlope zTransposeFactor)



createLeftFacesMultiColumnsWithDegreeInfoNoSlope ::  Origin -> [SingleDegreeRadii] -> [TransposeFactor] -> [[CornerPointsWithDegreeInfo]]
createLeftFacesMultiColumnsWithDegreeInfoNoSlope _ [] _ = []
createLeftFacesMultiColumnsWithDegreeInfoNoSlope topOrigin (d:ds) zTransposeFactor =
  (createLeftFacesWithDegreeInfoNoSlope topOrigin d zTransposeFactor ) :
    (createLeftFacesMultiColumnsWithDegreeInfoNoSlope topOrigin ds zTransposeFactor)


{-LeftFaceWithDegreeInfo version of createVerticalFaces-}
createLeftVerticalFacesWithDegreeInfo :: Origin -> SingleDegreeRadii -> Slope -> Slope -> [TransposeFactor] -> (Point-> CornerPoints) ->
                       (Point-> CornerPoints) -> (Point-> CornerPoints) -> (Point-> CornerPoints) -> [CornerPointsWithDegreeInfo]
createLeftVerticalFacesWithDegreeInfo origin (SingleDegreeRadii degree' radii') xSlope ySlope zTransposeFactor topFrontConstructor topBackConstructor
                            btmFrontConstructor btmBackConstructor =
  
  let verticalFaces = createVerticalFaces origin (SingleDegreeRadii degree' radii') xSlope ySlope zTransposeFactor topFrontConstructor topBackConstructor
                            btmFrontConstructor btmBackConstructor
  in  [LeftFaceWithDegreeInfo  x degree' | x <- verticalFaces ]

{-RightFaceWithDegreeInfo version of createVerticalFaces-}
createRightVerticalFacesWithDegreeInfo :: Origin -> SingleDegreeRadii -> Slope -> Slope -> [TransposeFactor] -> (Point-> CornerPoints) ->
                       (Point-> CornerPoints) -> (Point-> CornerPoints) -> (Point-> CornerPoints) -> [CornerPointsWithDegreeInfo]
createRightVerticalFacesWithDegreeInfo origin (SingleDegreeRadii degree' radii') xSlope ySlope zTransposeFactor topFrontConstructor topBackConstructor
                            btmFrontConstructor btmBackConstructor =
  
  let verticalFaces = createVerticalFaces origin (SingleDegreeRadii degree' radii') xSlope ySlope zTransposeFactor topFrontConstructor topBackConstructor
                            btmFrontConstructor btmBackConstructor
  in  [RightFaceWithDegreeInfo  x degree' | x <- verticalFaces ]

--CubesWithStartEndDegreeInfo {_cube::CornerPoints, _startDeg::Double, _endDeg::Double}
--FrontFaceWithStartEndDegreeInfo {_frontFace::CornerPoints, _startDeg::Double, _endDeg::Double}
extractFrontFaceWithDegreeInfo :: CornerPointsWithDegreeInfo -> CornerPointsWithDegreeInfo
extractFrontFaceWithDegreeInfo (CubesWithStartEndDegreeInfo cube' startDegree' endDegree') =
   FrontFaceWithStartEndDegreeInfo  (extractFrontFace cube') startDegree' endDegree'

backFaceFromFrontFaceWithDegreeInfo :: CornerPointsWithDegreeInfo -> CornerPointsWithDegreeInfo
backFaceFromFrontFaceWithDegreeInfo (FrontFaceWithStartEndDegreeInfo cube' startDegree' endDegree') =
  BackFaceWithStartEndDegreeInfo (backFaceFromFrontFace cube')  startDegree' endDegree'


{- |Create a vertical shape with walls based on an inner and outer MultiDegreeRadii-}
createVerticalWallsWithDegreeInfo ::  MultiDegreeRadii ->   MultiDegreeRadii ->      Origin -> [TransposeFactor] -> [[CornerPointsWithDegreeInfo]]
createVerticalWallsWithDegreeInfo     multiDegreeRadiiInner    multiDegreeRadiiOuter origin    transposeFactors =
       [
        currBackFace |+++~| currFrontFace
        | currFrontFace <- [List.map (extractFrontFaceWithDegreeInfo) currRow  | currRow  <- createHorizontallyAlignedCubesWithDegreeInfoNoSlope origin multiDegreeRadiiOuter transposeFactors] 
        | currBackFace <- [ List.map (backFaceFromFrontFaceWithDegreeInfo . extractFrontFaceWithDegreeInfo) currRow | currRow  <- (createHorizontallyAlignedCubesWithDegreeInfoNoSlope origin multiDegreeRadiiInner transposeFactors ) ]
       ]


cubeLensesTestDo = do

  {-Push a RightFaceWithDegreeInfo into a [LeftFaceWithDegreeInfo]-}
  let pushRightFaceWithDegreeInfoIntoAListOfLeftFaceWithDegreeInfo = TestCase $ assertEqual
        "pushRightFaceWithDegreeInfoIntoAListOfLeftFaceWithDegreeInfo"
        [CubesWithStartEndDegreeInfo
           {_cube = CubePoints {f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0},
                       f2 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0},
                       f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0},
                       f4 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}, 
                       b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                       b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                       b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                       b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}},
              _startDeg = 0.0, _endDeg = 10.0},
         CubesWithStartEndDegreeInfo 
           {_cube = CubePoints {f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 8.0},
                       f2 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0},
                       f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0},
                       f4 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0},
                       b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
                       b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                       b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0},
                       b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}},
              _startDeg = 10.0, _endDeg = 20.0}
        ]
        (let listOfLeftFacesWithDegreeInfo =
                [LeftFaceWithDegreeInfo
                  {_leftFace = LeftFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                                         f2 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0}, f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}},
                  _degree = 10.0},
                 LeftFaceWithDegreeInfo
                  {_leftFace = LeftFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
                              f2 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}, f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 8.0}},
                   _degree = 20.0}
                ]
  
             rightFaceWithDegreeInfo =
                RightFaceWithDegreeInfo
                  {_rightFace = RightFace {b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                                           f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0}, f4 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}},
                   _degree = 0.0}
  
         in rightFaceWithDegreeInfo +++~> listOfLeftFacesWithDegreeInfo
           
        )
  runTestTT pushRightFaceWithDegreeInfoIntoAListOfLeftFaceWithDegreeInfo
  
  {-Create a column of left faces that have degree info attached.-}
  let createLeftFacesColumnWithDegreeInfo = TestCase $ assertEqual
        "createLeftFacesColumnWithDegreeInfo"
        ( [LeftFaceWithDegreeInfo
           {_leftFace = LeftFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                              f2 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0}, f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}},
            _degree = 0.0},
           LeftFaceWithDegreeInfo
            {_leftFace = LeftFace {b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
                              f2 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}, f1 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 8.0}},
            _degree = 0.0}
          ]

        )
        ( let sdr = (SingleDegreeRadii 0 (List.map (Radius) [1,2,2]))
          in  createLeftFacesWithDegreeInfo (0::Degree) (Point 0 0 10) sdr flatXSlope flatYSlope [1,2..]
        )
  runTestTT createLeftFacesColumnWithDegreeInfo
  
  {-Create a column of right faces that have degree info attached.-}
  let createRightFacesColumnWithDegreeInfo = TestCase $ assertEqual
        "createRightFacesColumnWithDegreeInfo"
        ( [RightFaceWithDegreeInfo
           {_rightFace = RightFace {b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 10.0}, b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0},
                              f3 = Point {x_axis = 0.0, y_axis = -1.0, z_axis = 10.0}, f4 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}},
            _degree = 0.0},
           RightFaceWithDegreeInfo
            {_rightFace = RightFace {b3 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 9.0}, b4 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 8.0},
                              f3 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 9.0}, f4 = Point {x_axis = 0.0, y_axis = -2.0, z_axis = 8.0}},
            _degree = 0.0}
          ]

        )
        ( let sdr = (SingleDegreeRadii 0 (List.map (Radius) [1,2,2]))
          in  createRightFacesWithDegreeInfo (0::Degree) (Point 0 0 10) sdr flatXSlope flatYSlope [1,2..]
        )
  runTestTT createRightFacesColumnWithDegreeInfo
  
    
  {-Create a [CubesWithStartEndDegreeInfo] from FaceWithDegreeInfo, then filter them-}
  let createCubesWithStartEndDegreeInfoFromFaceWithDegreeInfo = TestCase $ assertEqual
        "createCubesWithStartEndDegreeInfoFromFaceWithDegreeInfo"
        [
           CubesWithStartEndDegreeInfo testCube 90 180 ,
           CubesWithStartEndDegreeInfo testCube 180 270
        ]
        (List.filter (areCubesWithStartEndDegreeInfoWithinDegreeRange 90 270)
         [
           (RightFaceWithDegreeInfo testRightFace 0) +++~ (LeftFaceWithDegreeInfo testLeftFace 90),
           (RightFaceWithDegreeInfo testRightFace 90) +++~ (LeftFaceWithDegreeInfo testLeftFace 180),
           (RightFaceWithDegreeInfo testRightFace 180) +++~ (LeftFaceWithDegreeInfo testLeftFace 270),
           (RightFaceWithDegreeInfo testRightFace 270) +++~ (LeftFaceWithDegreeInfo testLeftFace 360)
         ]
        )
  runTestTT createCubesWithStartEndDegreeInfoFromFaceWithDegreeInfo
  

  let addLeftRigtFacesWithDegreeInfoTest = TestCase $ assertEqual
        "addLeftRigtFacesWithDegreeInfoTest"
        (CubesWithStartEndDegreeInfo (testRightFace +++ testLeftFace) 0 90)
        (
           (RightFaceWithDegreeInfo testRightFace 0) +++~ (LeftFaceWithDegreeInfo testLeftFace 90)
        )
  runTestTT addLeftRigtFacesWithDegreeInfoTest

  
  {-
  Create a map of CubesWithStartEndDegreeInfo.
  See if it is in range.
  Test a 
  -}
  let  
       cubesWithStartEndDegreeInfoTest = TestCase $ assertEqual
        "CubesWithStartEndDegreeInfo"
        [
           CubesWithStartEndDegreeInfo testCube 90 180 ,
           CubesWithStartEndDegreeInfo testCube 180 270
        ]
        (List.filter (areCubesWithStartEndDegreeInfoWithinDegreeRange 90 270)
         [
           CubesWithStartEndDegreeInfo testCube 0 90 ,
           CubesWithStartEndDegreeInfo testCube 90 180 ,
           CubesWithStartEndDegreeInfo testCube 180 270 ,
           CubesWithStartEndDegreeInfo testCube 270 360 
         ]
        )
  runTestTT cubesWithStartEndDegreeInfoTest
  
  let cubeMap = Map.fromList
               [
                  (1, RadialRange  0  45),
                  (2,RadialRange 45  90),
                  (3,RadialRange 90 180)
                ]
      cubeMapTest = TestCase $ assertEqual
        "cubeMapTest"
        (
         (RadialRange  0  45)
         
        )
        ( 
         case cubeMap^.at 1  of
           Nothing -> (RadialRange  0  0)
           Just (RadialRange  startDegree'  endDegree' ) -> (RadialRange  startDegree'  endDegree' )

        ) 
  runTestTT cubeMapTest

  let 
      accessRadialLenseFieldTest = TestCase $ assertEqual
        "cubeMapTest"
        (
          0
        )
        ( 
          (RadialRange  0  45)^.startDegree       
        ) 
  runTestTT accessRadialLenseFieldTest


  let cubeMap = Map.fromList
               [
                  (1, RadialRange  0  45),
                  (2,RadialRange 45  90),
                  (3,RadialRange 90 180)
                ]
      getDegreeFromCubeMapTest = TestCase $ assertEqual
        "getDegreeFromCubeMapTest"
        (
         0
        )
        ( 
         case cubeMap^.at 1  of
           Nothing -> 500
           Just (RadialRange  startDegree'  endDegree' ) -> (RadialRange  startDegree'  endDegree' )^.startDegree

        ) 
  runTestTT getDegreeFromCubeMapTest
 
  let doesRadialRangeMeetBeginEndConditionsTest = TestCase $ assertEqual
        "doesRadialRangeMeetBeginEndConditionsTest"
        (
         True
        )
        (
          radialSliceWithinDegreeRange 10 45 (RadialRange  20  45)

        )
  
  
  runTestTT doesRadialRangeMeetBeginEndConditionsTest


