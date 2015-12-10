module Examples.ShoeLift.KangarooShoe where
import CornerPoints.Radius(Radius(..), buildSymmetricalRadius)
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces)
import CornerPoints.Points(Point(..))
import CornerPoints.Create(Angle(..), flatXSlope, flatYSlope, Slope(..))
import Test.HUnit
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|),
                                            DegreeRange(..), newCornerPointsWithDegrees, newCornerPointsWithDegreesList)
import Control.Lens
import Stl.StlCornerPoints((|+++^|), (||+++^||))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import CornerPoints.CornerPoints(Faces(..),(|@+++#@|))
import CornerPoints.Transpose(transposeZ)
import CornerPoints.Builder(processCornerPointsWithDegreesAndStl, FacesWithRange(..), (&@~+++@), (&@~+++#@), (|||@~+++^|||) )
import CornerPoints.FaceExtraction(extractTopFace, extractBottomFace)
import Primitives.Cylindrical(cylinderSolidNoSlopeSquaredOff )
import CornerPoints.FaceConversions(upperFaceFromLowerFace )
import CornerPoints.Degree(Degree(..))

--origin = Point 0 0 0
angles = map Angle [0,10..]

-- ===================================================== heel of shoe =====================================================
{--}
heelOfShoeRadius = buildSymmetricalRadius
   [ 35.8,--0
     35.3,--10
     35.0,--20
     35.3,--30
     34.6,--40
     33.7,--50
     33.6,--60
     32.2,--70
     31.4,--80
     31.3,--90
     32.0,--100
     33.4,--110
     35.8,--120
     39.7,--130
     46.3,--140
     41.9,--150
     38.2,--160
     36.0  --170
   ]
   35.1 --180

                 

heelOfShoeTriangles =         (
                               ( [  newCornerPointsWithDegreesList (createTopFaces (Point 0 0 15) heelOfShoeRadius angles flatXSlope (NegYSlope 20))
                                    |@~+++@|
                                    (createBottomFaces (Point 0 0 0) heelOfShoeRadius angles flatXSlope flatYSlope)
                                 ]
                               )
                                &@~+++#@
                                ((transposeZ(+(-5))).extractBottomFace)
                                &@~+++@
                                ((map (extractBottomFace) $ cylinderSolidNoSlopeSquaredOff (Radius 20) (Point 0 0 (-10)) angles 0 4) )
                                &@~+++#@
                                (transposeZ(+(-10)) . extractBottomFace)
                              )
                              |||@~+++^|||
                              {-[CubesWithDegrees] is made from bottom up, so align [FacesWithRange] same way.-}
                              [  [ (FacesWithRange FacesBottomFront (DegreeRange 0 360))],
                                 [ (FacesWithRange FaceFront (DegreeRange 0 360))],
                                 [ (FacesWithRange FaceFront (DegreeRange 0 360))],
                                 [ (FacesWithRange FacesFrontTop    (DegreeRange 0 360))]
                              ]


writeHeelToShoe = writeStlToFile $ newStlShape "kangaroo heel to shoe" heelOfShoeTriangles

-- =================================================== heel tread ===========================================
heelOfTreadRadius =
  buildSymmetricalRadius
   [ 36.9, --0
     36.7, --10
     36.6, --20
     36.1, --30
     35.2, --40
     33.8, --50
     31.5, --60
     29.9, --70
     27.7, --80
     27.5, --90
     27.7, --100
     28.5, --110
     30.7, --120
     34.1, --130
     30.4, --140
     26.4, --150
     23.8, --160
     22.5  --170
   ]
   22.2 --180

heelOfTreadTriangles  =
   
    ([  newCornerPointsWithDegreesList
         ( (createBottomFaces (Point 0 0 0) heelOfTreadRadius angles flatXSlope flatYSlope)  
           |@+++#@|
           ((transposeZ(+5)). upperFaceFromLowerFace)
         )
     ]
     &@~+++@
     ((map (extractTopFace) $ cylinderSolidNoSlopeSquaredOff (Radius 20) (Point 0 (-10) 15) angles 0 4) )
     &@~+++#@
     (transposeZ(+5) . extractTopFace)
    )
    |||@~+++^|||
    [ [ (FacesWithRange FacesFrontTop (DegreeRange 0 360))],
      [ (FacesWithRange FaceFront (DegreeRange 0 360))],
      [ (FacesWithRange FacesBottomFront (DegreeRange 0 360))]
       
    ]        

writeHeelOfTread = writeStlToFile $ newStlShape "kangaroo heel tread" heelOfTreadTriangles

-- =============================================== toe tread ==============================================
{-This is the top to the recycled tread. Will attach directly to shoe. Will need a wrapper around the top
of it, to cover the wire used for attaching.-}

{-The forward half of the toe is symmetrical, and that is all that is needed, so use symmetrical system.-}
toeOfTreadRadius =
  buildSymmetricalRadius
   [37.6, --0
    37.6, --10
    36.3, --20
    34.5, --30
    33.4, --40
    32.2, --50
    31.8, --60
    31.1, --70
    31.3, --80
    32.8, --90
    34.9, --100
    37.8, --110
    42.1, --120
    49.2, --130
    42.3, --140
    37.8, --150
    34.7, --160
    33.1 --170
   ]
   33.1 --180


-- ==================================================== tests ==================================================
kangarooShoeTestDo = do
  {- heelOfShoeCubesWithDegrees was merged into triangles'
  let seeBottomFacesWithDegrees = TestCase $ assertEqual
        "seeBottomFacesWithDegrees"
        []
  --runTestTT seeBottomFacesWithDegrees
  

  
  let seeLenghtOfTopLayerOfCubesList = TestCase $ assertEqual
        "seeLenghtOfTopLayerOfCubesList"
        36
        (length $ head heelOfShoeCubesWithDegrees)
  runTestTT seeLenghtOfTopLayerOfCubesList

  let seeLenghtOfLastLayerOfCubesList = TestCase $ assertEqual
        "seeLenghtOfLastLayerOfCubesList"
        36
        (length $ last heelOfShoeCubesWithDegrees)
  runTestTT seeLenghtOfLastLayerOfCubesList
  -}
  
  
  
  let seeRadiusLength = TestCase $ assertEqual
        "seeRadiusLength"
        37 --not 36 because starts at 0 instead of 1
        (length heelOfShoeRadius)
  runTestTT seeRadiusLength
   
  let seeRadius = TestCase $ assertEqual
        "seeRadius"
        [Radius {radius = 35.8},Radius {radius = 35.3},Radius {radius = 35.0},Radius {radius = 35.3},Radius {radius = 34.6},Radius {radius = 33.7},Radius {radius = 33.6},Radius {radius = 32.2},Radius {radius = 31.4},Radius {radius = 31.3},Radius {radius = 32.0},Radius {radius = 33.4},Radius {radius = 35.8},Radius {radius = 39.7},Radius {radius = 46.3},Radius {radius = 41.9},Radius {radius = 38.2},Radius {radius = 36.0},Radius {radius = 35.1},Radius {radius = 36.0},Radius {radius = 38.2},Radius {radius = 41.9},Radius {radius = 46.3},Radius {radius = 39.7},Radius {radius = 35.8},Radius {radius = 33.4},Radius {radius = 32.0},Radius {radius = 31.3},Radius {radius = 31.4},Radius {radius = 32.2},Radius {radius = 33.6},Radius {radius = 33.7},Radius {radius = 34.6},Radius {radius = 35.3},Radius {radius = 35.0},Radius {radius = 35.3},Radius {radius = 35.8}]
        (heelOfShoeRadius)
  runTestTT seeRadius































































































































































































     


  
