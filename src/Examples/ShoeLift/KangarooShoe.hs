module Examples.ShoeLift.KangarooShoe where
import CornerPoints.Radius(Radius(..), buildSymmetricalRadius)
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces)
import CornerPoints.Points(Point(..))
import CornerPoints.Create(Angle(..), flatXSlope, flatYSlope, Slope(..))
import Test.HUnit
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|),
                                            DegreeRange(..))
import Control.Lens
import Stl.StlCornerPoints((|+++^|), (||+++^||), Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import CornerPoints.CornerPoints((|@+++#@|), (|+++|))
import CornerPoints.Transpose(transposeZ)
import Builder.Builder(processCornerPointsWithDegreesAndStl, FacesWithRange(..), (&@~+++@), (&@~+++#@), (|||@~+++^|||),
                           newCornerPointsWith10DegreesBuilder)
import CornerPoints.FaceExtraction(extractTopFace, extractBottomFace)
import Primitives.Cylindrical(cylinderSolidNoSlopeSquaredOff, cylinderWallsNoSlope, cylinderWallsVariableRadiusNoSlope)
import CornerPoints.FaceConversions(upperFaceFromLowerFace )
import CornerPoints.Degree(Degree(..))
import CornerPoints.Transposable(transpose)

--make signatures more readable
type Thickness = Double
type Height    = Double

--origin = Point 0 0 0
angles = map Angle [0,10..]

-- =============================================== toe tread ==============================================


{-This is the top to the recycled tread. Will attach to shoe via an adaptor. Will need a wrapper around the top
of it, to cover the wire used for attaching.-}

{-This is the top of an existing tread from a previous shoe.-}
toeOfTreadRadius =
  map Radius
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
    33.1, --170
    33.0, --180
    33.5, --190
    35.6, --200
    38.9, --210
    44.6, --220
    54.5, --230
    45.7, --240
    39.9, --250
    36.0, --260
    33.5, --270
    32.4, --280
    31.1, --290
    31.0, --300
    31.8, --310
    33.5, --320
    34.7, --330
    35.8, --340
    37.6, --350
    37.6 --360
   ]
   
toeOfShoeRadius = map Radius
  [ 36.4,--0
    36.4 ,--10
    36.1 ,--20
    35.5,--30
    34.1,--40
    34.1,--50
    34.8,--60
    35.8,--70
    36.7,--80
    38.6,--90
    41.1,--100
    44.4,--110
    49.3,--120
    56.0,--130
    45.8,--140
    39.8,--150
    36.4,--160
    34.4,--170
    33.5,--180
    34.0,--190
    35.6,--200
    38.6,--210
    43.9,--220
    52.8,--230
    47.8,--240
    44.1,--250
    40.6,--260
    38.8,--270
    38.2,--280
    37.1,--290
    36.8,--300
    36.8,--310
    36.5,--320
    36.5,--330
    36.5,--340
    36.5,--350
    36.4--360
  ]

toeOfShoeTriangles =
 ((newCornerPointsWith10DegreesBuilder $ --riser meets original lift from former shoe, 5 mm high
   (createTopFaces (Point 0 0 5) toeOfTreadRadius angles flatXSlope flatYSlope)
   |+++|
   (createBottomFaces (Point 0 0 0) toeOfTreadRadius angles flatXSlope flatYSlope)
  )
  &@~+++@
  (createTopFaces (Point 0 0 10) toeOfShoeRadius angles flatXSlope flatYSlope) --adapt to the tread of shoe, 5 mm high
  &@~+++#@
  ((transposeZ (+5))  . extractTopFace) --tread of the shoe
 )
 |||@~+++^|||
 [ [(FacesWithRange FacesFrontTop (DegreeRange 0 360))],   --riser meets shoe
   [(FacesWithRange FaceFront (DegreeRange 0 360))],       --riser to shoe converstion
   [(FacesWithRange FacesBottomFront (DegreeRange 0 360))] --original lift
 ]

writeToe =  writeStlToFile $ newStlShape "kangaroo toe" toeOfShoeTriangles

-- ======================================== toe wire cover ===============================
{-This will a thin wall to wrap around the bottom of the shoe tread and top of the riser, to cover the wire,
and give extra vertical glue area.
-}

{-The front of the shoe slopes forward, so need a top outward slope for the front of the cover, for section on shoe.
The sides do not need to slope outwards.-}
toeWireCoverShoeRadius = zipWith (transpose)
                          (concat
                            [[(+2)| x <- [1..4]],
                             [(+0)| x <- [5..32]],
                             [(+2)| x <- [1..]]
                            ]
                          )
                          toeOfShoeRadius

toeWireCoverTriangles =
 ((newCornerPointsWith10DegreesBuilder $
   cylinderWallsVariableRadiusNoSlope toeOfShoeRadius (1.0::Thickness)  (Point 0 0 0) angles (5::Height)
  )
  &@~+++@
  (map (extractTopFace) (cylinderWallsVariableRadiusNoSlope toeWireCoverShoeRadius (1.0::Thickness)  (Point 0 0 5) angles (13::Height)))
 )
 |||@~+++^|||
 [ [(FacesWithRange FacesBackFrontTop (DegreeRange 0 80)),       --
    (FacesWithRange FacesBackFrontLeftTop (DegreeRange 80 90)),     --
    (FacesWithRange FacesBackFrontRightTop (DegreeRange 270 280)),  --
    (FacesWithRange FacesBackFrontTop (DegreeRange 280 360))
   ],
   [(FacesWithRange FacesBackBottomFront (DegreeRange 0 80)),       --
    (FacesWithRange FacesBackBottomFrontLeft (DegreeRange 80 90)),     --
    (FacesWithRange FacesBackBottomFrontRight (DegreeRange 270 280)),  --
    (FacesWithRange FacesBackBottomFront (DegreeRange 280 360))
   ]     -- 
 ]

writeToeWireCover = writeStlToFile $ newStlShape "kangaroo toe wire cover" toeWireCoverTriangles

-- ===================================================== heel of shoe =====================================================
{-The top is the shape of the heel of the shoe.
The bottom is a standard riser shape of squaredOff Radius 20 ^4-}
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
                               ( newCornerPointsWith10DegreesBuilder $
                                    (createTopFaces (Point 0 0 10) heelOfShoeRadius angles flatXSlope (NegYSlope 15))
                                    |+++|
                                    (createBottomFaces (Point 0 0 0) heelOfShoeRadius angles flatXSlope flatYSlope)
                               )
                                &@~+++#@
                                ((transposeZ(+(-5))).extractBottomFace)
                                &@~+++@
                                ((map (extractBottomFace) $ cylinderSolidNoSlopeSquaredOff (Radius 25) (Point 0 0 (-10)) angles 0 4) )
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

{- ====================================== heel of shoe wire cover===========================================
A wrapper to cover the wire where riser meets shoe, and provide extra vertical glue area.
Perhaps this should be combined with the ankle brace.-}


-- =================================================== heel tread ===========================================
{-The top is a standard riser shape of squaredOff Radius 20 ^4.
The bottom is the shape of the top of the recyled heel from his cowboy boot.-}
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
      (
       ( newCornerPointsWith10DegreesBuilder $
          (createBottomFaces (Point 0 0 0) heelOfTreadRadius angles flatXSlope flatYSlope)  
          |@+++#@|
          ((transposeZ(+5)). upperFaceFromLowerFace)
       )
      &@~+++@
     ((map (extractTopFace) $ cylinderSolidNoSlopeSquaredOff (Radius 25) (Point 0 (-7) 10) angles 0 4) )
     &@~+++#@
     (transposeZ(+5) . extractTopFace)
    )
    |||@~+++^|||
    [ [ (FacesWithRange FacesFrontTop (DegreeRange 0 360))],
      [ (FacesWithRange FaceFront (DegreeRange 0 360))],
      [ (FacesWithRange FacesBottomFront (DegreeRange 0 360))]
       
    ]        

writeHeelOfTread = writeStlToFile $ newStlShape "kangaroo heel tread" heelOfTreadTriangles




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
        (toeWireCoverShoeRadius)
  runTestTT seeRadius































































































































































































     


  
