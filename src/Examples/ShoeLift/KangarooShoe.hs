module Examples.ShoeLift.KangarooShoe where
import CornerPoints.Radius(Radius(..))
import CornerPoints.HorizontalFaces(createBottomFaces, createTopFaces)
import CornerPoints.Points(Point(..))
import CornerPoints.Create(Angle(..), flatXSlope, flatYSlope, Slope(..))
import Test.HUnit
import CornerPoints.CornerPointsWithDegrees(CornerPointsWithDegrees(..), (@~+++#@),(@~+++@),(|@~+++@|), (|@~+++#@|),
                                            DegreeRange(..),)
import Control.Lens
import Stl.StlCornerPoints((|+++^|), (||+++^||))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)
import CornerPoints.CornerPoints(Faces(..))
import CornerPoints.Transpose(transposeZ)
import CornerPoints.Builder((&@~#@~), processCornerPointsWithDegreesAndStl, FacesWithRange(..),(&@~+++@), (&@~+++#@) )
import CornerPoints.FaceExtraction(extractTopFace, extractBottomFace)
import Primitives.Cylindrical(cylinderSolidNoSlopeSquaredOff )

--origin = Point 0 0 0
angles = map Angle [0,10..360]
-- ===================================================== heel =====================================================
{-The radius of half of the symetrical heel-}
heelOfShoeHalf =
  
   [ 35.8,--0
     35.3,--10
     35.0,--20
     35.3,--30
     34.6,--40
     33.7,--50
     33.7,--60
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

{-To heelHalf: Add in the 180 degree, plus the second half of the heel-}
heelOfShoeRadius = map Radius $  heelOfShoeHalf ++ ( 35.1 : (reverse heelOfShoeHalf))

degrees = zipWith (,) [0,10..350] [10,20..360]
 
makeTopFacesWithDegrees cube (start,end) = TopFacesWithDegrees cube (DegreeRange start end)
                  
heelOfShoeCubesWithDegrees = [ (zipWith (makeTopFacesWithDegrees) (createTopFaces (Point 0 0 10) heelOfShoeRadius angles flatXSlope (NegYSlope 10))  degrees)
                               |@~+++@|
                               (createBottomFaces (Point 0 0 0) heelOfShoeRadius angles flatXSlope flatYSlope)
                             ]
                             &@~#@~
                             (|@~+++#@| ((transposeZ(+(-10))).extractBottomFace))
                             &@~+++@
                             ((map (extractBottomFace) $ cylinderSolidNoSlopeSquaredOff (Radius 30) (Point 0 0 (-20)) angles 0 4) )
                             &@~+++#@
                             (transposeZ(+(-10)) . extractBottomFace)

{-heelOfShoeCubesWithDegrees is made from bottom up so align FacesWithRange list same way.-}
triangles' = concat $ zipWith (processCornerPointsWithDegreesAndStl)
                              heelOfShoeCubesWithDegrees
                              [  [ (FacesWithRange FacesBottomFront (DegreeRange 0 360))],
                                 [ (FacesWithRange FaceFront (DegreeRange 0 360))],
                                 [ (FacesWithRange FaceFront (DegreeRange 0 360))],
                                 [ (FacesWithRange FacesFrontTop    (DegreeRange 0 360))]
                              ]

  --concat $  processCornerPointsWithDegreesAndStl  (head heelOfShoeCubesWithDegrees) [ (FacesWithRange FacesAll (DegreeRange 0 90)),(FacesWithRange FacesAll (DegreeRange 180 360))]

cubesStl = newStlShape "test" triangles'
  
write = writeStlToFile cubesStl
  
-- ==================================================== tests ==================================================
kangarooShoeTestDo = do
   
  let seeBottomFacesWithDegrees = TestCase $ assertEqual
        "seeBottomFacesWithDegrees"
        []
        heelOfShoeCubesWithDegrees
  runTestTT seeBottomFacesWithDegrees
  
  let seeDegrees = TestCase $ assertEqual
        "seeDegrees"
        [(0,10),(10,20),(20,30),(30,40),(40,50),(50,60),(60,70),(70,80),(80,90),(90,100),(100,110),(110,120),(120,130),(130,140),(140,150),(150,160),(160,170),
         (170,180),(180,190),(190,200),(200,210),(210,220),(220,230),(230,240),(240,250),(250,260),(260,270),(270,280),(280,290),(290,300),(300,310),(310,320),
         (320,330),(330,340),(340,350),(350,360)]
        degrees
  runTestTT seeDegrees
  
  

































































































































































































     


  
