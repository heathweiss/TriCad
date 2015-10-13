module Sockets.Bicycle (bicycleSocketDebug, bicycleSocketStlFile ) where
import TriCad.MathPolar(
  slopeAdjustedForVerticalAngle,
  createTopFaces,
  createBottomFacesSimplified,
  createTopFacesWithVariableSlope,
  radiusAdjustedForZslope,
  xyQuadrantAngle,
  createCornerPoint,
  Slope(..),
  Radius(..),
  Angle(..),
  flatXSlope,
  flatYSlope,
  )
import TriCad.Points(Point(..))
import TriCad.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import TriCad.StlCornerPoints((+++^))
import TriCad.StlBase (StlShape(..), newStlShape)
import TriCad.CornerPointsFaceExtraction ( extractTopFace, extractBottomFrontLine, extractFrontTopLine, extractBackTopLine, extractBottomFace, extractBackBottomLine, extractFrontFace ) 
import TriCad.CornerPointsFaceConversions(lowerFaceFromUpperFace, backBottomLineFromBottomFrontLine, backTopLineFromFrontTopLine, frontTopLineFromBackTopLine, upperFaceFromLowerFace, bottomFrontLineFromBackBottomLine)
import TriCad.CornerPointsTranspose ( transposeZ, transposeX)
import TriCad.CornerPointsDebug((+++^?), CubeName(..), CubeDebug(..), CubeDebugs(..))

bicycleSocketDebug = v0CubesDebug

bicycleSocketStlFile = newStlShape "Bicycle Socket" $ adaptorTriangles ++  v0Triangles ++ v1Triangles ++ v2Triangles ++ v3Triangles ++ v4Triangles ++ v5Triangles ++ v6Triangles

--enlarge the inner radii to create the wall.
outerRadiusFactor = 1.1

--height of each layer, except v0
layerHeight = 10
adaptorHeight = 13 --the length of threads going into the socket.

--height of the top of v0, to which all other layers will be added. This in turn is added to the adaptor.
v0Height = 10 + adaptorHeight

--the bottom, which is the bottom of the adpator
origin = (Point{x_axis=0, y_axis=0, z_axis=0})
adaptorTopOrigin = (Point{x_axis=0, y_axis=0, z_axis=adaptorHeight})

{------------------------------------------------ v6 --------------------------------}
v6InnerRadius = 
    [Radius 26.5,--0
     Radius 26,--1
     Radius 24.9,--2
     Radius 23.3,--3
     Radius 21.8,--4
     Radius 19.9, --5
     Radius 18.9,--6
     Radius 18.9,--7
     Radius 18.9,--8
     Radius 18.9,--9
     Radius 19,--10
     Radius 19.5,--11
     Radius 20.8,--12
     Radius 22.6, --13
     Radius 24.6,--14
     Radius 25.8,--15
     Radius 26.5,--16
     Radius 27,--17
     Radius 27.3, --18 180 degrees. should be 12, but make big for devel. ref.
     Radius 27, --17
     Radius 26, --16
     Radius 24.3,--15
     Radius 23.7,--14
     Radius 22.7,--13
     Radius 21.2,--12
     Radius 20.1,--11
     Radius 19.2,--10
     Radius 18.7,--9
     Radius 18.8,--8
     Radius 20,--7
     Radius 20.3,--6
     Radius 21.7,--5
     Radius 23.5,--4
     Radius 24.2,--3
     Radius 25.3,--2
     Radius 25.8,--1
     Radius 26.5--0
    ]
    
v6OuterRadius = map (\(Radius x) -> (Radius (x * outerRadiusFactor))) v6InnerRadius

v6Origin = (Point{x_axis=0, y_axis=0, z_axis=(v0Height + (layerHeight*6))})

v6Triangles =
   [FacesBackFrontTop | x <- [1..36]]
   +++^
   v6Cubes

v6Cubes = v5Cubes ++++ v6TopFaces

v6TopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces v6Origin v6OuterRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces v6Origin v6InnerRadius angles flatXSlope (PosYSlope 0))






{------------------------------------------------ v5 --------------------------------}
v5InnerRadius = 
    [Radius 26.1,--0
     Radius 25.7,--1
     Radius 24.8,--2
     Radius 23.7,--3
     Radius 22.2,--4
     Radius 20.9, --5
     Radius 19.4,--6
     Radius 18.6,--7
     Radius 18.2,--8
     Radius 18.2,--9
     Radius 18.7,--10
     Radius 19.4,--11
     Radius 21.1,--12
     Radius 21.5, --13
     Radius 23.1,--14
     Radius 24.3,--15
     Radius 25.3,--16
     Radius 26,--17
     Radius 26, --18 180 degrees
     Radius 25.3, --17
     Radius 24.2, --16
     Radius 23.1,--15
     Radius 21.2,--14
     Radius 19.9,--13
     Radius 19.3,--12
     Radius 18.7,--11
     Radius 18.7,--10
     Radius 18.7,--9
     Radius 18.7,--8
     Radius 20.2,--7
     Radius 21.1,--6
     Radius 21.9,--5
     Radius 23.6,--4
     Radius 23.6,--3
     Radius 25,--2
     Radius 25.4,--1
     Radius 26.1--0
    ] 
v5OuterRadius = map (\(Radius x) -> (Radius (x * outerRadiusFactor))) v5InnerRadius

v5Origin = (Point{x_axis=0, y_axis=0, z_axis=(v0Height + (layerHeight*5))})

v5Triangles =
   [FacesBackFront | x <- [1..36]]
   +++^
   v5Cubes

v5Cubes = v4Cubes ++++ v5TopFaces

v5TopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces v5Origin v5OuterRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces v5Origin v5InnerRadius angles flatXSlope (PosYSlope 0))





{------------------------------------------------ v4 --------------------------------}
v4InnerRadius = 
    [Radius 25,--0
     Radius 25,--1
     Radius 23.7,--2
     Radius 22.6,--3
     Radius 21.4,--4
     Radius 20.1, --5
     Radius 19,--6
     Radius 18.5,--7
     Radius 17.6,--8
     Radius 17.6,--9
     Radius 18.4,--10
     Radius 18.7,--11
     Radius 19.7,--12
     Radius 20, --13
     Radius 22.1,--14
     Radius 23.3,--15
     Radius 23.4,--16
     Radius 24.5,--17
     Radius 24.5, --18 target=================================
     Radius 24.3, --17
     Radius 23, --16
     Radius 21.7,--15
     Radius 20.1,--14
     Radius 19.9,--13
     Radius 18.7,--12
     Radius 18.1,--11
     Radius 17.9,--10
     Radius 17.8,--9
     Radius 18.1,--8
     Radius 19,--7
     Radius 20,--6
     Radius 20.9,--5
     Radius 22,--4
     Radius 23.1,--3
     Radius 23.8,--2
     Radius 25,--1
     Radius 25--0
    ]
v4OuterRadius = map (\(Radius x) -> (Radius (x * outerRadiusFactor))) v4InnerRadius

v4Origin = (Point{x_axis=0, y_axis=0, z_axis=(v0Height + (layerHeight*4))})

v4Triangles =
   [FacesBackFront | x <- [1..36]]
   +++^
   v4Cubes

v4Cubes = v3Cubes ++++ v4TopFaces

v4TopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces v4Origin v4OuterRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces v4Origin v4InnerRadius angles flatXSlope (PosYSlope 0))



{------------------------------------------------ v3 --------------------------------}
v3InnerRadius = 
    [Radius 24.8,--0
     Radius 24.5,--1
     Radius 22,--2
     Radius 21.6,--3
     Radius 20.3,--4
     Radius 19, --5
     Radius 18,--6
     Radius 17.5,--7
     Radius 17,--8
     Radius 16.9,--9
     Radius 17.3,--10
     Radius 17.7,--11
     Radius 18.6,--12
     Radius 19.1, --13
     Radius 20.9,--14
     Radius 21.8,--15
     Radius 22.1,--16
     Radius 22.4,--17
     Radius 23.3, --18 
     Radius 23.3, --17
     Radius 21.9, --16
     Radius 21,--15
     Radius 19.5,--14
     Radius 18.1,--13
     Radius 18.1,--12
     Radius 17.1,--11
     Radius 16.9,--10
     Radius 16.9,--9
     Radius 16.8,--8
     Radius 17.2,--7
     Radius 18.4 ,--6
     Radius 19.3,--5
     Radius 21,--4
     Radius 21.9,--3
     Radius 23.4,--2
     Radius 24,--1
     Radius 24.8--0
    ] 

v3OuterRadius = map (\(Radius x) -> (Radius (x * outerRadiusFactor))) v3InnerRadius

v3Origin = (Point{x_axis=0, y_axis=0, z_axis=(v0Height + (layerHeight*3))})

v3Triangles =
   [FacesBackFront | x <- [1..36]]
   +++^
   v3Cubes

v3Cubes = v2Cubes ++++ v3TopFaces

v3TopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces v3Origin v3OuterRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces v3Origin v3InnerRadius angles flatXSlope (PosYSlope 0))

{------------------------------------------------v2----------------------------------

-}

v2InnerRadius = map (\(Radius x) -> Radius (x * 0.9))
    [Radius 24.7,--0
     Radius 24,--1
     Radius 21.9,--2
     Radius 20.6,--3
     Radius 19.2,--4
     Radius 17.8, --5
     Radius 16.9,--6
     Radius 16.5,--7
     Radius 16.2,--8
     Radius 16.2,--9
     Radius 16.2,--10
     Radius 16.7,--11
     Radius 17.5,--12
     Radius 18.2, --13
     Radius 19.7,--14
     Radius 20.3,--15
     Radius 21,--16
     Radius 21.3,--17
     Radius 22.1, --18 180 degrees. should be 12, but make big for devel. ref.
     Radius 21.2, --17
     Radius 20.8, --16
     Radius 20.6,--15
     Radius 19.7,--14
     Radius 18.7,--13
     Radius 17.6,--12
     Radius 16.8,--11
     Radius 16.4,--10
     Radius 16.4,--9
     Radius 16.4,--8
     Radius 17.9,--7
     Radius 18.9,--6
     Radius 20.1,--5
     Radius 21.2,--4
     Radius 22.7,--3
     Radius 24.2,--2
     Radius 24.5,--1
     Radius 24.7--0
    ]  

v2OuterRadius = map (\(Radius x) -> (Radius (x * outerRadiusFactor))) v2InnerRadius

v2Origin = (Point{x_axis=0, y_axis=0, z_axis=(v0Height + (layerHeight*2))})

v2Triangles =
   [FacesBackFront | x <- [1..36]]
   +++^
   v2Cubes

v2CubesDebug = 
  [CubeName "v2 cubes" | x <- [1..]]
    +++^?
    v2Cubes

v2Cubes = v1Cubes ++++ v2TopFaces

v2TopFacesDebug = 
  [CubeName "v2 top Faces" | x <- [1..]]
    +++^?
    v2TopFaces

v2TopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces v2Origin v2OuterRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces v2Origin v2InnerRadius angles flatXSlope (PosYSlope 0))


{------------------------------------------------v1----------------------------------

-}

v1InnerRadius = map (\(Radius x) -> Radius (x * 0.9))
    [Radius 21.8,--0
     Radius 20.8,--1
     Radius 19.5,--2
     Radius 19.5,--3
     Radius 18,--4
     Radius 16.4, --5
     Radius 15.8,--6
     Radius 15.4,--7
     Radius 14.6,--8
     Radius 14.6,--9
     Radius 14.6,--10
     Radius 15,--11
     Radius 15.5,--12
     Radius 16.1, --13
     Radius 17.5,--14
     Radius 18.8,--15
     Radius 19.3,--16
     Radius 20.2,--17
     Radius 20.8, --18 180 degrees. should be 12, but make big for devel. ref.
     Radius 20.3, --17
     Radius 19.1, --16
     Radius 18.1,--15
     Radius 16.9,--14
     Radius 16.5,--13
     Radius 15.8,--12
     Radius 15.5,--11
     Radius 14.4,--10
     Radius 14.5,--9
     Radius 15.4,--8
     Radius 15.8,--7
     Radius 16.6,--6
     Radius 17.9,--5
     Radius 19.5,--4
     Radius 20.9,--3
     Radius 21.4,--2
     Radius 22.3,--1
     Radius 21.8--0
    ]  

v1OuterRadius = map (\(Radius x) -> (Radius (x * outerRadiusFactor))) v1InnerRadius

v1Origin = (Point{x_axis=0, y_axis=0, z_axis=(v0Height + layerHeight)})

v1Triangles =
   [FacesBackFront | x <- [1..36]]
   +++^
   v1Cubes

v1CubesDebug = 
  [CubeName "v1 cubes" | x <- [1..]]
    +++^?
    v1Cubes

v1Cubes = v0Cubes ++++ v1TopFaces

v1TopFacesDebug = 
  [CubeName "v1 top Faces" | x <- [1..]]
    +++^?
    v1TopFaces

v1TopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces v1Origin v1OuterRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces v1Origin v1InnerRadius angles flatXSlope (PosYSlope 0))

{----------------------------------------------- v0 -----------------------------------
Has an extended length to leave room for his hand and pinkie.

Will need some sort of a connector plate. 
-}
--radius of the v0 layer

v0InnerRadius = map (\(Radius x) -> Radius (x * 0.9))
    [Radius 15.6,--0
     Radius 14.1,--1
     Radius 13.7,--2
     Radius 13.2,--3
     Radius 12.6,--4
     Radius 11.9, --5
     Radius 11.3,--6
     Radius 10.2,--7
     Radius 9.6,--8
     Radius 9.6,--9
     Radius 9.7,--10
     Radius 9.7,--11
     Radius 10.6,--12
     Radius 11.7, --13
     Radius 12,--14
     Radius 13.6,--15
     Radius 14.3,--16 
     Radius 15.8,--17
     Radius 16.3, --18 180 degrees. should be 16.3, but make big for devel. ref.
     Radius 16.3, --17
     Radius 16.3, --16
     Radius 15.3,--15
     Radius 14.5,--14
     Radius 14.1,--13
     Radius 13.2,--12
     Radius 12.9,--11
     Radius 12.9,--10
     Radius 12.9,--9
     Radius 13.3,--8
     Radius 13.5,--7
     Radius 13.8,--6
     Radius 14,--5
     Radius 15,--4
     Radius 15.2,--3
     Radius 15.4,--2
     Radius 15.4,--1
     Radius 15.6--0
    ] 
    
v0OuterRadius = map (\(Radius x) -> (Radius (x * outerRadiusFactor))) v0InnerRadius

angles = [0,10..360]

v0BtmOrigin = (Point{x_axis=0, y_axis=0, z_axis=adaptorHeight})
v0TopOrigin = (Point{x_axis=0, y_axis=0, z_axis=v0Height})

v0Triangles =
   [FacesBackFront | x <- [1..36]]
   +++^
   v0Cubes

v0CubesDebug = 
  [CubeName "v0 cubes" | x <- [1..]]
  +++^?
  v0Cubes

v0Cubes = adaptorCubes ++++ v0TopFaces 

v0TopFacesDebug = 
  [CubeName "v0 top Faces" | x <- [1..]]
    +++^?
    v0TopFaces

v0TopFaces =
    --front line
    map (extractFrontTopLine) (createTopFaces v0TopOrigin v0OuterRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces v0TopOrigin v0InnerRadius angles flatXSlope (PosYSlope 0))

v0BtmFacesDebug = 
  [CubeName "v0 btm Faces" | x <- [1..]]
    +++^?
    v0BtmFaces

v0BtmFaces = 
  --front line
  map (extractBottomFrontLine)
      (createBottomFacesSimplified v0BtmOrigin v0OuterRadius (map (Angle) angles) flatXSlope flatYSlope)
  ++++
  --back line
  map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
      (createBottomFacesSimplified v0BtmOrigin v0InnerRadius (map (Angle) angles) flatXSlope flatYSlope)

{------------------------------------------------------- adpt ----------------------------------------
adapt from the 12.56 od mm bolt to v0 of the sockect
-}

adaptorInnerRadius = [Radius 6 | x <- [1..37]]
adaptorOuterRadius = [Radius 9 | x <- [1..37]]

adaptorTriangles = [FacesBackBottomFront | x <- [1..36]]
   +++^
   adaptorCubes

adaptorCubesDebug = 
  [CubeName "adaptor cubes Faces" | x <- [1..]]
  +++^?
  adaptorCubes

adaptorCubes = adaptorTopFaces ++++ adaptorBtmFaces

adaptorTopFacesDebug =
    [CubeName "adaptor top Faces" | x <- [1..]]
    +++^?
    adaptorTopFaces

adaptorTopFaces  =
    --front line
    map (extractFrontTopLine) (createTopFaces adaptorTopOrigin adaptorOuterRadius angles flatXSlope (PosYSlope 0))
    ++++
    --back line
    map (backTopLineFromFrontTopLine . extractFrontTopLine) (createTopFaces adaptorTopOrigin adaptorInnerRadius angles flatXSlope (PosYSlope 0))

adaptorBtmFacesDebug = 
  [CubeName "adaptor btm Faces" | x <- [1..]]
    +++^?
    adaptorBtmFaces
    
adaptorBtmFaces = 
  --front line
  map (extractBottomFrontLine)
      (createBottomFacesSimplified origin adaptorOuterRadius (map (Angle) angles) flatXSlope flatYSlope)
  ++++
  --back line
  map (backBottomLineFromBottomFrontLine . extractBottomFrontLine)
      (createBottomFacesSimplified origin adaptorInnerRadius (map (Angle) angles) flatXSlope flatYSlope)

{-
v0InnerRadius = map (\(Radius x) -> Radius (x * 0.9))
    [Radius ,--0
     Radius ,--1
     Radius ,--2
     Radius ,--3
     Radius ,--4
     Radius , --5
     Radius ,--6
     Radius ,--7
     Radius ,--8
     Radius ,--9
     Radius ,--10
     Radius ,--11
     Radius ,--12
     Radius , --13
     Radius ,--14
     Radius ,--15
     Radius ,--16
     Radius ,--17
     Radius , --18 180 degrees. should be 12, but make big for devel. ref.
     Radius , --17
     Radius , --16
     Radius ,--15
     Radius ,--14
     Radius ,--13
     Radius ,--12
     Radius ,--11
     Radius ,--10
     Radius ,--9
     Radius ,--8
     Radius ,--7
     Radius ,--6
     Radius ,--5
     Radius ,--4
     Radius ,--3
     Radius ,--2
     Radius ,--1
     Radius --0
    ] 
-}
