{-# LANGUAGE ParallelListComp #-}
module CornerPoints.VerticalFaces(
  createRightFaces,
  createLeftFaces,
  createHorizontallyAlignedCubes,
  createLeftFacesMultiColumns,
  SingleDegreeRadii(..),
  MultiDegreeRadii(..),
  TransposeFactor(..), transpose) where
import CornerPoints.Create(Slope(..), Origin(..), createCornerPoint, Angle(..), Degree(..))
import CornerPoints.CornerPoints(CornerPoints(..), (++>), (+++), (++++), Faces(..))
import CornerPoints.Transpose (transposeZ)
import CornerPoints.Points(Point(..))
import CornerPoints.Radius(Radius(..))
import CornerPoints.Transposable( TransposeLength, transpose)
                           



----------------------------------------- create left/right faces from Scan datatype------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{----------------------------------------- create left/right faces, no Scan datatype----------------------------------------------------
This is a base that gets called by createLeftFaces and createRightFaces, with just a different set of CornerPoints constructors.
It uses [[Radius]] and [degrees], instead of the new Scan datatype. Once the Scan datatype can be used, this will be removed.

Build [Left/RightFaces], from an array of Radius that runs downwards along
a single degree. This is the way that the scanner process the images, and gives them to TriCad.
If the data is supplied in an upwards direction, reverse the data, then use this same function, instead of
building a separate function for that purpose.

The vertical distance of each height, is the same for all pixels. Therefore, the height of each pixel
is a factor of location in the array. So to calculate the z_axis(height), an array is passed in which
will be zipped with the RightFaces after they have been created, using TransposeZ.
It would be more efficient to calculate it as the Faces are made up, but then would not be able to re-use
the createCornerPoint function.


Known uses:
Scanning
Scanning is done vertically, so it would be best to build the model that way, instead of transforing the data
to fit the horizontal model supplied by 'createBottomFaces'/'createTopFaces'

Create a set of right faces, which will be used as the initial faces, to which all the subsequent left
faces will be added via ++>. This is used to build both right/left faces.

Create in a top down direction, as that is the way the openCV data is supplied.

------------ given -----------
topOrigin:
-Point that gives the inner origin for the topmost face caculated. This is also the first Radius.

degree:
-The current degree on xy axis. Should be 0.

xSlope:
-x-axis slope on the top of the shape

ySlope:
-y-axis slope on the top of the shape

zTransposeFactor:
-An array of the heights(z_axis value) associated with each face. This value will be subtracted
 from the topOrigin z_axis.

inRadius:
-The array of Radius, which is the value read in from the file, as was calculated openCV.
 This is the location of the target value, in pixels. It needs to be translated into a distance.

---------returns ----------
An array of LeftFace or RightFace depending on data constructors passed in.
-}



createVerticalFaces :: Origin -> SingleDegreeRadii -> Slope -> Slope -> [TransposeFactor] -> (Point-> CornerPoints) ->
                       (Point-> CornerPoints) -> (Point-> CornerPoints) -> (Point-> CornerPoints) -> [CornerPoints]
createVerticalFaces origin (SingleDegreeRadii degree' radii') xSlope ySlope zTransposeFactor topFrontConstructor topBackConstructor
                            btmFrontConstructor btmBackConstructor =
  
  --zipWith  (\x y -> transposeZ ((-x)+) y  ) --negating x causes the z_axis to decrease from the top, as it should.
  -- zTransposeFactor --this should be an infinit of the height for each pixel, measured from topOrigin.
   let topFrontPoint =
         (createCornerPoint
           (topFrontConstructor)
           origin
           (head $ radii') 
           (Angle degree')
           xSlope
           ySlope
         )
       topBackPoint =  topBackConstructor origin

       topLine = topFrontPoint +++ topBackPoint

       bottomLines =
         [(createCornerPoint
            (btmFrontConstructor)
            (transposeZ (+(-currZVal)) origin)  --topOrigin
            currRadius
            (Angle degree')
            xSlope
            ySlope
          ) 
          +++
          btmBackConstructor (transposeZ (+(-currZVal)) origin)  --topOrigin
            | currRadius <- tail $ radii'
            | currZVal <-  zTransposeFactor
         ]
       
   in  
       topLine
       ++>
       bottomLines
   

-- |Create a column of RightFace
createRightFaces :: Origin -> SingleDegreeRadii -> Slope -> Slope -> [TransposeFactor] -> [CornerPoints]
createRightFaces origin singleDegreeRadii xSlope ySlope zTransposeFactor  =
  createVerticalFaces origin singleDegreeRadii xSlope ySlope zTransposeFactor (F3) (B3) (F4) (B4)


-- |Create a column of LeftFace 
createLeftFaces :: Origin -> SingleDegreeRadii -> Slope -> Slope -> [TransposeFactor] -> [CornerPoints]
createLeftFaces origin singleDegreeRadii xSlope ySlope zTransposeFactor  =
  createVerticalFaces origin singleDegreeRadii xSlope ySlope zTransposeFactor (F2) (B2) (F1) (B1)



{-
Can already create LeftFace, problem is to create an array of them from the vertical [Radius].
Cannot just map over them with createLeftFaces, because the degree is changing from column to column.
For this reason, will have to use recursion.
-}
createLeftFacesMultiColumns ::  Origin -> [SingleDegreeRadii] -> Slope -> Slope -> [TransposeFactor] -> [[CornerPoints]]
createLeftFacesMultiColumns _ [] _ _ _ = []
createLeftFacesMultiColumns topOrigin (d:ds) xSlope ySlope zTransposeFactor =
  (createLeftFaces topOrigin d xSlope ySlope zTransposeFactor ) :
    (createLeftFacesMultiColumns topOrigin ds xSlope ySlope zTransposeFactor)




{- |Join a [RightFace] to [[LeftFace]]
    Results in [[CornerPoints]] where each inner list represents horizontal layer..-}
{-

Normally: RightFace ++> [LeftFaces], instead of  [RightFace] ++> [[LeftFace]],  so need recursion to work through the extra level of lists.
-}
createHorizontallyAlignedCubes :: [CornerPoints] -> [[CornerPoints]] -> [[CornerPoints]]
createHorizontallyAlignedCubes ([]) _ = []
createHorizontallyAlignedCubes (x:xs) (ys) =
  let headOfLeftFaces = map (head) ys
  in (x ++> headOfLeftFaces) : (createHorizontallyAlignedCubes xs (map (tail) ys) )


-- Amount used to transpose a point
type TransposeFactor = Double


{-
Contains the [Radius] associated with a single degree from a vertical scan.

Scan.Json module declares it an instance of ToJSON and FromJSON for the aeson package.

Known uses:
Raw image data is parsed into Scan datatype, which contains [Degree]. This is then
processed into cubes.

Store the processed raw data as json, so the processing only has to be done once.
-}
data SingleDegreeRadii = SingleDegreeRadii {degree::Degree, radii::[Radius]}
     deriving (Show, Eq)

instance TransposeLength SingleDegreeRadii  where
  transpose f (SingleDegreeRadii degree' radii') = SingleDegreeRadii degree' (map (transpose f) radii')

{- |
Contains all the filtered data from a scan.
Is a [SingleDegreeRadii] and an assoc'd name.

Known uses:
Raw scan image data is processed into this, which is the last ADT, before being turned in CornerPoints.
It can be read to/from json, so that all the processing of scan data, can be saved to file.
-}
data MultiDegreeRadii = MultiDegreeRadii {name::String, degrees::[SingleDegreeRadii]}
          deriving (Show, Eq)

instance TransposeLength MultiDegreeRadii  where
  transpose f (MultiDegreeRadii name' degrees') = MultiDegreeRadii name' (map (transpose f) degrees')
