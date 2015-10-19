module Examples.Primitives.Cube(hello, writeJoinerStlFile, joinerCube) where
import Primitives.Cubical(rectangularCube)
import Stl.StlCornerPoints((+++^))
import CornerPoints.CornerPoints( Faces(..))
import Stl.StlBase (StlShape(..), newStlShape)
import Stl.StlFileWriter(writeStlToFile)

writeJoinerStlFile = writeStlToFile stlFile
stlFile = newStlShape "joiner cube" joinerTriangles

joinerTriangles =
  [FacesAll]
  +++^
  joinerCube
  
joinerCube =
  [rectangularCube 2 25 50]

hello = do
  putStrLn "hello from cube"
