module Examples.Shapes.Cube(hello, writeJoinerStlFile, joinerCube) where
import TriCad.Shapes.Cubical(rectangularCube)
import Stl.StlCornerPoints((+++^))
import TriCad.CornerPoints( Faces(..))
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
