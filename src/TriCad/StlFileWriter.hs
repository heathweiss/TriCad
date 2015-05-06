module TriCad.StlFileWriter(writeStlToFile, writeStlDebugToFile)  
where
import TriCad.StlBase(stlShapeToText, StlShape(..))
import TriCad.CornerPointsDebug((+++^?), (++^?), CubeName(..), CubeDebug(..), CubeDebugs(..), showDebugMsg)
import System.IO

{-
Writes the temp.stl file to Tricad/src/temp.stl folder when run from emacs repl.
Should have a config file in which this can be set.
given:
  shape: an StlShape generated form a file such as HeelGenerators, or Sockets
return:
  null as it is an IO() action.
  temp.stl file with the stlShape written to it. Will overrite whatever was in the file.
  Fails if file does not already exist.

Known uses:
BlackRunnerHeel uses this to write of the stl file.
-}
writeStlToFile :: StlShape -> IO()
writeStlToFile shape =  writeFile "temp.stl" $ stlShapeToText shape

{-
Writes the debyg.txt file to Tricad/src/debug.txt folder when run from emacs repl.
Should have a config file in which this can be set.
given:
  shape: an [CubeDebug] generated from a file such as HeelGenerators, or Sockets
return:
  null as it is an IO() action.
  debug.txt file with the CubeDebug written to it. Will overrite whatever was in the file.
  Fails if file does not already exist.

Needs to be formatted with /n. Should it be done here or in the CornerPointsDebug module?

Known uses:
BlackRunnerHeel uses this to write of the stl file.
-}
writeStlDebugToFile :: [CubeDebug] -> IO()
writeStlDebugToFile debugs = writeFile "debug.txt" $ show debugs
