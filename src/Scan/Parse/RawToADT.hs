{-# LANGUAGE ParallelListComp #-}
-- {-# LANGUAGE OverloadedStrings #-}
module Scan.Parse.RawToADT(parseToScan) where
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.Split as LS
import Scan.Parse(readDouble)
import TriCad.MathPolar( Radius(..))
import Scan.Json(Degree(..), Scan(..))
import Scan.Transform(minValueIndices, average)




parseToRadiusFiltered       f bs =  do contents <- LS.splitOn "$" $ BL.unpack bs
                                       let splitToSpaces = ((map (LS.splitOn ";") (LS.splitOn "$" contents)))
                                           splitToChars = [map (LS.splitOn " ") x | x <- splitToSpaces]
                                           readToDoubles = [[map ( readDouble) y | y <-  x] | x <- splitToChars ]
                                       [[((Radius) . f ) y  | y  <-  x] | x <- readToDoubles ]


parseToDegreeVals bs =  do contents <- LS.splitOn "$" $ BL.unpack bs
                           let splitToSpaces = ((map (LS.splitOn ";") (LS.splitOn "$" contents)))
                               splitToChars = [map (LS.splitOn " ") x | x <- splitToSpaces]
                           
                           [[readDouble y | y:ys <-  x] | x <- splitToChars ]

parseToScan_ f bs = do contents <- LS.splitOn "$" $ BL.unpack bs
                       let splitToSpaces = ((map (LS.splitOn ";") (LS.splitOn "$" contents)))
                           splitToChars = [map (LS.splitOn " ") x | x <- splitToSpaces]
                           degreeValsMulti =  [[readDouble y | y:ys <-  x] | x <- splitToChars ]
                           degreeVals = map head degreeValsMulti
                           readToDoublesNoDegreeVals = [[map ( readDouble) ys | y:ys <-  x] | x <- splitToChars ]
                           readToRadius =  [[((Radius) . f ) y  | y  <-  x] | x <- readToDoublesNoDegreeVals ]
                           degrees = zipWith (Degree) degreeVals readToRadius
                           scan = Scan {name= "myScan", degrees= degrees}
                       show scan

parseToScan :: ([Double] -> Double) -> [Char] -> Scan
parseToScan f str = 
                      let splitToSpaces = ((map (LS.splitOn ";") (LS.splitOn "$" str)))
                          splitToChars = [map (LS.splitOn " ") x | x <- splitToSpaces]
                          degreeValsMulti =  [[readDouble y | y:ys <-  x] | x <- splitToChars ]
                          degreeVals = map head degreeValsMulti
                          readToDoublesNoDegreeVals = [[map ( readDouble) ys | y:ys <-  x] | x <- splitToChars ]
                          readToRadius =  [[((Radius) . f ) y  | y  <-  x] | x <- readToDoublesNoDegreeVals ]
                          degrees = zipWith (Degree) degreeVals readToRadius
                          scan = Scan {name= "myScan", degrees= degrees}
                        in
                        scan


testFromString = (parseToScan (average . minValueIndices 5 ) "0 1 3 6;0 3 4 5 7$90 2 4 6; 90 3 4 8")

{-
The use of LS.splitOn is what I have to do to make it compile, and have correct output. WTF?
If I omit the parseToScan type signature, I can get rid of LS.splitOn "%", but then the output is incorrect
-}
testFromContrivedByteString = do contents <- LS.splitOn "%" $  BL.unpack $ BL.pack "0 1 3 6;0 3 4 5 7$90 2 4 6; 90 3 4 8" 
                                 let t = (parseToScan (average . minValueIndices 5 ) contents)
                                 show t

testFromFile = do contents <-   BL.readFile "/home/heath/haskell_projects/Tricad/sample.raww"
                  let temp =  BL.unpack contents
                  print $ show $  (parseToScan (average . minValueIndices 5 ) temp)

{-
compiles and tests but has 2 Scan objects
parseToScan f bs = do contents <- LS.splitOn "$" $ BL.unpack bs
                      let splitToSpaces = ((map (LS.splitOn ";") (LS.splitOn "$" contents)))
                          splitToChars = [map (LS.splitOn " ") x | x <- splitToSpaces]
                          degreeValsMulti =  [[readDouble y | y:ys <-  x] | x <- splitToChars ]
                          degreeVals = map head degreeValsMulti
                          readToDoublesNoDegreeVals = [[map ( readDouble) ys | y:ys <-  x] | x <- splitToChars ]
                          readToRadius =  [[((Radius) . f ) y  | y  <-  x] | x <- readToDoublesNoDegreeVals ]
                          degrees = zipWith (Degree) degreeVals readToRadius
                          scan = Scan "myScan" degrees
                      show scan
-}

