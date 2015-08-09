{-# LANGUAGE ParallelListComp #-}
module Scan.Parse.Raw(parseToChar, parseToDouble, parseToDoubleFiltered, parseToRadiusFiltered) where
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.Split as LS
import Scan.Parse(readDouble)
import TriCad.MathPolar( Radius(..))

{-
Parse
-}
parseToChar :: BL.ByteString -> [[[[Char]]]]
parseToChar bs =
  let splitDegree = LS.splitOn "$"  $ BL.unpack bs --[[degree]]
      splitColon =  map (LS.splitOn ";") splitDegree -- [[[row]]]
      --splitSpace =  map (LS.splitOn " ") splitColon  -- [[[[char]]]]
      splitSpace = [map (LS.splitOn " ") x | x <- splitColon ]
  in  splitSpace

--does a good job of parsing to a [[[1.0,2.0],[3.0,4.0]],[[5.0,6.0],[7.0,8.0]]]
parseToDouble bs =  do contents <- LS.splitOn "$" $ BL.unpack bs
                       let splitToSpaces = ((map (LS.splitOn ";") (LS.splitOn "$" contents)))
                           splitToChars = [map (LS.splitOn " ") x | x <- splitToSpaces]
                           
                       [[map (readDouble) y | y <-  x] | x <- splitToChars ]
                       
                      
--now supply a filter function on parseW
parseToDoubleFiltered f bs =  do contents <- LS.splitOn "$" $ BL.unpack bs
                                 let splitToSpaces = ((map (LS.splitOn ";") (LS.splitOn "$" contents)))
                                     splitToChars = [map (LS.splitOn " ") x | x <- splitToSpaces]
                                     readToDoubles = [[map ( readDouble) y | y <-  x] | x <- splitToChars ]
                                 [[f y  | y  <-  x] | x <- readToDoubles ]


parseToRadiusFiltered       f bs =  do contents <- LS.splitOn "$" $ BL.unpack bs
                                       let splitToSpaces = ((map (LS.splitOn ";") (LS.splitOn "$" contents)))
                                           splitToChars = [map (LS.splitOn " ") x | x <- splitToSpaces]
                                           readToDoubles = [[map ( readDouble) y | y <-  x] | x <- splitToChars ]
                                       [[((Radius) . f ) y  | y  <-  x] | x <- readToDoubles ]


