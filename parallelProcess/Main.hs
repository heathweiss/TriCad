module Main where

import Fib(fib)
import Control.Monad
import Text.Printf
import Scan.ParseJuicy(getRedLaserLineSingleImage, process10DegreeImagesToMultiDegreeRadiiBaseUsingMap )


{-
main_  =  forM_ [0..40] $ \i ->
            printf "n=%d => %d\n" i (fib i)
-}
main = process10DegreeImagesToMultiDegreeRadiiBaseUsingMap (getRedLaserLineSingleImage 175) "IMG_" "JPG" "src/Data/scanImages/" 2508
                                                                                             --fileNamePrefix  fileExtension filePath starterNumber
--main = putStrLn "hello"

{-
run from ghc with: -- run from ghc with: ./dist/build/parallelProcess/parallelProcess +RTS -N4
-Nx is the # of processors. I use N4 as I have 4 cores.
-l produces the output for threadscope, but threadscope runs out of stack space with 36 images. Have done 2 no problem. It is a +RTS param.
-s will show stats in console window. It is a +RTS param.

This allows the program to be run with multiple cores.
Can't do this by:
 -emacs haskell-mode repl
 -cabal run
-}
