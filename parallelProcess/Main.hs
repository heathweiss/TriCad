module Main where

import Fib(fib)
import Control.Monad
import Text.Printf
import Scan.ParseJuicy(process10DegreeImagesToMultiDegreeRadiiMultiCoreWrapper)


{-
main_  =  forM_ [0..40] $ \i ->
            printf "n=%d => %d\n" i (fib i)
-}
main = process10DegreeImagesToMultiDegreeRadiiMultiCoreWrapper
i = 4

{-
run from ghc with: -- run from ghc with: ./dist/build/parallelProcess/parallelProcess +RTS -N4 -l
-Nx is the # of processors. I use N4 as I have 4 cores.

This allows the program to be run with multiple cores.
Can't do this by:
 -emacs haskell-mode repl
 -cabal run
-}
