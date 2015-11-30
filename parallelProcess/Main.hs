module Main where

import Fib(fib)
import Control.Monad
import Text.Printf



main  =  forM_ [0..40] $ \i ->
            printf "n=%d => %d\n" i (fib i)

{-
run from ghc with: -- run from ghc with: ./dist/build/parallelProcess/parallelProcess +RTS -Nx -l
-where x is the numbers of processors eg: N4

This allows the program to be run with multiple cores.
Can't do this by:
 -emacs haskell-mode repl
 -cabal run
-}
