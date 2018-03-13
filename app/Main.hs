module Main where

import Turing
import TuringTape
import System.Environment
import TableParse
import Examples.All
import Data.List.Split


main :: IO ()
main = do
  {-
  as <- fmap (map read) getArgs
  turingAnimate addOne (as)
-}

  (f:bs) <- getArgs
  --let as = map read bs
  t <- readFile f
  --print t
  --turingAnimate (fromTuringTableGloss t) (map alph' as)
  --turingToBeamer "Power of 2?" 1 35 powerOfTwo (map read bs)
  --usingTable plainStyle turingInteractive t as
  --usingTable (turingToBeamer (head $ wordsBy (=='.') $ last $ wordsBy (=='/') f)1 25) t bs
  usingTable (turingToBeamer (head $ wordsBy (\x -> x == '|') $ head $ lines t) 1 25) t bs
  --turingToBeamer 9 21 (fromTuringTable t) (map alph as)
