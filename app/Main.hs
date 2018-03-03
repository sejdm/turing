module Main where

import Turing
import System.Environment


data States = Q0 | Q1 | Q2 | Qa | Qr
  deriving (Eq, Show, Read, Enum, Bounded, Ord)

startState = Q0
acceptState = Qa
rejectState = Qr

data Alphabet = O | I | B
  deriving (Eq, Show, Read, Enum, Bounded, Ord) 

blankSymbol = B


eg Q0 O = (Q0, O, R)
eg Q0 I = (Q0, I, R)
eg Q0 B = (Q1, B, R)

eg Q1 O = (Q2, O, L)
eg Q1 I = (Q1, O, L)
eg Q1 B = (Qr, I, R)

eg Q2 O = (Qa, O, L)
eg Q2 I = (Qr, I, L)
eg Q2 B = (Qr, B, R)
  
eg _ _ = (rejectState, blankSymbol, L)




main :: IO ()
main = do
  as <- fmap (map read) getArgs
  makeBeamer (5, 21) [ minBound .. maxBound ] [ minBound .. maxBound ]
    eg acceptState rejectState (startState, tapeFromList blankSymbol as)
