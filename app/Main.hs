module Main where

import Turing
import TuringTape
import System.Environment

data States = Q0 | Q1 | Q2 | Qa | Qr
  deriving (Eq, Show, Read, Enum, Bounded)

data Alphabet = O | I | B | X
  deriving (Eq, Show, Read, Enum, Bounded) 


eg Q0 X = (Q1, X, R)

eg Q1 O = (Q1, O, R)
eg Q1 I = (Q2, O, R)
eg Q1 B = (Qa, O, L)

eg Q2 O = (Q1, I, R)
eg Q2 I = (Q2, I, R)
eg Q2 B = (Qa, I, L)
  
eg Qa O = (Qa, O, L)
eg Qa I = (Qa, I, L)
eg Qa X = (Qr, X, L)
  
eg _ _ = (Qr, B, L)

machine1 = makeMachine (everything, everything, B, eg, Q0, Qr, Qr)




{-
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
-}

main :: IO ()
main = do
  as <- fmap (map read) getArgs

  turingToBeamer 5 21 machine1 as
  --makeBeamer (5, 21) everything everything eg acceptState rejectState (startState, tapeFromList blankSymbol as)
