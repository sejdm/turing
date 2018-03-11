module Examples.Shifter (shifter) where

import Turing
import TuringTape

data States = Q0 | Q1 | Q2 | Q3 | Qh
  deriving (Eq, Show, Read, Enum, Bounded)

data Alphabet = O | I | B | X
  deriving (Show, Eq, Read, Enum, Bounded) 

  
eg Q0 X = (Q1, X, R)

eg Q1 O = (Q1, O, R)
eg Q1 I = (Q2, O, R)
eg Q1 B = (Q3, O, L)

eg Q2 O = (Q1, I, R)
eg Q2 I = (Q2, I, R)
eg Q2 B = (Q3, I, L)
  
eg Q3 O = (Q3, O, L)
eg Q3 I = (Q3, I, L)
eg Q3 X = (Qh, X, L)
  
eg _ _ = (Qh, B, L)

shifter = makeMachine (everything, everything, B, eg, Q0, Qh, Qh)
