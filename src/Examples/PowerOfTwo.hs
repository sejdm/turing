module Examples.PowerOfTwo (powerOfTwo) where

import Turing
import TuringTape

data States = Q0 | Q1 | Qh
  deriving (Eq, Show, Read, Enum, Bounded)

data Alphabet = A | X | B
  deriving (Show, Eq, Read, Enum, Bounded) 

  
eg Q0 A = (Q1, A, R)
eg Q0 X = (Q0, X, R)

eg Q1 A = (Q0, X, R)
eg Q1 X = (Q1, X, R)

eg _ _ = (Qh, B, L)

powerOfTwo = makeMachine (everything, everything, B, eg, Q0, Qh, Qh)
