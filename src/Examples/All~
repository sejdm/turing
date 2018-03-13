module Examples.EvenLength (evenlength) where

import Turing
import TuringTape
import UsingMap

data StateQ = StateQ Int deriving (Eq, Read, Ord) 
q = StateQ

instance Show StateQ where
  show (StateQ n) = "$q_{" ++ show n ++ "}$"

data Alphabet = A | B | C | D | E | F | G | H | I | J | K | M | N | O | P | Q |  S | T | U | V | X | Y | Z 
  deriving (Show, Eq, Read, Ord) 

evenlength = pairListToMachine [

    (q 0, A) --> (q 1, A, R)
  , (q 0, B) --> (q 3, E, R)

  , (q 1, A) --> (q 0, A, R)
  , (q 1, B) --> (q 3, O, R)

  ] B (q 0) (q 3) (q 3)
