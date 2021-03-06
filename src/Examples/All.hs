module Examples.All (evenLength, shifter, powerOfTwo, addOne) where

import Turing
import TuringTape
import UsingMap
import ToLatex

data StateQ = StateQ Int deriving (Eq, Read, Ord) 
q = StateQ

data StateQS = Qs String deriving (Eq, Read, Ord) 

instance Show StateQ where
  show (StateQ n) = show n

instance Show StateQS where
  show (Qs n) = show n

instance ToLatex StateQ where
  toLatex (StateQ n) = "$q_{" ++ show n ++ "}$"

instance ToLatex StateQS where
  toLatex (Qs n) = "$q_{" ++ n ++ "}$"

data Alphabet = A | B | C | D | E | F | G | H | I | J | K | M | N | O | P | Q |  S | T | U | V | X | Y | Z | Bl
  deriving (Show, Eq, Read, Ord) 

instance ToLatex Alphabet where
  toLatex Bl = "\\textvisiblespace"
  toLatex x = "\\tt{" ++ show x ++ "}"




evenLength = pairListToMachine [

    (q 0, A) --> (q 1, A, R)
  , (q 0, Bl) --> (q 3, E, R)

  , (q 1, A) --> (q 0, A, R)
  , (q 1, Bl) --> (q 4, O, R)

  ] Bl (q 0) (q 3) (q 4)


shifter = pairListToMachine [
    (q 0, X) --> (q 1, X, R)

  , (q 1, O) --> (q 1, O, R)
  , (q 1, I) --> (q 2, O, R)
  , (q 1, Bl) --> (q 3, O, L)

  , (q 2, O) --> (q 1, I, R)
  , (q 2, I) --> (q 2, I, R)
  , (q 2, Bl) --> (q 3, I, L)

  , (q 3, O) --> (q 3, O, L)
  , (q 3, I) --> (q 3, I, L)
  , (q 3, X) --> (q 4, X, L)

 ] Bl (q 0) (q 4) (q 4)



powerOfTwo = pairListToMachine [

    (q 1, Bl) --> (q 9, Bl, R)
  , (q 1, X) --> (q 9, X, R)
  , (q 1, O) --> (q 2, Bl, R)

  , (q 2, X) --> (q 2, X, R)
  , (q 2, Bl) --> (q 8, Bl, R)
  , (q 2, O) --> (q 3, X, R)

  , (q 3, O) --> (q 4, O, R)
  , (q 3, Bl) --> (q 5, Bl, L)
  , (q 3, X) --> (q 3, X, R)

  , (q 4, X) --> (q 4, X, R)
  , (q 4, O) --> (q 3, X, R)
  , (q 4, Bl) --> (q 9, Bl, R)

  , (q 5, O) --> (q 5, O, L)
  , (q 5, X) --> (q 5, X, L)
  , (q 5, Bl) --> (q 2, Bl, R)

 ] Bl (q 1) (q 8) (q 9)


addOne = pairListToMachine [
    (q_r, O) --> (q_r, O, R)
  , (q_r, I) --> (q_r, I, R)
  , (q_r, Bl) --> (q_c, Bl, L)

  , (q_c, I) --> (q_c, O, L)
  , (q_c, O) --> (q_d, I, L)
  , (q_c, Bl) --> (q_d, I, L)
    ] Bl q_r q_d q_d
         where q_r = Qs "right"
               q_c = Qs "carry"
               q_d = Qs "done"
