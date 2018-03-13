module TuringState (stateDelta, TState (Accept, Reject), reaction) where

import TuringTape

data TState a = Accept | Reject | TState String (a -> (TState a, a, Direction))

instance Eq (TState a) where
  Accept == Accept = True
  Reject == Reject = True
  _ == _ = False

instance Show (TState a) where
  show (TState s _) = s
  show Accept = "acc"
  show Reject = "re"
  
reaction = TState

stateDelta :: TState a -> a -> (TState a, a, Direction)
stateDelta (TState _ f) x = f x
stateDelta s x = (s, x, R)


init = reaction "init" $ \x -> case x of
  "I" -> (Accept, "I", R)
  "O" -> (Reject, "I", R)

moveTill s d c t = reaction s $ \x ->
  if x == c then (moveTill s d c t, x, d) else (t, x, d)



example =
    moveTill "a" R 'O'
  $ moveTill "b" L 'I'
  $ Accept
