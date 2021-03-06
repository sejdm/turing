module UsingMap (pairListToMachine, (-->)) where

import Turing
import TuringTape
import qualified Data.Map as M
import Data.List

mapToMachine m b q0 qa qr = makeMachine ((nub $ qs ++ qs'), as, b, curry (flip (M.findWithDefault (qr, b, R)) m), q0, qa, qr)
  where qs = map fst $ M.keys m
        qs' = map (\(x,_,_) -> x) $ M.elems m
        as = nub (( map (\(_,x,_) -> x) $ M.elems m ) ++ map snd (M.keys m))



pairListToMachine l = mapToMachine (M.fromList l)

a --> b = (a, b)
