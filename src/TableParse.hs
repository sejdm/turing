{-# LANGUAGE NoMonomorphismRestriction #-}
module TableParse (usingTable) where

import qualified Data.Map as M
import Data.List.Split
import TuringTape
import Turing
import Data.List
import ToLatex

newtype State = Q String deriving (Eq, Ord)
newtype Alphabet  = Alph String  deriving (Eq, Ord, Read)
alph = Alph

instance Show Alphabet where
  show (Alph "%") = " "
  show (Alph s) = s

instance Show State where
  show (Q s) = s


instance ToLatex Alphabet where
  toLatex (Alph "%") = "\\textvisiblespace"
  toLatex (Alph s) = filter (/='"') s

instance ToLatex State where
  toLatex (Q s) = "$q_{" ++ filter (/='"') s ++ "}$"

--makeMap :: Ord a => [a] -> [[a]] -> M.Map (a, a) a
makeMap h (xs) = (map rmSpc h, M.fromList $ mkF (map rmSpc h) xs)
  where
    mkF h [] = [] 
    mkF h ((c:cs):rs) = zip (zip (repeat (rmSpc c)) h) cs ++ mkF h rs

    rmSpc = filter (/=' ')

mapToBinFun = curry . flip M.lookup


toTuple qc al s = let x:y:z:_ = wordsBy (\x -> x == ' ' || x == ',') s in (qc x, al y, read z :: Direction)



tableToMap qc al s = (map al (fst ys), M.mapKeys (\(q, a) -> (qc q, al a)) $ M.map (toTuple qc al) $ snd ys)
    where ys = makeMap (tail $ wordsBy (\x -> x == ' ' || x == '|') h) $ map (wordsBy (=='|')) xs
          (h:xs) = lines s


completeFun qc al = curry . flip (M.findWithDefault (qc "re", al "%", L))



fromTuringTable' qc al s = makeMachine (qs, as, al "%", completeFun qc al (snd t), qc "0", qc "acc", qc "re") 
  where t = tableToMap qc al s
        qs = nub $ sort $ ((map fst $ M.keys (snd t)) ++ (map (\(x,_,_) -> x) $ M.elems $ snd t))
        as = nub $ sort $ (fst t ++ (map (\(_,x,_) -> x) $ M.elems $ snd t))

fromTuringTable = fromTuringTable' Q Alph



usingTable h t as = h (fromTuringTable t) (map Alph as)
