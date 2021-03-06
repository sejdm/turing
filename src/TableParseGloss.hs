{-# LANGUAGE NoMonomorphismRestriction #-}
module TableParseGloss (fromTuringTableGloss, alph) where
import qualified Data.Map as M
import Data.List.Split
import TuringTape
import Turing
import Data.List


--makeMap :: Ord a => [a] -> [[a]] -> M.Map (a, a) a
makeMap h (xs) = (map rmSpc h, M.fromList $ mkF (map rmSpc h) xs)
  where
    mkF h [] = [] 
    mkF h ((c:cs):rs) = zip (zip (repeat (rmSpc c)) h) cs ++ mkF h rs

    rmSpc = filter (/=' ')

mapToBinFun = curry . flip M.lookup


toTuple s = let x:y:z:_ = wordsBy (\x -> x == ' ' || x == ',') s in (Q x, Alph y, read z :: Direction)

toTuple' s = let x:y:z:_ = wordsBy (\x -> x == ' ' || x == ',') s in (Q x, y, Alph z)


tableToMap' s = (M.mapKeys (\(q, a) -> (Q q, Alph a)) . M.map toTuple . snd . makeMap (tail $ wordsBy (\x -> x == ' ' || x == '|') h) . map (wordsBy (=='|'))) xs
    where
      (h:xs) = lines s


tableToMap s = (map Alph (fst ys), M.mapKeys (\(q, a) -> (Q q, Alph a)) $ M.map toTuple $ snd ys)
    where ys = makeMap (tail $ wordsBy (\x -> x == ' ' || x == '|') h) $ map (wordsBy (=='|')) xs
          (h:xs) = lines s

completeFun = curry . flip (M.findWithDefault (Q "acc", Alph "%", L))

fromTuringTableGloss s = makeMachine (qs, as, Alph "%", completeFun (snd t), Q "0", Q "acc", Q "re") 
  where t = tableToMap s
        qs = nub $ sort $ ((map fst $ M.keys (snd t)) ++ (map (\(x,_,_) -> x) $ M.elems $ snd t))
        as = nub $ sort $ (fst t ++ (map (\(_,x,_) -> x) $ M.elems $ snd t))
