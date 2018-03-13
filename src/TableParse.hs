{-# LANGUAGE NoMonomorphismRestriction #-}
module TableParse (usingTable) where

import qualified Data.Map as M
import Data.List.Split
import TuringTape
import Turing
import Data.List
import ToLatex
import Text.Read

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

fromRow qc al m c [] = m
fromRow qc al m c ((h1,s):xs) = case splitOn "," (filter (\x -> x /= ' ' && x /= ')' && x /= '(') s) of
  "":"":z:_ ->  fromRow qc al (M.insert (c, al h1) (c, al h1, readDir z)  m) c xs
  "":y:z:_ ->  fromRow qc al (M.insert (c, al h1) (c, al y, readDir z)  m) c xs
  x:"":z:_ ->  fromRow qc al (M.insert (c, al h1) (qc x, al h1, readDir z)  m) c xs
  x:y:z:_ -> fromRow qc al (M.insert (c, al h1) (qc x, al y, readDir z :: Direction)  m) c xs
  "acc":_ -> fromRow qc al (M.insert (c, al h1) (qc "acc", al h1, R :: Direction)  m) c xs
  "L":_ ->  fromRow qc al (M.insert (c, al h1) (c, al h1, L :: Direction)  m) c xs
  "R":_ ->  fromRow qc al (M.insert (c, al h1) (c, al h1, R :: Direction)  m) c xs
  _ -> fromRow qc al m c xs
  --_ -> fromRow qc al (M.insert (c, al h1) (qc "re", al "%", R :: Direction)  m) c xs
  --_ -> fromRow qc al m c xs

readDir "L" = L
readDir _ = R

fromR qc al h m (y:ys) = fromRow qc al m (qc y) (zip h ys)

fromT qc al ((_:h):rs) = foldl' (fromR qc al h) M.empty rs

tableToMap' qc al ys = (qc (head $ head $ tail k), map snd (M.keys m), m)
  where m = fromT qc al k
        k = map (split (dropDelims $ dropFinalBlank $ dropInitBlank $ oneOf "|")) $ filter (not . isInfixOf "|-") $ lines $ filter (/=' ') ys
  
toTuple qc al s = case wordsBy (\x -> x == ' ' || x == ',' || x == ')' || x == '(') s of
  x:y:z:_ -> case readMaybe z of
              Just d -> (qc x, al y, d)
              Nothing -> (qc "re", al "%", R)
  _ -> (qc "re", al "%", R)



tableToMap qc al s = (map al (fst ys), M.mapKeys (\(q, a) -> (qc q, al a)) $ M.map (toTuple qc al) $ snd ys)
    where ys = makeMap (tail $ wordsBy (\x -> x == ' ' || x == '|') h) $ map (wordsBy (=='|')) xs
          (h:xs) = lines s


completeFun qc al = curry . flip (M.findWithDefault (qc "re", al "%", L))



fromTuringTable' qc al s = makeMachine (qs, as, al "%", completeFun qc al ms, is, qc "acc", qc "re") 
  where (is, hd, ms) = tableToMap' qc al s
        qs = nub $ sort $ ((map fst $ M.keys ms) ++ (map (\(x,_,_) -> x) $ M.elems ms))
        as = nub $ sort $ (hd ++ (map (\(_,x,_) -> x) $ M.elems ms))

fromTuringTable = fromTuringTable' Q Alph



usingTable h t as = h (fromTuringTable t) (map Alph as)
