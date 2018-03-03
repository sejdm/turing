module Turing (tapeFromList, shift, readAtHead, writeAtHead, Direction (..), blankChar, turingList, runTuring, makeBeamer, interactiveTuring) where

import Data.List
import Control.DeepSeq
import System.IO
import System.Process
import System.Environment

data Tape a = Tape [a] [a] a -- Left of head (reversed), head and right of head, blank symbol

instance (Show a, Eq a) => Show (Tape a) where
  show (Tape xs (y:ys) b) = show (reverse xs) ++ show [y] ++ (show (takeWhile (/=b) ys))


tapeFromList x xs = (Tape [] (xs ++ repeat x) x) -- x is the blank symbol. Head on first element.

writeAtHead :: a -> Tape a -> Tape a
writeAtHead x (Tape xs (y:ys) c) = Tape xs (x:ys) c

readAtHead (Tape _ (y:ys) _) = y

data Direction = L | R deriving (Show)

shift R (Tape xs (y:ys) c) = Tape (y:xs) ys c
shift L t@(Tape [] _ _) = t
shift L (Tape (x:xs) ys c) = Tape xs (x:ys) c


type TuringFunction state alph = state -> alph -> (state, alph, Direction) 
type Snapshot state alph = (state, Tape alph)

turingList :: Eq q => TuringFunction q a -> q -> q -> Snapshot q a -> [Snapshot q a]
turingList f ac re = (\(as, bs) -> as ++ [head bs])
  . span (\(q, _) -> q /= ac && q /= re)
  . iterate (toTuring f)
    where toTuring f (q, t) = let (q', x, d) = f q (readAtHead t) in
            (q', shift d $ writeAtHead x t)




-- NFData
instance (NFData a, Eq a) => NFData (Tape a) where
  rnf (Tape xs ys b) = rnf xs `deepseq` rnf (takeWhile (/=b) ys) `deepseq` ()


deepLast [x] = x
deepLast (x:xs) = x `deepseq` deepLast xs


runTuring :: Eq q => TuringFunction q a -> q -> q -> (q, Tape a) -> (q, Tape a)
runTuring f ac re a@(q, t) | q == ac || q == re = a
                           | otherwise = let (q', c, dir) = f q (readAtHead t) in
                                      runTuring f ac re (q', shift dir $ writeAtHead c t)


interactiveTuring [] = return ()
interactiveTuring (x:xs) = system "clear" >> print x
           >> hSetBuffering stdin NoBuffering
           >> getChar >> interactiveTuring xs

blankChar (Tape _ _ x) = x -- To know what was used as the blank character on the tape



-- Beamerize

showFrame (m, n) qs as f ac re (q', Tape xs (y:ys) b) = "\n\\begin{frame}\n" ++ "\n{\\rm \\bf Current State: }"
  ++ show q' ++ "\\\\\n{\\rm \\bf Character under head: }" ++ show y ++ "\\\\{\\rm \\bf Current Status: }" ++  (if q' == ac  then "\\textcolor{green}{Halted (accepted)}" else (if q' == re then alert "Halted (rejected)" else "Computing")) ++ "\n\\\\\\vspace{0.5cm}"
  ++ (drawBinaryTable (filter (/=re) $ filter (/=ac) qs) as f (q', y)) ++ "\n\\vspace{0.5cm}\n\\\\~\\\\" ++ thetape ++ "\n\\end{frame}"
  where 
        thetape = listToTable m n (map showB (reverse xs) ++ ((if y == b then alert "-" else alert (showB y)) : map showB ys))
        showB x = if x == b then " " else show x



listToTable m n xs = "{\\tt\\footnotesize\\begin{tabular}{|" ++ intersperse '|' (replicate n 'C') ++ "|}\n\\hline\n " ++ (concat $ intersperse "\\\\\n\\hline \\hline\n" $ take m $ map (concat . intersperse " & ") $ chunksOf n xs) ++ "\\\\\\hline\n\n\\end{tabular}}"

chunksOf n = takeWhile (not.null) . unfoldr (Just . splitAt n)

makeBeamer (m, n) qs as f ac re p = do
  putStr "\\documentclass{beamer}\n \\usepackage{array}\n \\newcolumntype{C}{>{\\centering\\arraybackslash}p{0.3em}}\n\\begin{document}\\tt\n"
  mapM_ (putStrLn . showFrame (m, n) qs as f ac re) $ take (m*n) $ turingList f ac re p

  putStr "\n\\end{document}\n"

alert xs = "\\textcolor{red}{"++xs++"}"


drawBinaryTable xs ys f (x',y') = "\\begin{tabular}{c|" ++ replicate (length ys) 'c' ++ "}\n" ++ (" & " ++ (concat $ intersperse " & " $ map show ys)) ++ "\\\\\n\\hline\n" ++ (concat $ intersperse "\\\\\n" [show x ++ " & " ++ concat (intersperse "&" [if (x,y)==(x',y') then alert (show $ f x y) else show (f x y)  | y <- ys]) | x <- xs]) ++ "\n\\end{tabular}"
