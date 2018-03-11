module Latexify (listToTable, drawBinaryTable, alert) where

import Data.List.Split
import Data.List

listToTable f m n xs = "{" ++ f ++ "\\begin{tabular}{|" ++ intersperse '|' (replicate n 'C') ++ "|}\n\\hline\n " ++ (concat $ intersperse "\\\\\n\\hline \\hline\n" $ take m $ map (concat . intersperse " & ") $ chunksOf n xs) ++ "\\\\\\hline\n\n\\end{tabular}}"


alert xs = "\\textcolor{red}{"++xs++"}"


drawBinaryTable nm sfx sfy sff fo xs ys f (x',y') = "{\\hspace*{-1cm}" ++ fo ++ "\\begin{tabular}{c|" ++ replicate (length ys) 'c' ++ "|}\n" ++ (nm ++ " & " ++ (concat $ intersperse " & " $ map (\y -> if y == y' then (alert . sfy) y else sfy y) ys)) ++ "\\\\\n\\hline\n" ++ (concat $ intersperse "\\\\\n" [(if x /= x' then (sfx x) else (alert (sfx x))) ++ " & " ++ concat (intersperse "&" [if (x,y)==(x',y') then alert (sff $ f x y) else sff (f x y)  | y <- ys]) | x <- xs]) ++ "\n\\end{tabular}}"
