module Latexify (listToTable, drawBinaryTable, alert) where

import Data.List.Split
import Data.List

listToTable f m n xs = "{" ++ f ++ "\\begin{tabular}{|" ++ intersperse '|' (replicate n 'C') ++ "|}\n\\hline\n " ++ (concat $ intersperse "\\\\\n\\hline \\hline\n" $ take m $ map (concat . intersperse " & ") $ chunksOf n xs) ++ "\\\\\\hline\n\n\\end{tabular}}"


alert xs = "\\textcolor{red}{"++xs++"}"


drawBinaryTable fo xs ys f (x',y') = "{" ++ fo ++ "\\begin{tabular}{c|" ++ replicate (length ys) 'c' ++ "}\n" ++ (" & " ++ (concat $ intersperse " & " $ map show ys)) ++ "\\\\\n\\hline\n" ++ (concat $ intersperse "\\\\\n" [show x ++ " & " ++ concat (intersperse "&" [if (x,y)==(x',y') then alert (show $ f x y) else show (f x y)  | y <- ys]) | x <- xs]) ++ "\n\\end{tabular}}"
