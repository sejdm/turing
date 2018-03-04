module Turing (Turing, everything, makeMachine, turingToBeamer, turingExecute, turingInteractive) where

import System.IO
import Control.DeepSeq
import System.Process
import Latexify
import TuringTape


-- Turing (states, alphabet, blank symbol, transition function, initial state, accept state, reject state)
newtype Turing q a = Turing ([q], [a], a, TuringFunction q a, q, q, q)
makeMachine = Turing

everything :: (Bounded a, Enum a) => [a]
everything = [ minBound .. maxBound ]

turingToBeamer m n (Turing (qs, as, x, f, q', ac, re)) t = makeBeamer (m, n) qs as f ac re (q', tapeFromList x t)

turingToSnapshots (Turing (_, _, x, f, q', ac, re)) t = turingList f ac re (q', tapeFromList x t)

turingExecute (Turing (_, _, x, f, q', ac, re)) t = runTuring f ac re (q', tapeFromList x t)

turingInteractive m = interactiveShowSnaps . turingToSnapshots m




-- Helper functions
type TuringFunction state alph = state -> alph -> (state, alph, Direction) 
type Snapshot state alph = (state, Tape alph)

turingList :: Eq q => TuringFunction q a -> q -> q -> Snapshot q a -> [Snapshot q a]
turingList f ac re = (\(as, bs) -> as ++ [head bs])
  . span (\(q, _) -> q /= ac && q /= re)
  . iterate (toTuring f)
    where toTuring f (q, t) = let (q', x, d) = f q (readAtHead t) in
            (q', shift d $ writeAtHead x t)



-- To evaluate the last element of a list of snapshots without a space leak
deepLast [x] = x
deepLast (x:xs) = x `deepseq` deepLast xs


-- More efficient than evaluating the last element of a list of snapshots
runTuring :: Eq q => TuringFunction q a -> q -> q -> (q, Tape a) -> (q, Tape a)
runTuring f ac re a@(q, t) | q == ac || q == re = a
                           | otherwise = let (q', c, dir) = f q (readAtHead t) in
                                      runTuring f ac re (q', shift dir $ writeAtHead c t)



interactiveShowSnaps [] = return ()
interactiveShowSnaps (x:xs) = system "clear" >> print x
           >> hSetBuffering stdin NoBuffering
           >> getChar >> interactiveShowSnaps xs


-- Beamerize

showFrame (m, n) qs as f ac re (q', tape) = "\n\\begin{frame}\n" ++ "\n{\\rm \\bf Current State: }"
  ++ show q' ++ "\\\\\n{\\rm \\bf Character under head: }" ++ show y ++ "\\\\{\\rm \\bf Current Status: }" ++  (if q' == ac  then "\\textcolor{green}{Halted (accepted)}" else (if q' == re then alert "Halted (rejected)" else "Computing")) ++ "\n\\\\\\vspace{0.5cm}"
  ++ (drawBinaryTable "" (filter (/=re) $ filter (/=ac) qs) as f (q', y)) ++ "\n\\vspace{0.5cm}\n\\\\~\\\\" ++ thetape ++ "\n\\end{frame}"
  where 
        thetape = listToTable "\\tt\\footnotesize" m n (map showB (xs) ++ ((if y == b then alert "-" else alert (showB y)) : map showB ys))
        showB x = if x == b then " " else show x
        y = headOfTape tape
        ys = rightOfHead tape
        xs = leftOfHead tape
        b = blankChar tape



makeBeamer (m, n) qs as f ac re p = do
  putStr "\\documentclass{beamer}\n \\usepackage{array}\n \\newcolumntype{C}{>{\\centering\\arraybackslash}p{0.3em}}\n\\begin{document}\\tt\n"
  mapM_ (putStrLn . showFrame (m, n) qs as f ac re) $ take (m*n) $ turingList f ac re p

  putStr "\n\\end{document}\n"
