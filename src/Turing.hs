{-# LANGUAGE NoMonomorphismRestriction #-}
module Turing (Turing, everything, makeMachine, turingToBeamer, turingExecute, turingInteractive, turingAnimate) where

import System.IO
import System.Process
import Control.DeepSeq
import Latexify
import ToLatex
import TuringTape
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Data.List.Split


-- Turing (states, alphabet, blank symbol, transition function, initial state, accept state, reject state)
newtype Turing q a = Turing ([q], [a], a, TuringFunction q a, q, q, q)
makeMachine = Turing

everything :: (Bounded a, Enum a) => [a]
everything = [ minBound .. maxBound ]

turingToBeamer nm m n (Turing (qs, as, x, f, q', ac, re)) t = makeBeamer nm (m, n) qs as f ac re (q', tapeFromList x t)

turingToSnapshots (Turing (_, _, x, f, q', ac, re)) t = turingList f ac re (q', tapeFromList x t)

turingExecute (Turing (_, _, x, f, q', ac, re)) t = runTuring f ac re (q', tapeFromList x t)

turingInteractive m@(Turing (qs, as, _, f, q', ac, re)) = interactiveShowSnaps (uncurry f) [(q,a) | q <- qs, a <- as] . listToWorld . turingToSnapshots m






-- Helper functions
type TuringFunction state alph = state -> alph -> (state, alph, Direction) 
type Snapshot state alph = (state, Tape alph)

turingList :: Eq q => TuringFunction q a -> q -> q -> Snapshot q a -> [Snapshot q a]
turingList f ac re = (\(as, bs) -> as ++ [head bs])
  . span (\(q, _) -> q /= ac && q /= re)
  . iterate (toTuring f)
    where toTuring f (q, t) = let (q', x, d) = f q (readAtHead t) in
            (q', shiftTape d $ writeAtHead x t)



-- To evaluate the last element of a list of snapshots without a space leak
deepLast [x] = x
deepLast (x:xs) = x `deepseq` deepLast xs


-- More efficient than evaluating the last element of a list of snapshots
runTuring :: Eq q => TuringFunction q a -> q -> q -> (q, Tape a) -> (q, Tape a)
runTuring f ac re a@(q, t) | q == ac || q == re = a
                           | otherwise = let (q', c, dir) = f q (readAtHead t) in
                                      runTuring f ac re (q', shiftTape dir $ writeAtHead c t)


showDelta f x = show x ++ " -> " ++ show (f x)

interactiveShowSnaps _ _ (World [] []) = return ()
interactiveShowSnaps f c w = system "clear" >> mapM_ (putStrLn . showDelta f) c >> putStrLn "">> putStrLn ("(" ++ show (fst x) ++ ", " ++ show a ++ ") -> " ++ show (curry f (fst x) a) ) >>  putStrLn "">> print (snd x)
           >> hSetBuffering stdin NoBuffering
           >> getChar >>= (\h -> interactiveShowSnaps f c (case h of 'n' -> next w; 'p' -> previous w; _ -> w))
  where a = (headOfTape $ snd x)
        x = current w


-- Beamerize

showFrame nm i (m, n) qs as f ac re (q', tape) = "\n\\begin{frame}\n"
  ++ (if q' == ac  then "\\textcolor{Green}{Halted (accepted)}" else (if q' == re then alert "Halted (rejected)" else "Computing $\\ldots$")) ++ "\n({\\it Accept:} " ++ toLatex ac ++ "\n,\\ \\ {\\it Reject:} " ++ toLatex re ++ ")\n\\\\\\vspace{0.5cm}{\\rowcolors{1}{}{}"
  ++ (drawBinaryTable ("{\\rm \\bf " ++ nm ++ "}") (blueIf i) toLatex toLatex "}\\footnotesize " (filter (/=re) $ filter (/=ac) qs) as f (q', y)) ++ "\n\\vspace{0.5cm}\n\\\\~\\\\" ++ "{\\rowcolors{0}{Dandelion}{Dandelion}\\footnotesize" ++ thetape ++ "}" ++ "\n\\end{frame}"
  where 
        thetape = listToTable "\\hspace*{-1cm}\\footnotesize" m n (map toLatex (xs) ++ (headAlert (toLatex y) : map toLatex ys)) ++ "$\\   \\cdots$"
        y = headOfTape tape
        ys = rightOfHead tape
        xs = leftOfHead tape
        b = blankChar tape
        headAlert = (\x -> "\\cellcolor{SpringGreen}{" ++ x ++ "}") . alert

blueIf i x | x == i = "\\hspace*{-0.5cm}$\\rightarrow$" ++ toLatex x ++ ""
           | otherwise = toLatex x

--showB b x = if x == b then "\\textvisiblespace" else toLatex x


--texShow b (q, a, d) = "("++ show q ++ "," ++ showB b a ++ "," ++ show d ++ ")"
--texShow _ = show


makeBeamer nm (m, n) qs as f ac re p = do
  putStr "\\documentclass[xcolor=table,usenames,dvipsnames]{beamer}\n \\usepackage{array}\\usepackage{tabularx}\n \\newcolumntype{C}{>{\\centering\\arraybackslash}p{0.3em}}\n\\begin{document}\\tt\n"
  mapM_ (putStrLn . showFrame nm (fst p) (m, n) qs as f ac re) $ take (m*n + 130) $ turingList f ac re p

  putStr "\n\\end{document}\n"


-- Glossify

data World a = World [a] [a]
listToWorld xs = World [] xs
  
current (World _ (x:_)) = x
current (World (x:_) _) = x

next a@(World _ []) = a
next (World xs (y:ys)) = World (y:xs) ys

previous a@(World [] _) = a
previous (World (x:xs) ys) = World xs (x:ys)

--glossSnapImage = text . show . headOfTape . snd
glossLines t = pictures $  zipWith (\n x -> translate 0 (-(60*n)) (scale 0.45 0.45 x)) [0..]
   $ map (pictures . zipWith (\n x -> translate (900*n) 0 x) [0..] . map text . wordsBy (\y -> y =='|' || y == '+' || y == '-')) (lines t)
  
glossSnapImage t' b f (q, t) = pictures $
  translate (-670) 350 (scale 0.25 0.25 (glossLines t')) :
  (translate (-100) 300 (scale 0.25 0.25 $ text $ "Current State: " ++  acceptOrReject q)) :
  (translate (-100) (200) (scale 0.25 0.25 $ text $ showT b $ f q (headOfTape t))) :
  zipWith (\n x -> translate (25*n - 650) 0 (scale 0.25 0.25 x)) [0..] (take 80 $ map (inCell b) (leftOfHead t) ++ (color red (inCell b $ headOfTape t): (map (inCell b) (rightOfHead t) )))

acceptOrReject q = case s of
  "acc" -> "Accepted"
  "re" -> "Rejected"
  _ -> s
  where s = show q

showT bl (a, b, c) = "Next change: ( " ++ show a ++ ", " ++ showBl bl b ++ ", " ++ show c ++ " )"

showBl _ x = show x
--showBl b x = if b /= x then show x else ""

glossWorldImage t b f w = glossSnapImage t b f $ current w

glossReact (EventKey (Char 'n') Down alt _) = next
glossReact (EventKey (Char 'p') Down alt _) = previous
glossReact _ = id

turingAnimate t' t@(Turing (_,_,b,f,_,_,_)) xs = play FullScreen white 1 (listToWorld $ turingToSnapshots t xs) (glossWorldImage t' b f) glossReact (const id)

inCell b y = pictures [ rectangleWire 200 100 , scale 0.5 0.5 (translate (-150) (-50) $ text (showBl b y))]



--showFrame nm i (m, n) qs as f ac re (q', tape) = "\n\\begin{frame}\n" ++ "\n{\\rm \\bf State: }"
  -- ++ toLatex q' ++ ", {\\rm \\bf Character: }" ++ toLatex y ++ ", {\\rm \\bf Status: }" ++  (if q' == ac  then "\\textcolor{green}{Halted (accepted)}" else (if q' == re then alert "Halted (rejected)" else "Computing")) ++ "\\\\\n({\\it Accept:} " ++ toLatex ac ++ "\n\\ \\ {\\it Reject:} " ++ toLatex re ++ ")\n\\\\\\vspace{0.5cm}"
  -- ++ (drawBinaryTable ("{\\rm " ++ nm ++ "}") (blueIf i) toLatex toLatex "" (filter (/=re) $ filter (/=ac) qs) as f (q', y)) ++ "\n\\vspace{0.5cm}\n\\\\~\\\\" ++ "{\\scriptsize" ++ thetape ++ "}" ++ "\n\\end{frame}"
  -- where 
        -- thetape = listToTable "\\hspace*{-1cm}\\scriptsize" m n (map toLatex (xs) ++ (alert (toLatex y) : map toLatex ys)) ++ "$\\   \\cdots$"
        -- y = headOfTape tape
        -- ys = rightOfHead tape
        -- xs = leftOfHead tape
        -- b = blankChar tape


