module TuringTape (Tape, tapeFromList, shiftTape, readAtHead, writeAtHead, Direction (..), blankChar, leftOfHead, headOfTape, rightOfHead, tapeToList, whileNotBlank) where

import Data.List
import Control.DeepSeq
import ToLatex

data Tape a = Tape [a] [a] a -- Left of head (reversed), head and right of head, blank symbol

leftOfHead (Tape xs _ _) = reverse xs
headOfTape (Tape _ (x:_) _) = x
rightOfHead (Tape _ (_:xs) _) = xs
tapeToList t = leftOfHead t ++ (headOfTape t : rightOfHead t)
blankChar (Tape _ _ x) = x -- To know what was used as the blank character on the tape
isBlankChar t = blankChar t == headOfTape t

whileNotBlank t = leftOfHead t ++ (takeWhile (/= (blankChar t)) (headOfTape t : rightOfHead t))

instance (Show a, Eq a) => Show (Tape a) where
  show t = concat $ "|" : (intersperse "|" $ map (\x -> " " ++ show x ++ " ") (leftOfHead t) ++ ["[" ++ if isBlankChar t then " " else show (headOfTape t) ++ "]"] ++ map (\x -> " " ++ show x ++ " ") (takeWhile (/= blankChar t) $ rightOfHead t)) ++ ["|"]


tapeFromList x xs = (Tape [] (xs ++ repeat x) x) -- x is the blank symbol. Head on first element.

writeAtHead :: a -> Tape a -> Tape a
writeAtHead x (Tape xs (y:ys) c) = Tape xs (x:ys) c

readAtHead (Tape _ (y:ys) _) = y

data Direction = L | R deriving (Show, Read)
instance ToLatex Direction where
  toLatex L = "$\\mathcal{L}$"
  toLatex R = "$\\mathcal{R}$"

shiftTape R (Tape xs (y:ys) c) = Tape (y:xs) ys c
shiftTape L t@(Tape [] _ _) = t
shiftTape L (Tape (x:xs) ys c) = Tape xs (x:ys) c


-- NFData
instance (NFData a, Eq a) => NFData (Tape a) where
  rnf (Tape xs ys b) = b `seq` rnf xs `deepseq` rnf (takeWhile (/=b) ys) `deepseq` ()
