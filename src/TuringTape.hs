module TuringTape (Tape (..), tapeFromList, shift, readAtHead, writeAtHead, Direction (..), blankChar, leftOfHead, headOfTape, rightOfHead, tapeToList, whileNotBlank) where

import Data.List
import Control.DeepSeq

data Tape a = Tape [a] [a] a -- Left of head (reversed), head and right of head, blank symbol

leftOfHead (Tape xs _ _) = reverse xs
headOfTape (Tape _ (x:_) _) = x
rightOfHead (Tape _ (_:xs) _) = xs
tapeToList t = leftOfHead t ++ (headOfTape t : rightOfHead t)
blankChar (Tape _ _ x) = x -- To know what was used as the blank character on the tape

whileNotBlank t = leftOfHead t ++ (takeWhile (/= (blankChar t)) (headOfTape t : rightOfHead t))

instance (Show a, Eq a) => Show (Tape a) where
  show t = show (leftOfHead t) ++ show [headOfTape t] ++ (show (takeWhile (/= (blankChar t)) (rightOfHead t)))


tapeFromList x xs = (Tape [] (xs ++ repeat x) x) -- x is the blank symbol. Head on first element.

writeAtHead :: a -> Tape a -> Tape a
writeAtHead x (Tape xs (y:ys) c) = Tape xs (x:ys) c

readAtHead (Tape _ (y:ys) _) = y

data Direction = L | R deriving (Show)

shift R (Tape xs (y:ys) c) = Tape (y:xs) ys c
shift L t@(Tape [] _ _) = t
shift L (Tape (x:xs) ys c) = Tape xs (x:ys) c


-- NFData
instance (NFData a, Eq a) => NFData (Tape a) where
  rnf (Tape xs ys b) = rnf xs `deepseq` rnf (takeWhile (/=b) ys) `deepseq` ()
