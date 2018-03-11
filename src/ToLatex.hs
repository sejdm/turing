module ToLatex (ToLatex (..)) where

class Show a => ToLatex a where
  toLatex :: a -> String
  toLatex = show

instance (ToLatex a, ToLatex b) => ToLatex (a, b) where
  toLatex (a, b) = "(" ++ toLatex a ++ ", " ++ toLatex b ++ ")"


instance (ToLatex a, ToLatex b, ToLatex c) => ToLatex (a, b, c) where
  toLatex (a, b, c) = "(" ++ toLatex a ++ ", " ++ toLatex b ++ ", " ++ toLatex c ++ ")"
