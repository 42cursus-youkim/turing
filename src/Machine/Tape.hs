module Machine.Tape
  ( Tape (..),
    pfTape,
    pfTapeLong,
    initTape,
  )
where

import PyF (fmt, fmtTrim)
import System.Console.Pretty (Color (Red), Pretty (..), Style (Bold))

data Tape = Tape
  { left :: [Char],
    cursor :: Char,
    right :: [Char]
  }

ann :: String -> String
ann = color Red . style Bold

pfTape :: Tape -> String
pfTape (Tape l h r) = [fmt|{l}{ann [h]}{r}|]

pfTapeLong :: Tape -> String
pfTapeLong t =
  [fmtTrim|{pfTape t}
  {ann "^":<{length (left t)}}|]

initTape :: String -> Tape
initTape s = Tape [] (head s) (tail s)