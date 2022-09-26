module Machine.Tape
  ( Tape (..),
    pfTape,
    pfTapeLong,
    initTape,
    moveTape,
  )
where

import Model.Action (Direction (..))
import PyF (fmt, fmtTrim)
import System.Console.Pretty (Color (Red), Pretty (..), Style (Bold))

data Tape = Tape
  { left :: [Char],
    cursor :: Char,
    right :: [Char],
    blank :: Char
  }

ann :: String -> String
ann = color Red . style Bold

pfTape :: Tape -> String
pfTape (Tape l h r _) = [fmt|[{l}{ann [h]}{r}]|]

pfTapeLong :: Tape -> String
pfTapeLong t =
  [fmtTrim|{pfTape t}
  {ann "^":<{length (left t)}}|]

initTape :: String -> Char -> Tape
initTape s = Tape [] (head s) (tail s)

moveLeft :: Tape -> Tape
moveLeft (Tape [] c r b) = Tape [] b (c : r) b
moveLeft (Tape (l : ls) c r b) = Tape ls l (c : r) b

moveRight :: Tape -> Tape
moveRight (Tape l c [] b) = Tape (c : l) b [] b
moveRight (Tape l c (r : rs) b) = Tape (c : l) r rs b

moveTape :: Tape -> Direction -> Tape
moveTape t d = case d of
  ToLeft -> moveLeft t
  ToRight -> moveRight t
