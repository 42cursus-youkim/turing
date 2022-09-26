module Machine.Tape
  ( Tape (..),
    pfTape,
    pfTapeLong,
    initTape,
    moveTape,
    writeTape,
  )
where

import Model.Action (Direction (..))
import PyF (fmt, fmtTrim)
import System.Console.Pretty (Color (..), Pretty (..), Style (Bold))

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
moveLeft (Tape l c r b) = Tape (init l) (last l) (c : r) b

moveRight :: Tape -> Tape
moveRight (Tape l c [] b) = Tape (l ++ [c]) b [] b
moveRight (Tape l c r b) = Tape (l ++ [c]) (head r) (tail r) b

moveTape :: Tape -> Direction -> Tape
moveTape t d = case d of
  ToLeft -> moveLeft t
  ToRight -> moveRight t

writeTape :: Tape -> Char -> Tape
writeTape (Tape l _ r b) c = Tape l c r b
