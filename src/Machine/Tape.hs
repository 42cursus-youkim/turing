module Machine.Tape
  ( Tape (..),
    pfTape,
    initTape,
    moveTape,
    writeTape,
  )
where

import Model.Action (Direction (..))
import PyF (fmt, fmtTrim)
import System.Console.Pretty (Color (..), Pretty (..), Style (Bold))
import Util (boldCol)

data Tape = Tape
  { left :: [Char],
    cursor :: Char,
    right :: [Char],
    blank :: Char
  }
  deriving (Show)

pfTape :: Tape -> String
pfTape (Tape l h r _) = [fmt|[{l}{boldCol Red [h]}{r}]|]

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
