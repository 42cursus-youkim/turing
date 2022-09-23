{-# LANGUAGE OverloadedStrings #-}

module Util
  ( stripR,
    termWidth,
    boldCol,
    capitalize,
    slugify,
    indent,
    putStrIndent,
    note,
  )
where

import Data.Char (isSpace, toUpper)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (groupBy)
import Data.Maybe (fromMaybe)
import PyF (fmtTrim)
import System.Console.Pretty (Color (..), Style (..), color, style)
import System.Console.Terminal.Size (Window (width), size)

stripL :: Char -> String -> String
stripL x = dropWhile (== x)

stripR :: Char -> String -> String
stripR x = reverse . stripL x . reverse

termWidth :: IO Int
termWidth = size <&> maybe 80 width

boldCol :: Color -> String -> String
boldCol c = color c . style Bold

capitalize :: String -> String
capitalize = concatMap (\(c : cs) -> toUpper c : cs) . words

slugify :: String -> String
slugify = map (\c -> if c == '_' then ' ' else c)

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

putStrIndent :: String -> IO ()
putStrIndent = putStr . indent 2

note :: a -> Maybe b -> Either a b
note x = maybe (Left x) Right
