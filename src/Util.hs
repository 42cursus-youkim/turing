{-# LANGUAGE OverloadedStrings #-}

module Util
  ( stripL,
    stripR,
    termWidth,
    boldCol,
    header,
    unindent,
  )
where

import Data.Char
import Data.Function (on)
import Data.List
import Data.Maybe (fromMaybe)
import PyF (fmt)
import System.Console.Pretty (Color (..), Style (..), color, style)
import System.Console.Terminal.Size

stripL :: Char -> String -> String
stripL x = dropWhile (== x)

stripR :: Char -> String -> String
stripR x = reverse . stripL x . reverse

termWidth :: IO Int
termWidth =
  size >>= \case
    Nothing -> return 80
    Just s -> return $ width s

boldCol :: Color -> String -> String
boldCol c = color c . style Bold

capitalize :: String -> String
capitalize =
  concatMap (\(c : cs) -> toUpper c : cs) . groupBy ((==) `on` isSpace)

unindent :: String -> String
unindent t = unindentBy n t
  where
    n = length . takeWhile (== ' ') . head $ lines t

unindentBy :: Int -> String -> String
unindentBy n t = unlines $ map f $ lines t
  where
    toStrip = replicate n ' '
    f s = fromMaybe s (stripPrefix toStrip s)

slugify :: String -> String
slugify = map (\c -> if c == '_' then ' ' else c)

header :: String -> IO ()
header name =
  do
    w <- termWidth
    let line = replicate w '*'
        slugged = capitalize . slugify $ name
    putStrLn [fmt|{line}\n*{slugged:^{w - 2}}*\n{line}|]
