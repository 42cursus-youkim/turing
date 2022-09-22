{-# LANGUAGE OverloadedStrings #-}

module Util
  ( stripL,
    stripR,
    termWidth,
    boldCol,
    header,
  )
where

import Data.Char ( isSpace, toUpper )
import Data.Function (on)
import Data.List ( groupBy )
import PyF (fmtTrim)
import System.Console.Pretty (Color (..), Style (..), color, style)
import System.Console.Terminal.Size (Window (width), size)

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

slugify :: String -> String
slugify = map (\c -> if c == '_' then ' ' else c)

header :: String -> IO ()
header name =
  do
    w <- termWidth
    let line = replicate w '*'
        slugged = capitalize . slugify $ name
    putStrLn
      [fmtTrim|
      {line}
      *{slugged:^{w - 2}}*
      {line}|]
