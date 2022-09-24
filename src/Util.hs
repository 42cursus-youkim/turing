module Util (stripL, stripR, termWidth) where

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
