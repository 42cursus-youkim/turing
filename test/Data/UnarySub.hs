module Data.UnarySub
  ( unarySub,
    getUnarySubJSON,
  )
where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Model.Action
import Model.Program

testFile :: FilePath
testFile = "docs/examples/unary_sub.json"

getUnarySubJSON :: IO B.ByteString
getUnarySubJSON = B.readFile testFile

unarySub :: Program
unarySub =
  Program
    { name = "unary_sub",
      alphabet = ["1", ".", "-", "="],
      blank = ".",
      states = ["scanright", "eraseone", "subone", "skip", "HALT"],
      initial = "scanright",
      finals = ["HALT"],
      transitions =
        M.fromList
          [ ( "scanright",
              [ Action {read_ = ".", to_state = "scanright", write = ".", action = "RIGHT"},
                Action {read_ = "1", to_state = "scanright", write = "1", action = "RIGHT"},
                Action {read_ = "-", to_state = "scanright", write = "-", action = "RIGHT"},
                Action {read_ = "=", to_state = "eraseone", write = ".", action = "LEFT"}
              ]
            ),
            ( "eraseone",
              [ Action {read_ = "1", to_state = "subone", write = "=", action = "LEFT"},
                Action {read_ = "-", to_state = "HALT", write = ".", action = "LEFT"}
              ]
            ),
            ( "subone",
              [ Action {read_ = "1", to_state = "subone", write = "1", action = "LEFT"},
                Action {read_ = "-", to_state = "skip", write = "-", action = "LEFT"}
              ]
            ),
            ( "skip",
              [ Action {read_ = ".", to_state = "skip", write = ".", action = "LEFT"},
                Action {read_ = "1", to_state = "scanright", write = ".", action = "RIGHT"}
              ]
            )
          ]
    }
