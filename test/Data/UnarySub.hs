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
      blank = '.',
      states = ["scanright", "eraseone", "subone", "skip", "HALT"],
      initial = "scanright",
      finals = ["HALT"],
      transitions =
        M.fromList
          [ ( "scanright",
              [ Action {read_ = '.', to_state = "scanright", write = '.', action = ToRight},
                Action {read_ = '1', to_state = "scanright", write = '1', action = ToRight},
                Action {read_ = '-', to_state = "scanright", write = '-', action = ToRight},
                Action {read_ = '=', to_state = "eraseone", write = '.', action = ToLeft}
              ]
            ),
            ( "eraseone",
              [ Action {read_ = '1', to_state = "subone", write = '=', action = ToLeft},
                Action {read_ = '-', to_state = "HALT", write = '.', action = ToLeft}
              ]
            ),
            ( "subone",
              [ Action {read_ = '1', to_state = "subone", write = '1', action = ToLeft},
                Action {read_ = '-', to_state = "skip", write = '-', action = ToLeft}
              ]
            ),
            ( "skip",
              [ Action {read_ = '.', to_state = "skip", write = '.', action = ToLeft},
                Action {read_ = '1', to_state = "scanright", write = '.', action = ToRight}
              ]
            )
          ]
    }
