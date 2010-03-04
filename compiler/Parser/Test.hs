
module Yadorigi.Parser.Test where

import Yadorigi.Syntax
import Yadorigi.Parser.Parser
import Yadorigi.Parser.Tokenizer

import Text.Parsec
import Control.Monad

-- Parser Tester

main :: IO ()
main = do contents <- getContents
          let tokenizerResult = runParser tokenizer () "<interactive>" contents
          case tokenizerResult of
              (Right ts) ->
                  do sequence_ (map print ts)
                     let parserResult = runParser moduleParser () "<tokenStream>" ts
                     case parserResult of
                         (Right result) -> print result
                         (Left error) -> print error
              (Left error) -> print error

