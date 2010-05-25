
module Yadorigi.Parser.Test where

import Yadorigi.Syntax
import Yadorigi.Parser.Parser
import Yadorigi.Parser.Tokenizer

import Text.Parsec
import Control.Monad

-- Parser Tester

main :: IO ()
main = do
    contents <- getContents
    let tokenizerResult = runParser tokenizer () "<interactive>" contents
    case tokenizerResult of
        (Right ts) -> do
            mapM print ts
            either print print (runParser moduleParser () "<tokenStream>" ts)
        (Left error) -> print error

