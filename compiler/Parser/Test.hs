
module Yadorigi.Parser.Test where

import Control.Monad
import Text.Parsec

import Yadorigi.Syntax
import Yadorigi.Parser.Parser
import Yadorigi.Parser.Tokenizer

-- Parser Tester

main :: IO ()
main = do
    contents <- getContents
    let tokenizerResult = runParser tokenizer () "<interactive>" contents
    case tokenizerResult of
        (Right ts) -> do
            mapM_ print ts
            either print print (runParser moduleParser () "<tokenStream>" ts)
        (Left error) -> print error

