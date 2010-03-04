
module Yadorigi.SemanticAnalysis.Test where

import Yadorigi.Parser.DataTypes
import Yadorigi.Parser.Parser
import Yadorigi.Parser.Tokenizer
import Yadorigi.SemanticAnalysis.ScopeResolution

import Text.Parsec
import Control.Monad

-- Tester

main :: IO ()
main = do
    contents <- getContents
    case runParser tokenizer () "<interactive>" contents of
        (Right ts) ->
            case runParser moduleParser () "<tokenStream>" ts of
                (Right result) -> do
                    print result
                    putStrLn "bindScopeName"
                    print $ bindScopeNameToModule result
                (Left error) -> print error
        (Left error) -> print error

