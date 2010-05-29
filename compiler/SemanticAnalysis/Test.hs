
module Yadorigi.SemanticAnalysis.Test where

import Data.Functor
import Data.Tuple.All
import Control.Monad
import Text.Parsec

import System.Environment

import Yadorigi.Monad.Either
import Yadorigi.Syntax
import Yadorigi.Parser.Parser
import Yadorigi.Parser.Tokenizer
import Yadorigi.SemanticAnalysis.BindScope
import Yadorigi.SemanticAnalysis.ReferModule
import Yadorigi.SemanticAnalysis.NameResolution

-- Tester

parsing :: String -> String -> Either ParseError Module
parsing filename contents =
    runParser tokenizer () filename contents >>= runParser moduleParser () filename

main :: IO ()
main = do
    files <- getArgs
    parsedData <- sequence <$> map (uncurry parsing) <$> zip files <$> mapM readFile files
    case parsedData of
        (Right result) -> case referModule $ map (bindScope'' undefined) result of
            (Right result) -> mapM_ (print.nameResolutionModule) result
            (Left error) -> print error
        (Left error) -> print error

