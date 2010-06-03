
module Yadorigi.Typing.Test where

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
import Yadorigi.Typing.KindInference

-- Tester

parsing :: String -> String -> Either ParseError Module
parsing filename contents =
    runParser tokenizer () filename contents >>= runParser moduleParser () filename

main :: IO ()
main = do
    files <- getArgs
    parsedData <- sequence <$> map (uncurry parsing) <$> zip files <$> mapM readFile files
    case parsedData of
        (Left error) -> print error
        (Right result) -> case referModule $ map (bindScope'' undefined) result of
            (Left error) -> print error
            (Right result) -> case mapM nameResolutionModule result of
                (Left error) -> print error
                (Right result) -> case kindInfModules result of
                    (Left error) -> print error
                    (Right result) -> mapM_ print result

