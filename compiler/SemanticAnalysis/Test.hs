
module Yadorigi.SemanticAnalysis.Test where

import Yadorigi.Syntax
import Yadorigi.Parser.Parser
import Yadorigi.Parser.Tokenizer
import Yadorigi.SemanticAnalysis.BindScope
import Yadorigi.SemanticAnalysis.ReferModule

import Text.Parsec
import Data.Functor
import Data.Maybe
import Control.Monad

import System.Environment

-- Tester

parsing :: String -> String -> Either ParseError Module
parsing filename contents =
    case runParser tokenizer () filename contents of
        (Right ts) -> runParser moduleParser () filename ts
        (Left error) -> Left error


main :: IO ()
main = do
    files <- getArgs
    parsedData <- map (uncurry parsing) <$> zip files <$> mapM readFile files
    let errors = catMaybes $ map (either Just (const Nothing)) parsedData
        succs = catMaybes $ map (either (const Nothing) Just) parsedData
        entities = referModule $ map (bindScope_ declScopeList) succs
    putStrLn "Error :"
    mapM_ print errors
    putStrLn "Success :"
    mapM_ print succs
    putStrLn "Entities :"
    either print (mapM_ print) entities

