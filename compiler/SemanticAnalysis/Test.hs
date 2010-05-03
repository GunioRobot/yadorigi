
module Yadorigi.SemanticAnalysis.Test where

import Data.Functor
import Data.Tuple.All
import Control.Monad
import Text.Parsec

import Yadorigi.Monad.Either
import Yadorigi.Syntax
import Yadorigi.Parser.Parser
import Yadorigi.Parser.Tokenizer
import Yadorigi.SemanticAnalysis.BindScope
import Yadorigi.SemanticAnalysis.ReferModule
import Yadorigi.SemanticAnalysis.NameResolution

import System.Environment

-- Tester

parsing :: String -> String -> Either ParseError Module
parsing filename contents =
    runParser tokenizer () filename contents >>= runParser moduleParser () filename

main :: IO ()
main = do
    files <- getArgs
    parsedData <- sequence <$> map (uncurry parsing) <$> zip files <$> mapM readFile files
    case parsedData of
        (Right result) -> case referModule $ map bindScope' result of
            (Right result) -> mapM_ (print.f) result
            (Left error) -> print error
        (Left error) -> print error

f (mod,modname,names,types) =
    let names' = [(modname,smodname,name) | ((modname,name),smodname) <- names]++
            [(modname,smodname,name) | ((modname,_),children,smodname) <- types, name <- children]
        gnames = [name | name <- names', sel1 name == []]
        lnames = [(smodname,[],name) | (modname,smodname,name) <- names', null $ modname]
        types' = [(modname,smodname,name) | ((modname,name),_,smodname) <- types] in
            nameResolution' ((modname,[]),gnames,types',lnames) mod

