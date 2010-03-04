
module Yadorigi.Parser.DataTypes where

import Text.Parsec
import Yadorigi.Syntax

-- Token

data Token' = Token' SourcePos Token

instance Show Token' where
    show (Token' pos t) = show pos++" "++show t

type TokenStream = [Token']

data Token
    = LiteralToken Literal {- literal token -}
    | NameToken ScopedName {- name token -}
    | OpToken ScopedName {- operator token -}
    | ReservedToken String {- reserved word token, reserved symbol token -}
        deriving (Eq,Show)
