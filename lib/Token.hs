module Token where

import Text.Parsec (SourcePos)

-- this is the type that the parser requires - a stream of tokens paired with their respective source code positions
type TokenPos = (Token, SourcePos)

-- these are the abstract tokens that can occur
data Token
  = USING
  | CLASS
  | SUBCLASSOF
  | FIELDS
  | INIT
  | OBJ
  | CONST
  | VAR
  | PROCEDURE
  | METHOD
  | RETURNS
  | CALL
  | READ
  | IF
  | THEN
  | WHILE
  | DO
  | PRINTI
  | PRINTS
  | PRINTLNS
  | ERROR
  | NOT
  | (::=)
  | (:=)
  | Comma
  | (:.)
  | (:>)
  | (:<)
  | (:+)
  | (:-)
  | (:*)
  | (:/)
  | OpenRoundBracket
  | CloseRoundBracket
  | OpenSquareBracket
  | CloseSquareBracket
  | OpenCurlyBracket
  | CloseCurlyBracket
  | SymbolName String
  | ClassName String
  | String String
  | Number Integer
  deriving (Eq, Show)