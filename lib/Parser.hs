{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import Data.Maybe (fromMaybe)
import SyntaxTree
  ( ActualParameterList,
    Call (..),
    ClassDeclaration (..),
    Command (..),
    Condition (..),
    Expression (..),
    Factor (..),
    FormalParameterList,
    IntSymbolDeclaration (Int),
    MethodDeclaration (..),
    ObjectSymbolDeclaration (Object),
    Operator (..),
    ProcedureDeclaration (..),
    ProcedureHeader (..),
    Program (..),
    Relation (..),
    Sign (..),
    SymbolDeclaration (IntDeclaration, ObjectDeclaration),
    SymbolReference (..),
    Term (..),
  )
import Text.Parsec.Combinator (eof, many1, optionMaybe)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Pos (SourcePos)
import Text.Parsec.Prim
  ( Parsec,
    many,
    parse,
    tokenPrim,
    (<|>),
  )
import Token (Token (..), TokenPos)

{-# ANN module ("hlint: ignore Use $>") #-}
{-# ANN module ("hlint: ignore Use <$") #-}
{-# ANN module ("hlint: ignore Use lambda-case") #-}

-- a parser generates some output by consuming a list of tokens + positions
type Parser a = Parsec [TokenPos] () a

type ParseResult a = Either ParseError a

-- all syntactic elements (SyntaxTree) are parseable
class Parseable a where
  parser :: Parser a

  parse :: [TokenPos] -> Either ParseError a
  parse = Text.Parsec.Prim.parse parser "input"

-- for every class instance, we also define these synonyms to make the code more readable
program :: Parser Program
program = parser

instance Parseable Program where
  parser = do
    mcps <- using
    _ <- accept DO
    cmd <- command
    eof
    return
      ( case mcps of
          Nothing -> Program [] [] cmd
          Just (cs, ps) -> Program cs ps cmd
      )
    where
      using =
        optionMaybe $
          (,)
            <$> (accept USING *> accept OpenSquareBracket *> many classdeclaration)
              <*> (many proceduredeclaration <* accept CloseSquareBracket)

classdeclaration :: Parser ClassDeclaration
classdeclaration = parser

instance Parseable ClassDeclaration where
  parser =
    Class
      <$> (accept CLASS *> acceptClassName)
        <*> formalparameterlist
        <*> optionMaybe (accept SUBCLASSOF *> acceptClassName)
        <*> (fromMaybe [] <$> optionMaybe (accept FIELDS *> many1 symboldeclaration))
        <*> (accept INIT *> command)
        <*> (fromMaybe [] <$> optionMaybe (accept OpenSquareBracket *> many1 methoddeclaration <* accept CloseSquareBracket))

intsymboldeclaration :: Parser IntSymbolDeclaration
intsymboldeclaration = parser

instance Parseable IntSymbolDeclaration where
  parser = Int <$> (accept INT *> acceptSymbolName)

objectsymboldeclaration :: Parser ObjectSymbolDeclaration
objectsymboldeclaration = parser

instance Parseable ObjectSymbolDeclaration where
  parser = Object <$> (accept OBJ *> acceptClassName) <*> acceptSymbolName

symboldeclaration :: Parser SymbolDeclaration
symboldeclaration = parser

instance Parseable SymbolDeclaration where
  parser =
    IntDeclaration <$> intsymboldeclaration
      <|> ObjectDeclaration <$> objectsymboldeclaration

formalparameterlist :: Parser FormalParameterList
formalparameterlist = parser

instance Parseable FormalParameterList where
  parser =
    fromMaybe []
      <$> ( accept OpenRoundBracket
              *> optionMaybe ((:) <$> symboldeclaration <*> many (accept Comma *> symboldeclaration))
              <* accept CloseRoundBracket
          )

actualparameterlist :: Parser ActualParameterList
actualparameterlist = parser

instance Parseable ActualParameterList where
  parser =
    fromMaybe []
      <$> ( accept OpenRoundBracket
              *> optionMaybe ((:) <$> expression <*> many (accept Comma *> expression))
              <* accept CloseRoundBracket
          )

methoddeclaration :: Parser MethodDeclaration
methoddeclaration = parser

instance Parseable MethodDeclaration where
  parser = Method <$> (accept METHOD *> procedureheader) <*> command

proceduredeclaration :: Parser ProcedureDeclaration
proceduredeclaration = parser

instance Parseable ProcedureDeclaration where
  parser = Procedure <$> (accept PROCEDURE *> procedureheader) <*> command

procedureheader :: Parser ProcedureHeader
procedureheader = parser

instance Parseable ProcedureHeader where
  parser =
    ProcedureHeader <$> acceptSymbolName
      <*> formalparameterlist
      <*> optionMaybe (accept RETURNS *> symboldeclaration)
      <*> do
        ml <- optionMaybe $ accept USING *> accept OpenSquareBracket *> many1 proceduredeclaration <* accept CloseSquareBracket
        return $ fromMaybe [] ml

call :: Parser Call
call = parser

instance Parseable Call where
  parser = do
    sr <- symbolreference
    mapl <- optionMaybe actualparameterlist
    return
      ( case mapl of
          Nothing -> SymbolReference sr
          Just es -> Call sr es
      )

symbolreference :: Parser SymbolReference
symbolreference = parser

instance Parseable SymbolReference where
  parser = do
    n <- acceptSymbolName
    mn <- optionMaybe $ accept (:.) *> acceptSymbolName
    return
      ( case mn of
          Nothing -> NameReference n
          Just n' -> FieldReference n n'
      )

command :: Parser Command
command = parser

instance Parseable Command where
  parser =
    Assignment <$> symbolreference <* accept (::=) <*> expression
      <|> SymbolDeclarationCommand <$> symboldeclaration
      <|> CallCommand <$> (accept CALL *> call)
      <|> Read <$> (accept READ *> acceptSymbolName)
      <|> Block <$> (fromList <$> (accept OpenCurlyBracket *> many1 command <* accept CloseCurlyBracket))
      <|> IfThen <$> (accept IF *> condition) <*> (accept THEN *> command)
      <|> While <$> (accept WHILE *> condition) <*> (accept DO *> command)
      <|> PrintI <$> (accept PRINTI *> expression)
      <|> PrintS <$> (accept PRINTS *> acceptString)
      <|> PrintLnS <$> (accept PRINTLNS *> acceptString)
      <|> pure SyntaxTree.Error <* accept ERROR

condition :: Parser Condition
condition = parser

instance Parseable Condition where
  parser =
    (Comparison <$> expression <*> relation <*> expression)
      <|> (Negation <$> (accept NOT *> condition))

relation :: Parser Relation
relation = parser

instance Parseable Relation where
  parser =
    (accept (:<) *> pure Smaller)
      <|> (accept (:>) *> pure Greater)
      <|> (accept (:=) *> pure Equals)

expression :: Parser Expression
expression = parser

instance Parseable Expression where
  parser =
    Expression
      <$> ((:|) <$> firstSignTerm <*> manySignTerms)
    where
      firstSignTerm = do
        ms <- optionMaybe sign
        t <- term
        return (fromMaybe Plus ms, t)
      manySignTerms = many ((,) <$> sign <*> term)

term :: Parser Term
term = parser

instance Parseable Term where
  parser = Term <$> factor <*> many ((,) <$> operator <*> factor)

factor :: Parser Factor
factor = parser

instance Parseable Factor where
  parser =
    (CallFactor <$> call)
      <|> (ClassInstantiation <$> acceptClassName <*> actualparameterlist)
      <|> (SyntaxTree.Integer <$> acceptInteger)
      <|> (CompositeFactor <$> (accept OpenRoundBracket *> expression <* accept CloseRoundBracket))

sign :: Parser Sign
sign = parser

instance Parseable Sign where
  parser =
    (accept (:-) *> pure Minus)
      <|> (accept (:+) *> pure Plus)

operator :: Parser Operator
operator = parser

instance Parseable Operator where
  parser =
    (accept (:*) *> pure Times)
      <|> (accept (:/) *> pure Divide)

{- Primitive helper functions
 - Most of these are only needed because we don't have a standard character parser, for which parsec is optimized
 - Rather, we use our own abstract token input stream type
 - So we need to reimplement things like advance, accept or satisfy
 -}

accept :: Token -> Parser Token
accept t = satisfy (== t)

advance :: SourcePos -> t -> [TokenPos] -> SourcePos
advance _ _ ((_, pos) : _) = pos
advance pos _ [] = pos

acceptSymbolName :: Parser String
acceptSymbolName = tokenPrim show advance (\t -> case t of (SymbolName n, _) -> Just n; _ -> Nothing)

acceptClassName :: Parser String
acceptClassName = tokenPrim show advance (\t -> case t of (ClassName n, _) -> Just n; _ -> Nothing)

acceptString :: Parser String
acceptString = tokenPrim show advance (\t -> case t of (String s, _) -> Just s; _ -> Nothing)

acceptInteger :: Parser Integer
acceptInteger = tokenPrim show advance (\t -> case t of (Token.Integer n, _) -> Just n; _ -> Nothing)

satisfy :: (Token -> Bool) -> Parser Token
satisfy p = tokenPrim show advance (\(t, _) -> if p t then Just t else Nothing)