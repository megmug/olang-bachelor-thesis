{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module SyntaxTree where

import Data.List.NonEmpty (NonEmpty)

{- There is a type for all important nonterminals in the grammar
 - The grammar is not represented 1:1, rather an optimized version that is stripped of unnecessary details
 - This makes it easier to compile the language
 -}
type ClassName = String

type SymbolName = String

type Initializer = Command

type ActualParameterList = [Expression]

type FormalParameterList = [SymbolDeclaration]

data Program
  = Program
      [ClassDeclaration]
      [ProcedureDeclaration]
      Command
  deriving (Eq, Show)

data ClassDeclaration
  = Class
      ClassName
      FormalParameterList
      (Maybe ClassName)
      [SymbolDeclaration]
      Initializer
      [MethodDeclaration]
  deriving (Eq, Show)

data IntSymbolDeclaration = Int SymbolName deriving (Eq, Show)

data ObjectSymbolDeclaration = Object ClassName SymbolName deriving (Eq, Show)

data SymbolDeclaration
  = IntDeclaration IntSymbolDeclaration
  | ObjectDeclaration ObjectSymbolDeclaration
  deriving (Eq, Show)

data MethodDeclaration = Method ProcedureHeader Command deriving (Eq, Show)

data ProcedureDeclaration = Procedure ProcedureHeader Command deriving (Eq, Show)

data ProcedureHeader
  = ProcedureHeader
      SymbolName
      FormalParameterList
      (Maybe SymbolDeclaration)
      [ProcedureDeclaration]
  deriving (Eq, Show)

data Call
  = SymbolReference SymbolReference
  | Call SymbolReference ActualParameterList
  deriving (Eq, Show)

data SymbolReference
  = NameReference SymbolName
  | FieldReference SymbolName SymbolName
  deriving (Eq, Show)

data Command
  = Assignment SymbolReference Expression
  | SymbolDeclarationCommand SymbolDeclaration
  | CallCommand Call
  | Read SymbolName
  | Block (NonEmpty Command)
  | IfThen Condition Command
  | While Condition Command
  | PrintI Expression
  | PrintS String
  | PrintLnS String
  | Error
  deriving (Eq, Show)

data Condition
  = Comparison Expression Relation Expression
  | Negation Condition
  deriving (Eq, Show)

data Relation = Equals | Smaller | Greater deriving (Eq, Show)

data Expression
  = Expression
      (NonEmpty (Sign, Term))
  deriving (Eq, Show)

data Term
  = Term
      Factor
      [(Operator, Factor)]
  deriving (Eq, Show)

data Factor
  = CallFactor Call
  | ClassInstantiation ClassName ActualParameterList
  | Integer Integer
  | CompositeFactor Expression
  deriving (Eq, Show)

data Sign = Plus | Minus deriving (Eq, Show)

data Operator = Times | Divide deriving (Eq, Show)