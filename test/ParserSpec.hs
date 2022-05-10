module ParserSpec where

import Data.Either (fromRight, isLeft, isRight)
import Parser (ParseResult, Parseable (parse))
import SyntaxTree
  ( ActualParameterList,
    Call (SymbolReference),
    ClassDeclaration,
    Command,
    Condition,
    Expression,
    Factor (CallFactor, Integer),
    FormalParameterList,
    IntSymbolDeclaration (..),
    MethodDeclaration,
    ObjectSymbolDeclaration (..),
    Operator (..),
    ProcedureDeclaration,
    ProcedureHeader,
    Program,
    Relation (..),
    Sign (..),
    SymbolDeclaration,
    SymbolReference (FieldReference, NameReference),
    Term,
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Token (TokenPos)
import Tokenizer (tokenize)

unsafeTokenize :: String -> [TokenPos]
unsafeTokenize = fromRight [] . tokenize

spec :: Spec
spec = do
  describe "parser tests" $ do
    describe "signs" $ do
      it "can parse plus sign" $
        do
          parse (unsafeTokenize "+") :: ParseResult Sign
          `shouldBe` Right Plus

      it "can parse minus sign" $
        do
          parse (unsafeTokenize "-") :: ParseResult Sign
          `shouldBe` Right Minus

    describe "operators" $ do
      it "can parse times operator" $
        do
          parse (unsafeTokenize "*") :: ParseResult Operator
          `shouldBe` Right Times

      it "can parse divide operator" $
        do
          parse (unsafeTokenize "/") :: ParseResult Operator
          `shouldBe` Right Divide

    describe "factors" $ do
      it "can parse call factor representing a simple name reference" $
        do
          parse (unsafeTokenize "test") :: ParseResult Factor
          `shouldBe` Right (CallFactor $ SymbolReference $ NameReference "test")

      it "can parse call factor representing a field reference" $
        do
          parse (unsafeTokenize "test.field") :: ParseResult Factor
          `shouldBe` Right (CallFactor $ SymbolReference $ FieldReference "test" "field")

      it "can parse class instantiation factor" $
        do
          parse (unsafeTokenize "Rational(1,2)") :: ParseResult Factor
          `shouldSatisfy` isRight

      it "can parse number factor" $
        do
          parse (unsafeTokenize "1234") :: ParseResult Factor
          `shouldBe` Right (SyntaxTree.Integer 1234)

      it "can parse composite factor" $
        do
          parse (unsafeTokenize "(1)") :: ParseResult Factor
          `shouldSatisfy` isRight

    describe "terms" $ do
      it "can parse simple unit term" $
        do
          parse (unsafeTokenize "1234") :: ParseResult Term
          `shouldSatisfy` isRight

      it "can parse complex term" $
        do
          parse (unsafeTokenize "1 * 2") :: ParseResult Term
          `shouldSatisfy` isRight

    describe "expressions" $ do
      it "can parse simple unit expression without leading sign" $
        do
          parse (unsafeTokenize "1") :: ParseResult Expression
          `shouldSatisfy` isRight

      it "can parse simple unit expression with leading sign" $
        do
          parse (unsafeTokenize "+1") :: ParseResult Expression
          `shouldSatisfy` isRight

      it "can parse complex expression without leading sign" $
        do
          parse (unsafeTokenize "1 + 2 - 3") :: ParseResult Expression
          `shouldSatisfy` isRight

    describe "relations" $ do
      it "can parse equals relation" $
        do
          parse (unsafeTokenize "=") :: ParseResult Relation
          `shouldBe` Right Equals

      it "can parse smaller relation" $
        do
          parse (unsafeTokenize "<") :: ParseResult Relation
          `shouldBe` Right Smaller

      it "can parse greater relation" $
        do
          parse (unsafeTokenize ">") :: ParseResult Relation
          `shouldBe` Right Greater

    describe "conditions" $ do
      it "can parse comparison" $
        do
          parse (unsafeTokenize "1<2") :: ParseResult Condition
          `shouldSatisfy` isRight

      it "can parse negation" $
        do
          parse (unsafeTokenize "NOT 1=2") :: ParseResult Condition
          `shouldSatisfy` isRight

    describe "commands" $ do
      it "can parse assignment" $
        do
          parse (unsafeTokenize "test := 3") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse int declaration command" $
        do
          parse (unsafeTokenize "INT test") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse object declaration command" $
        do
          parse (unsafeTokenize "OBJ Test test") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse call command" $
        do
          parse (unsafeTokenize "CALL test()") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse read command" $
        do
          parse (unsafeTokenize "READ test") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse unit command block" $
        do
          parse (unsafeTokenize "{ READ test }") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse complex command block" $
        do
          parse (unsafeTokenize "{ READ test READ test }") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can not parse empty command block" $
        do
          parse (unsafeTokenize "{ }") :: ParseResult Command
          `shouldSatisfy` isLeft

      it "can parse if-then-command" $
        do
          parse (unsafeTokenize "IF 1 = 1 THEN PRINTI test") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse while command" $
        do
          parse (unsafeTokenize "WHILE 1 = 1 DO PRINTI test") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse printi command" $
        do
          parse (unsafeTokenize "PRINTI test") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse prints command" $
        do
          parse (unsafeTokenize "PRINTS \"test\"") :: ParseResult Command
          `shouldSatisfy` isRight

      it "can parse error command" $
        do
          parse (unsafeTokenize "ERROR \"test\" ") :: ParseResult Command
          `shouldSatisfy` isRight

    describe "calls" $ do
      it "can parse name reference" $
        do
          parse (unsafeTokenize "test") :: ParseResult Call
          `shouldSatisfy` isRight

      it "can parse field reference" $
        do
          parse (unsafeTokenize "test.field") :: ParseResult Call
          `shouldBe` Right (SymbolReference $ FieldReference "test" "field")

      it "can parse function call" $
        do
          parse (unsafeTokenize "test(1)") :: ParseResult Call
          `shouldSatisfy` isRight

      it "can parse method call" $
        do
          parse (unsafeTokenize "test.test(1)") :: ParseResult Call
          `shouldSatisfy` isRight

    describe "procedure headers" $ do
      it "can parse procedure header without return and without sub-procedures" $
        do
          parse (unsafeTokenize "test(INT x)") :: ParseResult ProcedureHeader
          `shouldSatisfy` isRight

      it "can parse procedure header with return and without sub-procedures" $
        do
          parse (unsafeTokenize "test(INT test) RETURNS INT test") :: ParseResult ProcedureHeader
          `shouldSatisfy` isRight

      it "can parse procedure header with return and with one sub-procedure" $
        do
          parse (unsafeTokenize "test(INT x) RETURNS INT test USING [ PROCEDURE test() PRINTI 1 ]") :: ParseResult ProcedureHeader
          `shouldSatisfy` isRight

      it "can parse procedure header with return and with two sub-procedures" $
        do
          parse (unsafeTokenize "test(INT x) RETURNS INT test USING [ PROCEDURE test() PRINTI 1  PROCEDURE test() PRINTI 1 ]") :: ParseResult ProcedureHeader
          `shouldSatisfy` isRight

    describe "procedure declarations" $ do
      it "can parse procedure declaration" $
        do
          parse (unsafeTokenize "PROCEDURE add(INT x, INT y) RETURNS INT sum { sum := a + b }") :: ParseResult ProcedureDeclaration
          `shouldSatisfy` isRight

    describe "method declarations" $ do
      it "can parse procedure declaration" $
        do
          parse (unsafeTokenize "METHOD add(INT x, INT y) RETURNS INT sum { sum := a + b }") :: ParseResult MethodDeclaration
          `shouldSatisfy` isRight

    describe "actual parameter lists" $ do
      it "can parse empty actual parameter list" $ do (parse (unsafeTokenize "()") :: ParseResult ActualParameterList) `shouldBe` Right []
      it "can parse singleton actual parameter list" $ do (parse (unsafeTokenize "(1)") :: ParseResult ActualParameterList) `shouldSatisfy` isRight
      it "can not parse singleton actual parameter list without closing bracket" $ do (parse (unsafeTokenize "(1") :: ParseResult ActualParameterList) `shouldSatisfy` isLeft
      it "can parse long actual parameter list" $ do (parse (unsafeTokenize "(1,2)") :: ParseResult ActualParameterList) `shouldSatisfy` isRight

    describe "formal parameter lists" $ do
      it "can parse empty formal parameter list" $ do (parse (unsafeTokenize "()") :: ParseResult FormalParameterList) `shouldBe` Right []
      it "can parse singleton formal parameter list" $ do (parse (unsafeTokenize "(INT x)") :: ParseResult FormalParameterList) `shouldSatisfy` isRight
      it "can not parse singleton formal parameter list without closing bracket" $ do (parse (unsafeTokenize "(INT x") :: ParseResult FormalParameterList) `shouldSatisfy` isLeft
      it "can parse long formal parameter list" $ do (parse (unsafeTokenize "(INT x, INT x)") :: ParseResult FormalParameterList) `shouldSatisfy` isRight

    describe "symbol declarations" $ do
      it "can parse object symbol" $
        do
          parse (unsafeTokenize "OBJ Test test") :: ParseResult SymbolDeclaration
          `shouldSatisfy` isRight

      it "can parse int symbol" $
        do
          parse (unsafeTokenize "INT test") :: ParseResult SymbolDeclaration
          `shouldSatisfy` isRight

    describe "object declarations" $ do
      it "can parse object declaration" $
        do
          parse (unsafeTokenize "OBJ Test test") :: ParseResult ObjectSymbolDeclaration
          `shouldBe` Right (Object "Test" "test")

    describe "int declarations" $ do
      it "can parse int declaration" $
        do
          parse (unsafeTokenize "INT test") :: ParseResult IntSymbolDeclaration
          `shouldBe` Right (Int "test")

    describe "class declarations" $ do
      it "can parse class declaration without subclass, without fields and without methods" $
        do
          parse (unsafeTokenize "CLASS Test() INIT PRINTI 1") :: ParseResult ClassDeclaration
          `shouldSatisfy` isRight

      it "can parse class declaration without subclass, without fields and with one method" $
        do
          parse (unsafeTokenize "CLASS Test() INIT PRINTI 1 [ METHOD test() PRINTI 1 ]") :: ParseResult ClassDeclaration
          `shouldSatisfy` isRight

      it "can parse class declaration without subclass, one field and one method" $
        do
          parse (unsafeTokenize "CLASS Test() FIELDS INT x INIT PRINTI 1 [ METHOD test() PRINTI 1 ]") :: ParseResult ClassDeclaration
          `shouldSatisfy` isRight

      it "can parse class declaration with subclass, one field and one method" $
        do
          parse (unsafeTokenize "CLASS Test() SUBCLASSOF Test FIELDS INT x INIT PRINTI 1 [ METHOD test() PRINTI 1 ]") :: ParseResult ClassDeclaration
          `shouldSatisfy` isRight

      it "can parse class declaration with subclass, two fields and one method" $
        do
          parse (unsafeTokenize "CLASS Test() SUBCLASSOF Test FIELDS INT x INT x INIT PRINTI 1 [ METHOD test() PRINTI 1 ]") :: ParseResult ClassDeclaration
          `shouldSatisfy` isRight

      it "can parse class declaration with subclass, two fields and two methods" $
        do
          parse (unsafeTokenize "CLASS Test() SUBCLASSOF Test FIELDS INT x INT x INIT PRINTI 1 [ METHOD test() PRINTI 1  METHOD test() PRINTI 1 ]") :: ParseResult ClassDeclaration
          `shouldSatisfy` isRight

      it "can not parse class declaration without subclass, no field and one method" $
        do
          parse (unsafeTokenize "CLASS Test() FIELDS INIT PRINTI 1 [ METHOD test() PRINTI 1 ]") :: ParseResult ClassDeclaration
          `shouldSatisfy` isLeft

      it "can not parse class declaration without subclass, one field and no method" $
        do
          parse (unsafeTokenize "CLASS Test() FIELDS INT x INIT PRINTI 1 [ ]") :: ParseResult ClassDeclaration
          `shouldSatisfy` isLeft

    describe "programs" $ do
      it "can parse program without class or procedure declarations" $
        do
          parse (unsafeTokenize "DO {INT x x := 1}") :: ParseResult Program
          `shouldSatisfy` isRight

      it "can parse program without class or procedure declarations, but with USING block" $
        do
          parse (unsafeTokenize "USING [ ] DO {INT x x := 1}") :: ParseResult Program
          `shouldSatisfy` isRight

      it "can parse program with one class and no procedure declarations" $
        do
          parse (unsafeTokenize "USING [ CLASS Test() FIELDS INT x INIT PRINTI 1 [ METHOD test() PRINTI 1 ] ] DO {INT x x := 1}") :: ParseResult Program
          `shouldSatisfy` isRight

      it "can parse program with no class and one procedure declaration" $
        do
          parse (unsafeTokenize "USING [ PROCEDURE add(INT x, INT y) RETURNS INT sum { sum := a + b } ] DO {INT x x := 1}") :: ParseResult Program
          `shouldSatisfy` isRight

      it "can parse program with one class and one procedure declaration" $
        do
          parse (unsafeTokenize "USING [ CLASS Test() FIELDS INT x INIT PRINTI 1 [ METHOD test() PRINTI 1 ] PROCEDURE add(INT x, INT y) RETURNS INT sum { sum := a + b } ] DO {INT x x := 1}") :: ParseResult Program
          `shouldSatisfy` isRight

      it "can parse program with two classes and one procedure declaration" $
        do
          parse (unsafeTokenize "USING [ CLASS Test() FIELDS INT x INIT PRINTI 1 [ METHOD test() PRINTI 1 ]  CLASS Test() FIELDS INT x INIT PRINTI 1 [ METHOD test() PRINTI 1 ]  PROCEDURE add(INT x, INT y) RETURNS INT sum { sum := a + b } ] DO {INT x x := 1}") :: ParseResult Program
          `shouldSatisfy` isRight

      it "can parse program with two classes and two procedure declarations" $
        do
          parse (unsafeTokenize "USING [ CLASS Test() FIELDS INT x INIT PRINTI 1 [ METHOD test() PRINTI 1 ]  CLASS Test() FIELDS INT x INIT PRINTI 1 [ METHOD test() PRINTI 1 ]  PROCEDURE add(INT x, INT y) RETURNS INT sum { sum := a + b }  PROCEDURE add(INT x, INT y) RETURNS INT sum { sum := a + b } ] DO {INT x x := 1}") :: ParseResult Program
          `shouldSatisfy` isRight