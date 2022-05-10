module CodeGeneratorSpec where

import CodeGenerator
  ( Generatable (generate),
    calculateCommandStackMemoryRequirements,
  )
import Command (Command)
import Data.Either (fromRight, isLeft, isRight)
import Parser (Parseable (parse))
import SyntaxTree (Command (Read), Program (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Tokenizer (tokenize)

unsafeParseCommand :: String -> SyntaxTree.Command
unsafeParseCommand = fromRight (SyntaxTree.Read "") . parse . fromRight [] . tokenize

unsafeParseProgram :: String -> Program
unsafeParseProgram = fromRight (Program [] [] (SyntaxTree.Read "")) . parse . fromRight [] . tokenize

unsafeGenerate :: String -> Either String [Command.Command]
unsafeGenerate = generate . unsafeParseProgram

spec :: Spec
spec = do
  describe "helper function tests" $ do
    describe "commands" $ do
      it "can count simple command mem requirements correctly" $
        do
          calculateCommandStackMemoryRequirements
            ( unsafeParseCommand
                "{ INT m \
                \ m := 2 \
                \ isprime := 1 \
                \ WHILE m < n DO { \
                \ IF (n / m) * m = n THEN isprime := 0 \
                \ m := m + 1 \
                \ } }" ::
                SyntaxTree.Command
            )
            `shouldBe` 1
          calculateCommandStackMemoryRequirements
            ( unsafeParseCommand
                "{ INT m \
                \ INT n }" ::
                SyntaxTree.Command
            )
            `shouldBe` 2
          calculateCommandStackMemoryRequirements
            ( unsafeParseCommand
                "{ }" ::
                SyntaxTree.Command
            )
            `shouldBe` 0
      it "can calculate complicated command mem requirements correctly" $
        do
          calculateCommandStackMemoryRequirements (unsafeParseCommand "{INT m {INT n}}") `shouldBe` 2
          calculateCommandStackMemoryRequirements (unsafeParseCommand "{{INT n} INT m}") `shouldBe` 1
          calculateCommandStackMemoryRequirements (unsafeParseCommand "{INT m {INT n} INT l}") `shouldBe` 2
          calculateCommandStackMemoryRequirements (unsafeParseCommand "{INT m {INT n INT k} INT l}") `shouldBe` 3

  describe "compiling test programs" $ do
    it "can not compile illegalreference program" $ do
      prog <- readFile "resources/test-programs/illegalreference.olang"
      unsafeGenerate prog `shouldSatisfy` isLeft

    it "can compile divzero program" $ do
      prog <- readFile "resources/test-programs/divzero.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

  describe "compiling example programs" $ do
    it "can compile ackermann program" $ do
      prog <- readFile "resources/example-programs/ackermann.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile animals program" $ do
      prog <- readFile "resources/example-programs/animals.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile bank program" $ do
      prog <- readFile "resources/example-programs/bank.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile linked list program" $ do
      prog <- readFile "resources/example-programs/linkedlist.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile potfak program" $ do
      prog <- readFile "resources/example-programs/potfak.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile primes program" $ do
      prog <- readFile "resources/example-programs/primes.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile rational program" $ do
      prog <- readFile "resources/example-programs/rational.olang"
      unsafeGenerate prog `shouldSatisfy` isRight
