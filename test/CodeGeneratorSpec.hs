module CodeGeneratorSpec where

import CodeGenerator
  ( Generatable (generate),
    calculateInstructionStackMemoryRequirements,
  )
import MachineInstruction (Instruction)
import Data.Either (fromRight, isLeft, isRight)
import Parser (Parseable (parse))
import SyntaxTree (Instruction (Read), Program (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Tokenizer (tokenize)

unsafeParseInstruction :: String -> SyntaxTree.Instruction
unsafeParseInstruction = fromRight (SyntaxTree.Read "") . parse . fromRight [] . tokenize

unsafeParseProgram :: String -> Program
unsafeParseProgram = fromRight (Program [] [] (SyntaxTree.Read "")) . parse . fromRight [] . tokenize

unsafeGenerate :: String -> Either String [MachineInstruction.Instruction]
unsafeGenerate = generate . unsafeParseProgram

spec :: Spec
spec = do
  describe "helper function tests" $ do
    describe "instructions" $ do
      it "can count simple instruction mem requirements correctly" $
        do
          calculateInstructionStackMemoryRequirements
            ( unsafeParseInstruction
                "{ INT m \
                \ m := 2 \
                \ isprime := 1 \
                \ WHILE m < n DO { \
                \ IF (n / m) * m = n THEN isprime := 0 \
                \ m := m + 1 \
                \ } }" ::
                SyntaxTree.Instruction
            )
            `shouldBe` 1
          calculateInstructionStackMemoryRequirements
            ( unsafeParseInstruction
                "{ INT m \
                \ INT n }" ::
                SyntaxTree.Instruction
            )
            `shouldBe` 2
          calculateInstructionStackMemoryRequirements
            ( unsafeParseInstruction
                "{ }" ::
                SyntaxTree.Instruction
            )
            `shouldBe` 0
      it "can calculate complicated instruction mem requirements correctly" $
        do
          calculateInstructionStackMemoryRequirements (unsafeParseInstruction "{INT m {INT n}}") `shouldBe` 2
          calculateInstructionStackMemoryRequirements (unsafeParseInstruction "{{INT n} INT m}") `shouldBe` 1
          calculateInstructionStackMemoryRequirements (unsafeParseInstruction "{INT m {INT n} INT l}") `shouldBe` 2
          calculateInstructionStackMemoryRequirements (unsafeParseInstruction "{INT m {INT n INT k} INT l}") `shouldBe` 3

  describe "compiling test programs" $ do
    it "can compile divzero program" $ do
      prog <- readFile "resources/test-programs/divzero.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can not compile illegalreference program" $ do
      prog <- readFile "resources/test-programs/illegalreference.olang"
      unsafeGenerate prog `shouldSatisfy` isLeft

    it "can compile primes-bounded program" $ do
      prog <- readFile "resources/test-programs/primes-bounded.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile this-shadow-by-return-param program" $ do
      prog <- readFile "resources/test-programs/this-shadow-by-return-param.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile undefinedobject program" $ do
      prog <- readFile "resources/test-programs/undefinedobject.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile uninitializedreturnparameter program" $ do
      prog <- readFile "resources/test-programs/uninitializedreturnparameter.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

  describe "compiling example programs" $ do
    it "can compile ackermann program" $ do
      prog <- readFile "resources/example-programs/ackermann.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile animals program" $ do
      prog <- readFile "resources/example-programs/animals.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile animals-procedures program" $ do
      prog <- readFile "resources/example-programs/animals-procedures.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile expression program" $ do
      prog <- readFile "resources/example-programs/expression.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile fac0 program" $ do
      prog <- readFile "resources/example-programs/fac0.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile fac1 program" $ do
      prog <- readFile "resources/example-programs/fac1.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile fac2 program" $ do
      prog <- readFile "resources/example-programs/fac2.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile fractran program" $ do
      prog <- readFile "resources/example-programs/fractran.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile hello program" $ do
      prog <- readFile "resources/example-programs/hello.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile intbox0 program" $ do
      prog <- readFile "resources/example-programs/intbox0.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile intbox1 program" $ do
      prog <- readFile "resources/example-programs/intbox1.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile intbox2 program" $ do
      prog <- readFile "resources/example-programs/intbox2.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile primes program" $ do
      prog <- readFile "resources/example-programs/primes.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile rational program" $ do
      prog <- readFile "resources/example-programs/rational.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can compile sum program" $ do
      prog <- readFile "resources/example-programs/sum.olang"
      unsafeGenerate prog `shouldSatisfy` isRight