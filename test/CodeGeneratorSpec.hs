module CodeGeneratorSpec where

import CodeGenerator
  ( Generatable (generate),
    calculateStackMemoryRequirement,
  )
import Data.Either (fromRight, isLeft, isRight)
import Parser (Parseable (parse))
import SyntaxTree (Instruction (Read), Program (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Tokenizer (tokenize)
import MachineInstruction

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
          calculateStackMemoryRequirement
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
          calculateStackMemoryRequirement
            ( unsafeParseInstruction
                "{ INT m \
                \ INT n }" ::
                SyntaxTree.Instruction
            )
            `shouldBe` 2
          calculateStackMemoryRequirement
            ( unsafeParseInstruction
                "{ }" ::
                SyntaxTree.Instruction
            )
            `shouldBe` 0
      it "can calculate complicated instruction mem requirements correctly" $
        do
          calculateStackMemoryRequirement (unsafeParseInstruction "{INT m {INT n}}") `shouldBe` 2
          calculateStackMemoryRequirement (unsafeParseInstruction "{{INT n} INT m}") `shouldBe` 1
          calculateStackMemoryRequirement (unsafeParseInstruction "{INT m {INT n} INT l}") `shouldBe` 2
          calculateStackMemoryRequirement (unsafeParseInstruction "{INT m {INT n INT k} INT l}") `shouldBe` 3

  describe "compiling test programs" $ do
    it "can compile divzero program" $ do
      prog <- readFile "resources/test-programs/divzero.olang"
      unsafeGenerate prog `shouldSatisfy` isRight

    it "can not compile illegalreference program" $ do
      prog <- readFile "resources/test-programs/illegalreference.olang"
      unsafeGenerate prog `shouldSatisfy` isLeft

    it "can not compile return incompatible shadow program" $ do
      prog <- readFile "resources/test-programs/return-param-shadowed-by-incompatible-type.olang"
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

  describe "example programs for machine compile to the correct instructions" $ do
    it "program fac0 compiles to the correct machine code" $ do
      prog <- readFile "resources/example-programs/fac0.olang"
      unsafeGenerate prog `shouldBe` Right [PushInt 0,PushInt 0,PrintStr "Please enter a natural number n: ",PushInt 0,StoreStack 0,MachineInstruction.Read,StoreStack 0,PushInt 0,StoreStack 1,PushInt 1,StoreStack 1,LoadStack 0,PushInt 0,CombineBinary Smaller,JumpIfFalse 19,LoadStack 0,PrintInt,PrintStrLn " is not a natural number!",Halt,LoadStack 0,PushInt 0,CombineBinary Greater,JumpIfFalse 32,LoadStack 1,LoadStack 0,CombineBinary Times,StoreStack 1,LoadStack 0,PushInt 1,CombineBinary Minus,StoreStack 0,Jump 19,PrintStr "n! = ",LoadStack 1,PrintInt,Halt]

    it "program fac1 compiles to the correct machine code" $ do
      prog <- readFile "resources/example-programs/fac1.olang"
      unsafeGenerate prog `shouldBe` Right [Jump 29,PushInt 0,LoadStack 0,PushInt 0,CombineBinary Smaller,JumpIfFalse 10,LoadStack 0,PrintInt,PrintStrLn " is not a natural number!",Halt,LoadStack 0,PushInt 0,CombineBinary Equals,JumpIfFalse 16,PushInt 1,StoreStack 1,LoadStack 0,PushInt 0,CombineBinary Greater,JumpIfFalse 27,LoadStack 0,LoadStack 0,PushInt 1,CombineBinary Minus,CallProcedure 1 1,CombineBinary Times,StoreStack 1,LoadStack 1,Return True,PushInt 0,PrintStr "Please enter a natural number n: ",PushInt 0,StoreStack 0,MachineInstruction.Read,StoreStack 0,PrintStr "n! = ",LoadStack 0,CallProcedure 1 1,PrintInt,Halt]

    it "program fac2 compiles to the correct machine code" $ do
      prog <- readFile "resources/example-programs/fac2.olang"
      unsafeGenerate prog `shouldBe` Right [Jump 14,PushInt 0,PushInt (-1),StoreStack 1,AllocateHeap 1 0,StoreStack 1,LoadStack 1,PushInt 0,StoreHeap 0,LoadStack 1,LoadStack 0,StoreHeap 0,LoadStack 1,Return True,Jump 22,LoadStack 0,LoadStack 0,LoadHeap 0,LoadStack 1,CombineBinary Times,StoreHeap 0,Return False,Jump 27,LoadStack 0,LoadHeap 0,PrintInt,Return False,PushInt 0,PushInt 0,CreateMethodTable 0 [(1,23),(0,15)],PrintStr "Please enter a natural number n: ",PushInt 0,StoreStack 0,MachineInstruction.Read,StoreStack 0,PushInt (-1),StoreStack 1,PushInt 1,CallProcedure 1 1,StoreStack 1,LoadStack 0,PushInt 0,CombineBinary Smaller,JumpIfFalse 48,LoadStack 0,PrintInt,PrintStrLn " is not a natural number!",Halt,LoadStack 0,PushInt 0,CombineBinary Greater,JumpIfFalse 60,LoadStack 1,LoadStack 0,CallMethod 0 1,LoadStack 0,PushInt 1,CombineBinary Minus,StoreStack 0,Jump 48,PrintStr "n! = ",LoadStack 1,CallMethod 1 0,Halt]