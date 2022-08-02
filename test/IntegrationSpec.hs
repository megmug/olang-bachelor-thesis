module IntegrationSpec where

import CodeGenerator (Generatable (generate))
import MachineInstruction (Instruction)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Either (fromRight)
import Machine (runTest)
import Parser (Parseable (parse))
import SyntaxTree (Instruction (Read), Program (..))
import Test.Hspec
  ( Spec,
    anyArithException,
    describe,
    it,
    shouldBe,
    shouldThrow, anyException,
  )
import Tokenizer (tokenize)

unsafeParseProgram :: String -> Program
unsafeParseProgram = fromRight (Program [] [] (SyntaxTree.Read "")) . parse . fromRight [] . tokenize

unsafeGenerate :: String -> [MachineInstruction.Instruction]
unsafeGenerate p = fromRight [] $ generate $ unsafeParseProgram p

spec :: Spec
spec = do
  describe "running test programs" $ do
    it "dividing by zero throws an exception" $ do
      source <- readFile "resources/test-programs/divzero.olang"
      let prog = unsafeGenerate source
      (evaluate . force) (runTest prog []) `shouldThrow` anyArithException
    
    it "running the undefined object field test program should throw a null pointer exception" $ do
      source <- readFile "resources/test-programs/undefinedobject.olang"
      let prog = unsafeGenerate source
      (evaluate . force) (runTest prog []) `shouldThrow` anyException

    it "running the uninitialized return parameter test program should throw a null pointer exception" $ do
      source <- readFile "resources/test-programs/uninitializedreturnparameter.olang"
      let prog = unsafeGenerate source
      (evaluate . force) (runTest prog []) `shouldThrow` anyException

    it "running the this-shadow-by-return-param test program should work" $ do
      source <- readFile "resources/test-programs/this-shadow-by-return-param.olang"
      let prog = unsafeGenerate source
      runTest prog [] `shouldBe` Right "1"

    it "primes program outputs the right numbers" $ do
      source <- readFile "resources/test-programs/primes-bounded.olang"
      runTest (unsafeGenerate source) [] `shouldBe` Right "I will now begin listing some primes\n2\n3\n5\n"
  
  describe "running example programs" $ do
    it "hello program program outputs 'Hello World'" $ do
      source <- readFile "resources/example-programs/hello.olang"
      runTest (unsafeGenerate source) [] `shouldBe` Right "Hello world!\n"

    it "ackermann program outputs the right numbers" $ do
      source <- readFile "resources/example-programs/ackermann.olang"
      runTest (unsafeGenerate source) ["3", "3"] `shouldBe` Right "This program calculates the ackermann function ack(n, m).\nPlease enter a natural number n: Please enter a natural number m: ack(n, m) = 61"

    it "sum program outputs the right numbers" $ do
      source <- readFile "resources/example-programs/sum.olang"
      runTest (unsafeGenerate source) ["1", "2"] `shouldBe` Right "This program calculates the sum of two integers a + b.\nPlease enter a: Please enter b: a + b = 3"
    
    it "intbox0 program works correctly" $ do
      source <- readFile "resources/example-programs/intbox0.olang"
      runTest (unsafeGenerate source) [] `shouldBe` Right "1"
  
    it "intbox1 program works correctly" $ do
      source <- readFile "resources/example-programs/intbox1.olang"
      runTest (unsafeGenerate source) [] `shouldBe` Right "0\n1\n"

    it "intbox2 program works correctly" $ do
      source <- readFile "resources/example-programs/intbox2.olang"
      runTest (unsafeGenerate source) [] `shouldBe` Right "0\n1\n"

    it "rational program works correctly" $ do
      source <- readFile "resources/example-programs/rational.olang"
      runTest (unsafeGenerate source) ["3", "5", "7", "9"] `shouldBe` Right "This program prompts you to enter two rational numbers, and performs some calculations with them.\n*First number*\nPlease enter the numerator: Please enter the denominator: *Second number*\nPlease enter the numerator: Please enter the denominator: (3 / 5) + (7 / 9) = 62 / 45\n(3 / 5) - (7 / 9) = 8 / -45\n(3 / 5) * (7 / 9) = 7 / 15\n(3 / 5) / (7 / 9) = 27 / 35\n"

    it "animals program works correctly" $ do
      source <- readFile "resources/example-programs/animals.olang"
      runTest (unsafeGenerate source) ["0"] `shouldBe` Right "What kind of animal do you like most?\n0: Dogs\n1: Cats\notherwise: a different one\nCongratulations, you get a dog!\nA dog was born!\nWhat sound does it make?\nWoof!\n"
      runTest (unsafeGenerate source) ["1"] `shouldBe` Right "What kind of animal do you like most?\n0: Dogs\n1: Cats\notherwise: a different one\nCongratulations, you get a cat!\nA cat was born!\nWhat sound does it make?\nMeow!\n"

    it "expression program works correctly" $ do
      source <- readFile "resources/example-programs/expression.olang"
      runTest (unsafeGenerate source) [] `shouldBe` Right "(((3!)^(3^3)) * ((3 * 4) / (9 - 7))) = 6140942214464815497216"

    it "animals-procedures program works correctly" $ do
      source <- readFile "resources/example-programs/animals-procedures.olang"
      runTest (unsafeGenerate source) [] `shouldBe` Right "An animal was born!\n*generic animal sound*\n*generic animal sound*\nA dog was born!\n*generic animal sound*\nWoof!\nA cat was born!\n*generic animal sound*\nMeow!\n"

    it "fac0 program works correctly" $ do
      source <- readFile "resources/example-programs/fac0.olang"
      runTest (unsafeGenerate source) ["3"] `shouldBe` Right "Please enter a natural number n: n! = 6"

    it "fac1 program works correctly" $ do
      source <- readFile "resources/example-programs/fac1.olang"
      runTest (unsafeGenerate source) ["3"] `shouldBe` Right "Please enter a natural number n: n! = 6"

    it "fac2 program works correctly" $ do
      source <- readFile "resources/example-programs/fac2.olang"
      runTest (unsafeGenerate source) ["3"] `shouldBe` Right "Please enter a natural number n: n! = 6"