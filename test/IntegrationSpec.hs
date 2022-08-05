module IntegrationSpec where

import CodeGenerator (Generatable (generate))
import MachineInstruction (Instruction)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Either (fromRight)
import Machine (runProgramTest)
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
      (evaluate . force) (runProgramTest prog []) `shouldThrow` anyArithException
    
    it "running the undefined object field test program should throw a null pointer exception" $ do
      source <- readFile "resources/test-programs/undefinedobject.olang"
      let prog = unsafeGenerate source
      (evaluate . force) (runProgramTest prog []) `shouldThrow` anyException

    it "running the uninitialized return parameter test program should throw a null pointer exception" $ do
      source <- readFile "resources/test-programs/uninitializedreturnparameter.olang"
      let prog = unsafeGenerate source
      (evaluate . force) (runProgramTest prog []) `shouldThrow` anyException

    it "running the this-shadow-by-return-param test program should work" $ do
      source <- readFile "resources/test-programs/this-shadow-by-return-param.olang"
      let prog = unsafeGenerate source
      runProgramTest prog [] `shouldBe` Right "1"

    it "primes program outputs the right numbers" $ do
      source <- readFile "resources/test-programs/primes-bounded.olang"
      runProgramTest (unsafeGenerate source) [] `shouldBe` Right "I will now begin listing some primes\n2\n3\n5\n"

    it "fractran program outputs the right numbers when PRIMEGAME is run for 100 steps" $ do
      source <- readFile "resources/test-programs/fractran-bounded.olang"
      runProgramTest (unsafeGenerate source) ["0", "2"] `shouldBe` Right "Welcome to the FRACTRAN interpreter.\nWhich program do you want to execute?\n0: PRIMEGAME\n1: Fibonacci\n2: POLYGAME\n3: Enter custom program interactively\nIf started with input 2, PRIMEGAME computes all prime powers of 2 (among some other numbers which are not powers of 2).\nInput number: Program: 17 / 91, 78 / 85, 19 / 51, 23 / 38, 29 / 33, 77 / 29, 95 / 23, 77 / 19, 1 / 17, 11 / 13, 13 / 11, 15 / 2, 1 / 7, 55 / 1, \nInput: 2\nProgram output: \n15\n825\n725\n1925\n2275\n425\n390\n330\n290\n770\n910\n170\n156\n132\n116\n"
  
  describe "running example programs" $ do
    it "hello program program outputs 'Hello World'" $ do
      source <- readFile "resources/example-programs/hello.olang"
      runProgramTest (unsafeGenerate source) [] `shouldBe` Right "Hello world!\n"

    it "ackermann program outputs the right numbers" $ do
      source <- readFile "resources/example-programs/ackermann.olang"
      runProgramTest (unsafeGenerate source) ["3", "3"] `shouldBe` Right "This program calculates the ackermann function ack(n, m).\nPlease enter a natural number n: Please enter a natural number m: ack(n, m) = 61"

    it "sum program outputs the right numbers" $ do
      source <- readFile "resources/example-programs/sum.olang"
      runProgramTest (unsafeGenerate source) ["1", "2"] `shouldBe` Right "This program calculates the sum of two integers a + b.\nPlease enter a: Please enter b: a + b = 3"
    
    it "intbox0 program works correctly" $ do
      source <- readFile "resources/example-programs/intbox0.olang"
      runProgramTest (unsafeGenerate source) [] `shouldBe` Right "1"
  
    it "intbox1 program works correctly" $ do
      source <- readFile "resources/example-programs/intbox1.olang"
      runProgramTest (unsafeGenerate source) [] `shouldBe` Right "0\n1\n"

    it "intbox2 program works correctly" $ do
      source <- readFile "resources/example-programs/intbox2.olang"
      runProgramTest (unsafeGenerate source) [] `shouldBe` Right "0\n1\n"

    it "rational program works correctly" $ do
      source <- readFile "resources/example-programs/rational.olang"
      runProgramTest (unsafeGenerate source) ["3", "5", "7", "9"] `shouldBe` Right "This program prompts you to enter two rational numbers, and performs some calculations with them.\n*First number*\nPlease enter the numerator: Please enter the denominator: *Second number*\nPlease enter the numerator: Please enter the denominator: (3 / 5) + (7 / 9) = 62 / 45\n(3 / 5) - (7 / 9) = 8 / -45\n(3 / 5) * (7 / 9) = 7 / 15\n(3 / 5) / (7 / 9) = 27 / 35\n"

    it "animals program works correctly" $ do
      source <- readFile "resources/example-programs/animals.olang"
      runProgramTest (unsafeGenerate source) ["0"] `shouldBe` Right "What kind of animal do you like most?\n0: Dogs\n1: Cats\notherwise: a different one\nCongratulations, you get a dog!\nA dog was born!\nWhat sound does it make?\nWoof!\n"
      runProgramTest (unsafeGenerate source) ["1"] `shouldBe` Right "What kind of animal do you like most?\n0: Dogs\n1: Cats\notherwise: a different one\nCongratulations, you get a cat!\nA cat was born!\nWhat sound does it make?\nMeow!\n"

    it "expression program works correctly" $ do
      source <- readFile "resources/example-programs/expression.olang"
      runProgramTest (unsafeGenerate source) [] `shouldBe` Right "(((3!)^(3^3)) * ((3 * 4) / (9 - 7))) = 6140942214464815497216"

    it "animals-procedures program works correctly" $ do
      source <- readFile "resources/example-programs/animals-procedures.olang"
      runProgramTest (unsafeGenerate source) [] `shouldBe` Right "An animal was born!\n*generic animal sound*\n*generic animal sound*\nA dog was born!\n*generic animal sound*\nWoof!\nA cat was born!\n*generic animal sound*\nMeow!\n"

    it "fac0 program works correctly" $ do
      source <- readFile "resources/example-programs/fac0.olang"
      runProgramTest (unsafeGenerate source) ["3"] `shouldBe` Right "Please enter a natural number n: n! = 6"

    it "fac1 program works correctly" $ do
      source <- readFile "resources/example-programs/fac1.olang"
      runProgramTest (unsafeGenerate source) ["3"] `shouldBe` Right "Please enter a natural number n: n! = 6"

    it "fac2 program works correctly" $ do
      source <- readFile "resources/example-programs/fac2.olang"
      runProgramTest (unsafeGenerate source) ["3"] `shouldBe` Right "Please enter a natural number n: n! = 6"