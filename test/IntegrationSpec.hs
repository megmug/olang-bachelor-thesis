module IntegrationSpec where

import CodeGenerator (Generatable (generate))
import MachineInstruction (Instruction)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Either (fromRight, isRight)
import Machine (runTest)
import Parser (Parseable (parse))
import SyntaxTree (Instruction (Read), Program (..))
import Test.Hspec
  ( Spec,
    anyArithException,
    describe,
    it,
    shouldBe,
    shouldSatisfy,
    shouldThrow, anyException,
  )
import Tokenizer (tokenize)

unsafeParseProgram :: String -> Program
unsafeParseProgram = fromRight (Program [] [] (SyntaxTree.Read "")) . parse . fromRight [] . tokenize

unsafeGenerate :: String -> [MachineInstruction.Instruction]
unsafeGenerate p = fromRight [] $ generate $ unsafeParseProgram p

spec :: Spec
spec = do
  describe "running programs" $ do
    it "test ackermann program outputs the right numbers" $ do
      ackSource <- readFile "resources/test-programs/ackermann.olang"
      let ackProgram = unsafeGenerate ackSource
      let ackOutput = runTest ackProgram ["3", "3"]
      ackOutput `shouldSatisfy` isRight
      case ackOutput of
        Left _ -> return ()
        Right out -> out `shouldBe` "61"

      let sndAckOutput = runTest ackProgram ["3", "4"]
      sndAckOutput `shouldSatisfy` isRight
      case sndAckOutput of
        Left _ -> return ()
        Right out -> out `shouldBe` "125"

    it "dividing by zero throws an exception" $ do
      divSource <- readFile "resources/test-programs/divzero.olang"
      let divProgram = unsafeGenerate divSource
      (evaluate . force) (runTest divProgram []) `shouldThrow` anyArithException
    
    it "running the undefined object field program should throw a null pointer exception" $ do
      source <- readFile "resources/test-programs/undefinedobject.olang"
      let prog = unsafeGenerate source
      (evaluate . force) (runTest prog []) `shouldThrow` anyException
