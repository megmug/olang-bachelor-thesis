module TokenizerSpec where

import Data.Either
import Test.Hspec
import Tokenizer

spec :: Spec
spec = do
  describe "tokenizer tests" $ do
    it "can tokenize simple assignment command" $ do
      tokenize "hello := 1 + 1" `shouldSatisfy` isRight

    it "can tokenize all zero-argument tokens seperated by spaces" $ do
      tokenize "USING CLASS SUBCLASSOF FIELDS INIT OBJ STATIC CONST VAR PROCEDURE METHOD RETURNS CALL READ WRITE IF THEN WHILE DO ERROR NOT := = , . > < + - * / ( ) [ ] { }"
        `shouldSatisfy` isRight

    it "can tokenize simple program" $ do
      tokenize "DO {VAR x x := 1}" `shouldSatisfy` isRight
