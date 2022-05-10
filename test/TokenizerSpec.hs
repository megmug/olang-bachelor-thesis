module TokenizerSpec where

import Data.Either (isRight)
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Tokenizer (tokenize)

spec :: Spec
spec = do
  describe "tokenizer tests" $ do
    it "can tokenize simple assignment command" $ do
      tokenize "hello := 1 + 1" `shouldSatisfy` isRight

    it "can tokenize all zero-argument tokens seperated by spaces" $ do
      tokenize "USING CLASS SUBCLASSOF FIELDS INIT INT OBJ PROCEDURE METHOD RETURNS CALL READ WRITE IF THEN WHILE DO ERROR NOT := = , . > < + - * / ( ) [ ] { }"
        `shouldSatisfy` isRight

    it "can tokenize simple program" $ do
      tokenize "DO {INT x x := 1}" `shouldSatisfy` isRight
