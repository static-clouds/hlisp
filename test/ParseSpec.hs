module ParseSpec (spec) where

import HLisp.Parse (literal, VariableValue(I), Exp(Literal))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parsec (parse)


spec :: Spec
spec = do
  describe "parse literal" $ do
    it "parses an integer" $ do
      let Right (Literal _ parsedValue) = parse literal "" "10"
      parsedValue `shouldBe` I 10
