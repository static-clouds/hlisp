module TokenizeSpec (spec) where

import HLisp.Tokenize (tokenize, Token (TIdentifier))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parsec (parse)


spec :: Spec
spec = do
  describe "parse literal" $ do
    it "parses an identifier" $ do
      let Right [v] = parse tokenize "" "+"
      v `shouldBe` TIdentifier "+"
