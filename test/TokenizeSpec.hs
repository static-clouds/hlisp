module TokenizeSpec (spec) where

import HLisp.Tokenize (tokenize, Token (TBoolean, TCharacter, TIdentifier, TNumber, TString, TSymbol))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parsec (parse)

import Test.QuickCheck
import Control.Monad (liftM, liftM2)

letters :: [Char]
letters = "abcdefghijklmnopqrstuvwxyz"
specialInitial :: [Char]
specialInitial = "!$%&*/:<=>?^_~"

genInitial :: Gen Char
genInitial = oneof $ map return $ letters ++ specialInitial

genDigit :: Gen Char
genDigit = choose ('a', 'z')

genSpecialSubsequent :: Gen Char
genSpecialSubsequent = oneof $ map return ['+', '-', '.', '@']

genSubsequent :: Gen Char
genSubsequent = oneof [genInitial, genDigit, genSpecialSubsequent]

genPeculiarIdentifier :: Gen String
genPeculiarIdentifier = oneof $ map return ["+", "-", "..."]

instance Arbitrary Token where
  arbitrary = oneof [ TBoolean <$> elements [True, False]
                    , TIdentifier <$> liftM2 (:) genInitial (listOf genSubsequent)
                    , TIdentifier <$> genPeculiarIdentifier
                    , TNumber <$> choose (0, 100000)
                    , TCharacter <$> liftM2 (:) arbitraryASCIIChar (return [])
                    , TCharacter <$> oneof (map return ["space", "newline"])
                    , TString <$> fmap escape (listOf arbitraryASCIIChar)
                    , TSymbol <$> oneof (map return ["a"])
                    ]
    where
      -- escape " and \ inside quoted strings
      escape "\"" = "\\\""
      escape "\\" = "\\\\"
      escape c    = c
  shrink (TBoolean b) = []
  shrink (TIdentifier s) = [TIdentifier s' | s' <- shrink s]
  shrink (TNumber i) = [TNumber i' | i' <- shrink i]
  shrink (TCharacter c) = []
  shrink (TString s) = [TString s' | s' <- shrink s]
  shrink (TSymbol s) = []

spec :: Spec
spec = do
  describe "parse literal" $ do
    it "parses an identifier" $ do
      let Right [v] = parse tokenize "" "+"
      v `shouldBe` TIdentifier "+"
