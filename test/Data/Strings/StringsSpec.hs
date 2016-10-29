{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Data.Strings.StringsSpec (spec) where

import           Data.Strings
import           Prelude                    hiding ((++))
import           Test.Hspec

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as CB
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as CLB
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT

s = "s" :: String
lt = "lt" :: LT.Text
t = "t" :: T.Text
lb = "lb" :: LB.ByteString
b = "b" :: B.ByteString

spec :: Spec
spec = do
  describe "concatenation operator (++)" $ do
    it "works!" $ do
      "imm" ++ "imm" `shouldBe` ("immimm" :: String)
      "imm" ++ s `shouldBe` ("imms" :: String)
      "imm" ++ lt `shouldBe` ("immlt" :: LT.Text)
      "imm" ++ t `shouldBe` ("immt" :: T.Text)
      "imm" ++ lb `shouldBe` ("immlb" :: LB.ByteString)
      "imm" ++ b `shouldBe` ("immb" :: B.ByteString)
