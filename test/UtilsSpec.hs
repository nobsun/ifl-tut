{-# LANGUAGE OverloadedStrings #-}
module UtilsSpec
  ( spec
  ) where

import Data.String
import qualified Codec.Binary.UTF8.String as U
import qualified Data.ByteString as B
import Test.Hspec
import Text.Show.Unicode
import Utils

newtype UString a = UString a deriving Eq

ustring :: B.ByteString -> UString String
ustring = UString . U.decode . B.unpack

instance IsString a => IsString (UString a) where
  fromString = UString . fromString
  
instance Show a => Show (UString a) where
  show (UString s) = ushow s

spec :: Spec
spec = do
  { describe "space :: Int -> String -- スペース文字を指定個数含む文字列" $ do
    { it "space 0 ==> \"\"" $ space 0 `shouldBe` ""
    ; it "space 4 ==> \"    \"" $ space 4 `shouldBe` "    "
    }
  }
