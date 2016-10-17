-- File: tests/Codec/Binary/ColdwidowSpec.hs

module Codec.Binary.ColdwidowSpec (spec) where

import Test.Hspec
import Codec.Binary.Coldwidow

import Data.ByteString.Lazy (singleton, pack)

spec :: Spec
spec = do
  describe "encode" $ do
    it "returns character \"0\" when encoding value 0" $
      encode 0 `shouldBe` "0"
    it "returns character \":\" when encoding vlaue 44" $
      encode 44 `shouldBe` ":"
    it "returns characters \"10\" when encoding value 45" $
      encode 45 `shouldBe` "10"
    it "returns charcters \"11\" when encoding value 46" $
      encode 46 `shouldBe` "11"
    it "returns charcters \"1:\" when encoding value 89" $
      encode 89 `shouldBe` "1:"
    it "returns characters \":0\" when encoding value 45*44" $
      encode (45*44) `shouldBe` ":0"
    it "returns characters \"::\" when encoding value 45*44+44" $
      encode (45*44+44) `shouldBe` "::"
      
  describe "decode" $ do
    it "decodes \"0\" as 0" $
      decode "0" `shouldBe` 0
    it "decodes \":\" as 44" $
      decode ":" `shouldBe` 44
    it "decodes \"10\" as 45" $
      decode "10" `shouldBe` 45
    it "decodes \"11\" as 46" $
      decode "11" `shouldBe` 46
    it "decodes \"1:\" as 89" $
      decode "1:" `shouldBe` 89
    it "decodes \":0\" as 45*44" $
      decode ":0" `shouldBe` (45*44)
    it "decodes \"::\" as 45*44+44" $
      decode "::" `shouldBe` (45*44+44)
    it "decodes \"Q\" as 26" $
      decode "Q" `shouldBe` 26

  describe "packInteger" $ do
    it "packs [0] as 0" $
      packInteger (singleton 0) `shouldBe` 0
    it "packs [0, 0] as 0" $
      packInteger (pack [0, 0]) `shouldBe` 0
    it "packs [1] as 1" $
      packInteger (singleton 1) `shouldBe` 1
    it "packs [0, 1] as 1" $
      packInteger (pack [0, 1]) `shouldBe` 1
    it "packs [1, 0] as 256" $
      packInteger (pack [1, 0]) `shouldBe` 256

  describe "unpackInteger" $ do
    it "unpacks 0 as [0]" $
      unpackInteger 0 `shouldBe` (singleton 0)
    it "unpacks 1 as [1]" $
      unpackInteger 1 `shouldBe` (singleton 1)
    it "unpacks 256 as [1, 0]" $
      unpackInteger 256 `shouldBe` (pack [1, 0])
    it "unpacks 65535 as [255, 255]" $
      unpackInteger 65535 `shouldBe` (pack [255, 255])
    it "unpacks 1+2*256+3*256*256+4*256*256*256 as [4, 3, 2, 1]" $
      unpackInteger (1+2*256+3*256*256+4*256*256*256) `shouldBe` (pack [4, 3, 2, 1])
