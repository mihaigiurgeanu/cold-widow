-- File: tests/Codec/Binary/ColdwidowSpec.hs

module Codec.Binary.ColdwidowSpec (spec) where

import Test.Hspec
import Codec.Binary.Coldwidow

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