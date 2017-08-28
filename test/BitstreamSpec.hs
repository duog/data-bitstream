{-# LANGUAGE BinaryLiterals #-}

module BitstreamSpec where

import Test.Tasty.Hspec

import Data.Bitstream

spec_stream :: Spec
spec_stream = do
  describe "Buff" $ do
    it "should add" $ do
      nullBuff `addBuff` nullBuff
        `shouldBe` (Nothing,nullBuff)
      nullBuff `addBuff` (Buff (4,0b10100000))
        `shouldBe` (Nothing, (Buff (4,0b10100000)))
      (Buff (1,0b10000000)) `addBuff` (Buff (1,0b10000000))
        `shouldBe` (Nothing, (Buff (2,0b11000000)))
      (Buff (4,0b10100000)) `addBuff` (Buff (4,0b10100000))
        `shouldBe` (Just 0b10101010, nullBuff)
      (Buff (6,0b10101000)) `addBuff` (Buff (4,0b01010000))
        `shouldBe` (Just 0b10101001, (Buff (2,0b01000000)))
  describe "Stream" $ do
    it "should be a monoid" $ do
      (S []  nullBuff 0 `mappend` S []  (Buff (4,0b10100000)) 4)
        `shouldBe` (S []    (Buff (4,0b10100000)) 4)  
      (S [1] nullBuff 8 `mappend` S [2] (Buff (4,0b10100000)) 12)
        `shouldBe` (S [1,2] (Buff (4,0b10100000)) 20)
      (S []  (Buff (4,0b01010000)) 4 `mappend` S [] (Buff (4,0b10100000)) 4)
        `shouldBe` (S [0b01011010] nullBuff 8)

      --   01010101 101 + 10101010 11001100 000111
      -- = 01010101 10110101 01011001 10000011 1
      (S [0b01010101] (Buff (3,0b10100000)) 11) `mappend` (S [0b10101010, 0b11001100] (Buff (6,0b00011100)) 22)
        `shouldBe` (S [0b01010101,0b10110101,0b01011001,0b10000011] (Buff (1,0b10000000)) 33)

  describe "Bitstream" $ do
    it "should produce word aligned results" $ do
      execBitstream 0 (pure ()) `shouldBe` []
      execBitstream 0 (emitBit False) `shouldBe` [0b00000000]
      execBitstream 0 (emitBit True) `shouldBe`  [0b10000000]
      execBitstream 0 (emitBit True >> emitBit False >> emitBit True) `shouldBe` [0b10100000]
