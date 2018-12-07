module FlagsSpec where

import Test.Hspec

import qualified Flags

tests = do
  it "parses empty params as no flags" $ do
    (fs, as) <- Flags.parse [""]
    fs `shouldBe` []
    as `shouldBe` [""]
  it "parses no flags and args correctly" $ do
    (fs, as) <- Flags.parse ["key=value", "other=else"]
    fs `shouldBe` []
    as `shouldBe` ["key=value", "other=else"]
  it "parses flags and no args correctly" $ do
    (fs, as) <- Flags.parse ["--version", "-h"]
    fs `shouldBe` [Flags.Version, Flags.Help]
    as `shouldBe` []
  it "parses flags and args correctly" $ do
    (fs, as) <- Flags.parse ["--version", "-h", "key=value"]
    fs `shouldBe` [Flags.Version, Flags.Help]
    as `shouldBe` ["key=value"]
  it "uses default config.yaml if none" $ do
    (fs, as) <- Flags.parse ["-c"]
    fs `shouldBe` [Flags.Config "config.yaml"]
    as `shouldBe` []
  it "overrides config.yaml if specified" $ do
    (fs, as) <- Flags.parse ["--config=c.yaml"]
    fs `shouldBe` [Flags.Config "c.yaml"]
    as `shouldBe` []

