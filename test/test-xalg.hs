{-# LANGUAGE QuasiQuotes #-}

module Main where

import XAlg hiding (main)

import Text.Megaparsec
import Text.Megaparsec.String
import Test.Hspec
import Test.QuickCheck


main :: IO ()
main = hspec $ do

  describe "Expr parser" $ do

    it "parsers integer literals as intended" $
      [xexpr| 3 |]
      `shouldBe`
      Lit (Int 3)

    it "understands order of operations" $ do
      [xexpr| 2+3*4^5 |]
        `shouldBe`
        Plus (Lit 2) (Mult (Lit 3) (Power (Lit 4) (Lit 5)))

      [xexpr| 2^3*4+5 |]
        `shouldBe`
        Plus (Mult (Power (Lit 2) (Lit 3)) (Lit 4)) (Lit 5)

    it "allows operators to be space-padded" $
      [xexpr| z^2 - z ^ 5 |]
      `shouldBe`
      Minus (Power (Var "z") (Lit 2)) (Power (Var "z") (Lit 5))

  describe "Primative Expr rewriting" $

    describe "plusZeroRightId:  'x+0 => x'" $ do

      it "works on 'w+0'" $
        rewrite plusZeroRightId [xexpr| w+0 |]
        `shouldBe`
        Just [xexpr| w |]

      it "works on '2+y+0'" $
        rewrite plusZeroRightId [xexpr| 2+y+0 |]
        `shouldBe`
        Just [xexpr| 2+y |]

      it "can be used recursively" $
        rewriteRec plusZeroRightId [xexpr| (y+0)*(8+0)+0 |]
        `shouldBe`
        [xexpr| y*8 |]

  describe "Expr focusing" $ do

    let tpt = initFocus [xexpr| 2+x |]

    it "'moveUp' does nothing when focus is at top node" $
      tpt `shouldBe` moveUp tpt

    it "'moveDown' does nothing when focus is at a node with no children" $
      moveDown tpt
      `shouldBe`
      moveDown (moveDown tpt)

    it "'moveDown' works as intended on '2+x'" $
      (extractFocus . moveDown $ tpt)
      `shouldBe`
      Just 2

    it "'moveRight . moveDown' works as intended on '2+x'" $
      (extractFocus . moveRight . moveDown $ tpt)
      `shouldBe`
      Just (Var "x")
