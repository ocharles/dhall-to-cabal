{-# language OverloadedStrings #-}

module DhallToCabal.Tests
  ( versionSpec
  )
  where

import Data.Maybe ( fromMaybe )
import Data.Void ( Void )
import Dhall.Core ( Const(..), Expr(..), Chunks(..) )

import qualified Data.Text as StrictText
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Parser
import qualified DhallToCabal
import qualified Distribution.Text as Cabal ( simpleParse )
import qualified Test.Tasty
import qualified Test.Tasty.HUnit


versionSpec :: Test.Tasty.TestTree
versionSpec =
  Test.Tasty.testGroup
    "version"
    [ testExtraction
        "version"
        DhallToCabal.version
        ( Lam
            "Version"
            ( Const Type )
            ( Lam
                "v"
                ( Pi "_" ( Const Type ) ( Pi "_" Text "Version" ) )
                ( App "v" ( TextLit ( Chunks [] "1.0.0" ) ) )
            )
        )
        ( fromMaybe
            ( error "Could not parse version" )
            ( Cabal.simpleParse ( StrictText.unpack "1.0.0" ) )
        ) ]


extract x y =
  either ( const Nothing ) Just ( Dhall.toMonadic ( Dhall.extract x y ) )


testExtraction
  :: ( Eq a, Show a )
  => Test.Tasty.TestName
  -> Dhall.Type a
  -> Dhall.Core.Expr Dhall.Parser.Src Void
  -> a
  -> Test.Tasty.TestTree
testExtraction testName t expr expected =
  Test.Tasty.testGroup
    testName
    [ Test.Tasty.HUnit.testCase
        "original"
        ( Just expected Test.Tasty.HUnit.@=? extract t expr )
    , Test.Tasty.HUnit.testCase
        "alphaNormalize"
        ( Just expected
            Test.Tasty.HUnit.@=?
              extract t ( Dhall.Core.alphaNormalize expr )
        )
    , Test.Tasty.HUnit.testCase
        "betaNormalize"
        ( Just expected
            Test.Tasty.HUnit.@=?
              extract t ( Dhall.Core.normalize expr )
        )
    ]
