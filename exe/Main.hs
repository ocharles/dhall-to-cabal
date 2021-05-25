{-# language LambdaCase #-}
{-# language NoMonomorphismRestriction #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language ViewPatterns #-}

module Main ( main ) where

import Control.Applicative ( (<**>), optional, (<|>) )
import Control.Monad ( join )
import Data.Char ( isAlphaNum )
import Data.Foldable ( asum, foldl' )
import Data.Function ( (&) )
import Data.Maybe ( fromMaybe )
import Data.String ( fromString )
import Data.Version ( showVersion )
import Lens.Micro ( set )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>), (<.>), addTrailingPathSeparator, normalise, takeDirectory )

import CabalToDhall ( KnownDefault, getDefault, resolvePreludeVar )
import DhallLocation ( preludeLocation, typesLocation, dhallFromGitHub )
import DhallToCabal
import DhallToCabal.FactorType ( KnownType(..), factored, mapWithBindings )
import DhallToCabal.Util ( relativeTo )
import qualified Paths_dhall_to_cabal as Paths

import qualified Data.Text.IO as StrictText
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Dhall
import qualified Dhall.Core as Dhall
import qualified Dhall.Core as Expr ( Expr(..) )
import qualified Dhall.Lint as Lint
import qualified Dhall.Parser
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Options.Applicative as OptParse
import qualified System.IO


data Command
  = RunDhallToCabal DhallToCabalOptions
  | PrintType PrintTypeOptions
  | PrintDefault PrintDefaultOptions
  | PrintVersion


shouldBeImported :: KnownType -> Bool
shouldBeImported Extension = True
shouldBeImported LicenseId = True
shouldBeImported LicenseExceptionId = True
shouldBeImported _ = False


data DhallToCabalOptions = DhallToCabalOptions
  { dhallFilePath :: Maybe String
  , explain :: Bool
  , outputDirection :: OutputDirection
  }


data OutputDirection
  = OutputStdout
  | OutputDir RelativeDir


data RelativeDir
  = RelativeToCWD FilePath
  | RelativeToInput FilePath


dhallToCabalOptionsParser :: OptParse.Parser DhallToCabalOptions
dhallToCabalOptionsParser =
  DhallToCabalOptions
    <$>
      optional
        ( OptParse.argument
            OptParse.str
            ( mconcat
                [ OptParse.metavar "<dhall input file>"
                , OptParse.help "The Dhall expression to convert to a Cabal file"
                ]
            )
        )
    <*>
      OptParse.flag
        False
        True
        ( mconcat
            [ OptParse.long "explain"
            , OptParse.help "Provide explanations to type Dhall syntax and type errors."
            ]
        )
    <*>
      outputDirectionParser
  where
  -- Note order: the --output-dir-input does not fail because it has a default value.
  outputDirectionParser =
        OptParse.flag'
          OutputStdout
          ( mconcat
            [ OptParse.long "output-stdout"
            , OptParse.help "Write the .cabal file to standard output."
            ]
          )

    <|> OutputDir . RelativeToCWD <$> OptParse.strOption
          ( mconcat
              [ OptParse.long "output-dir-cwd"
              , OptParse.metavar "DIR"
              , OptParse.help "Write the resulting .cabal file to DIR/NAME.cabal, where NAME is taken from the .cabal file's 'name' field, and DIR is relative to the current working directory."
              ]
          )

    <|> OutputDir . RelativeToInput <$> OptParse.strOption
          ( mconcat
              [ OptParse.long "output-dir-input"
              , OptParse.metavar "DIR"
              , OptParse.help "Write the resulting .cabal file to DIR/NAME.cabal, where NAME is taken from the .cabal file's 'name' field, and DIR is relative to the directory containing the input Dhall file (or the current working directory if the input is from standard input)."
              , OptParse.value "."
              ]
          )


data PrintTypeOptions = PrintTypeOptions
  { typeToPrint :: KnownType
  , selfContained :: Bool
  }


printTypeOptionsParser :: OptParse.Parser PrintTypeOptions
printTypeOptionsParser =
  PrintTypeOptions
    <$>
      OptParse.option OptParse.auto modifiers
    <*>
      OptParse.flag
        False
        True
        ( mconcat
            [ OptParse.long "self-contained"
            , OptParse.help "Emit self-contained types. Without this flag, some large enumerations are referenced by import."
            ]
        )

  where

    modifiers =
      mconcat
        [ OptParse.long "print-type"
        , OptParse.help "Print out the description of a type. For a full description, try --print-type Package"
        , OptParse.metavar "TYPE"
        ]



printVersionParser :: OptParse.Parser ()
printVersionParser =
  OptParse.flag'
    ()
    ( mconcat
      [ OptParse.long "version"
      , OptParse.help "Display dhall-to-cabal's version and exit."
      ]
    )


data PrintDefaultOptions =
  PrintDefaultOptions
    { defaultToPrint :: KnownDefault }


printDefaultOptionsParser :: OptParse.Parser PrintDefaultOptions
printDefaultOptionsParser =
  PrintDefaultOptions
    <$>
      OptParse.option OptParse.auto
        ( mconcat
            [ OptParse.long "print-default"
            , OptParse.help "Print out the default values for a type, as found in prelude.defaults"
            , OptParse.metavar "TYPE"
            ]
        )


runDhallToCabal :: DhallToCabalOptions -> IO ()
runDhallToCabal DhallToCabalOptions { dhallFilePath, explain, outputDirection } = do
  source <-
    case dhallFilePath of
      Nothing ->
        StrictText.getContents

      Just filePath ->
        StrictText.readFile filePath

  let
    settings = Dhall.defaultInputSettings
      & set Dhall.rootDirectory inputDir
      & set Dhall.sourceName ( fromMaybe "(STDIN)" dhallFilePath )

  pkgDesc <- explaining ( dhallToCabal settings source )
  let
    rendered =
      addWarningHeader
        ( Cabal.showGenericPackageDescription pkgDesc )

  case outputDirection of

    OutputStdout ->
      putStrLn rendered

    OutputDir _ -> do
      createDirectoryIfMissing True destDir
      let dest = destDir </> pkgDescName pkgDesc <.> "cabal"
      putStrLn ( "Writing to " ++ normalise dest ++ "." )
      writeFile dest rendered

  where
    inputDir = maybe "." takeDirectory dhallFilePath

    destDir =
      case outputDirection of
        OutputStdout ->
          "."
        OutputDir (RelativeToCWD dir) ->
          dir
        OutputDir (RelativeToInput relativeDir) ->
          inputDir </> relativeDir

    explaining =
      if explain then Dhall.detailed else id

    pkgDescName = Cabal.unPackageName . Cabal.pkgName . Cabal.package . Cabal.packageDescription

    -- Make sure that the displayed source path is copy-pasteable to
    -- the command line, and won't break the generated Cabal
    -- file. This is a somewhat conservative check.
    isAcceptableCommentChar c =
      isAlphaNum c || c == '.' || c == '/' || c == '_' || c == '-'

    -- These regeneration instructions assume that (a) if we are
    -- writing to stdout, the user is redirecting it a file in the
    -- current directory, (b) the generated file is not moved after we
    -- produce it, (c) there are no symlinks breaking .. components,
    -- and (d) they will run the regeneration instructions from the
    -- directory of the generated .cabal file.
    --
    -- These assumptions are necessarily approximations, but they're a
    -- reasonable guess at the common case and at least give the user
    -- something to go on.
    regenerationInstructions =
      case dhallFilePath of
        Just filePath | all isAcceptableCommentChar filePath ->
          [ "-- Instead, edit the source Dhall file, namely"
          , "-- '" ++ filePath ++ "', and re-generate this file by running"
          , "-- 'dhall-to-cabal -- " ++ sourcePath ++ "'."
          ]
          where
            sourcePath =
              relativeTo ( addTrailingPathSeparator destDir ) filePath
        _ ->
          [ "-- Instead, edit the source Dhall file (which may have the"
          , "-- '.dhall' extension) and re-run dhall-to-cabal, passing "
          , "-- the source file's name as its argument."
          ]

    -- Starting with Cabal 2.2, the cabal-version field *has* to be
    -- the first thing in the .cabal file. So split that off and plonk
    -- the warning header after it.
    addWarningHeader str =
      case lines str of
        [] ->
          error "addWarningHeader: no cabal-version line found?"
        cabalVersion : rest ->
          unlines $ concat
            [ [ cabalVersion
              , "-- * * * * * * * * * * * * WARNING * * * * * * * * * * * *"
              , "-- This file has been AUTO-GENERATED by dhall-to-cabal."
              , "--"
              , "-- Do not edit it by hand, because your changes will be over-written!"
              , "--"
              ]
            , regenerationInstructions
            , [ "-- * * * * * * * * * * * * WARNING * * * * * * * * * * * *" ]
            , rest
            ]


main :: IO ()
main = do
  command <-
    OptParse.execParser opts

  case command of
    RunDhallToCabal options ->
      runDhallToCabal options

    PrintType options ->
      printType options

    PrintDefault options ->
      printDefault options

    PrintVersion ->
      printVersion

  where

  parser =
    asum
      [ RunDhallToCabal <$> dhallToCabalOptionsParser
      , PrintType <$> printTypeOptionsParser
      , PrintDefault <$> printDefaultOptionsParser
      , PrintVersion <$ printVersionParser
      ]

  opts =
    OptParse.info ( parser <**> OptParse.helper ) modifiers

  modifiers =
    mconcat
      [ OptParse.progDesc "Generate Cabal files from Dhall expressions"
      , OptParse.footer
          ( "The default behaviour is to generate a .cabal file in the "
         ++ "directory of the input file with a name corresponding to the "
         ++ "'name' field of the generated package description (i.e., the "
         ++ "same behaviour as specifying '--output-dir-input .'). In the "
         ++ "case of reading from standard input, the .cabal file will be "
         ++ "placed in the current working directory."
          )
      ]



-- Shamelessly taken from dhall-format

opts :: Pretty.LayoutOptions
opts =
  Pretty.defaultLayoutOptions
    { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0 }


printType :: PrintTypeOptions -> IO ()
printType PrintTypeOptions { .. } = do
  Pretty.renderIO
    System.IO.stdout
    ( Pretty.layoutSmart opts
        ( Pretty.pretty ( Lint.lint result ) )
    )

  putStrLn ""

  where

    linkedType =
      join . mapWithBindings linkType . factored

    linkType var t =
      let
        name = fromString ( show t )
      in if shouldBeImported t && not selfContained
         then Expr.Var ( var "types" ) `Expr.Field` Dhall.makeFieldSelection name
         else Expr.Var ( var name )

    bindTypes expr =
      foldl' bindType expr [ minBound .. maxBound ]

    bindType expr t =
      Expr.Let
        ( Dhall.makeBinding
          ( fromString ( show t ) )
          ( linkedType t )
        )
        expr

    bindImport =
      Expr.Let
        ( Dhall.makeBinding
            "types"
            ( Expr.Embed ( typesLocation dhallFromGitHub ) )
        )

    -- Unconditionally add everything, since lint will remove the
    -- redundant bindings.
    result =
      bindImport
        ( bindTypes ( linkedType typeToPrint ) )


printVersion :: IO ()
printVersion = do
  putStrLn ( "dhall-to-cabal version " ++ showVersion Paths.version )


printDefault :: PrintDefaultOptions -> IO ()
printDefault PrintDefaultOptions {..} = do
  Pretty.renderIO
    System.IO.stdout
    ( Pretty.layoutSmart opts
        ( Pretty.pretty ( Lint.lint ( withPreludeImport expr ) ) )
    )

  putStrLn ""

  where
    withPreludeImport =
      Expr.Let
        ( Dhall.makeBinding
            "prelude"
            ( Expr.Embed ( preludeLocation dhallFromGitHub ) )
        )

    expr :: Expr.Expr Dhall.Parser.Src Dhall.Import
    expr = getDefault
             ( typesLocation dhallFromGitHub )
             resolvePreludeVar defaultToPrint
