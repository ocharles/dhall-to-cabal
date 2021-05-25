{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

module CabalToDhall
  ( cabalToDhall
  , parseGenericPackageDescriptionThrows
  , KnownDefault (..)
  , PreludeReference (..)
  , resolvePreludeVar
  , getDefault
  ) where

import Data.Foldable ( foldMap, toList )
import Data.Functor.Contravariant ( (>$<), Contravariant( contramap ) )
import Data.Semigroup ( Semigroup, (<>) )
import Data.Void ( Void, absurd )
import Numeric.Natural ( Natural )

import qualified Data.ByteString as ByteString
import qualified Data.Sequence as Seq
import qualified Data.Text as StrictText
import qualified Data.Text.Encoding as StrictText
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Core as Expr ( Expr(..), Var(..), Chunks(..), makeBinding )
import qualified Dhall.Map as Map
import qualified Dhall.Parser
import qualified Distribution.Compiler as Cabal
import qualified Distribution.License as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.SPDX as SPDX
import qualified Distribution.System as Cabal
import qualified Distribution.Types.Benchmark as Cabal
import qualified Distribution.Types.BenchmarkInterface as Cabal
import qualified Distribution.Types.BuildInfo as Cabal
import qualified Distribution.Types.BuildType as Cabal
import qualified Distribution.Types.CondTree as Cabal
import qualified Distribution.Types.Condition as Cabal
import qualified Distribution.Types.Dependency as Cabal
import qualified Distribution.Types.ExeDependency as Cabal
import qualified Distribution.Types.Executable as Cabal
import qualified Distribution.Types.ExecutableScope as Cabal
import qualified Distribution.Types.ForeignLib as Cabal
import qualified Distribution.Types.ForeignLibOption as Cabal
import qualified Distribution.Types.ForeignLibType as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal
import qualified Distribution.Types.IncludeRenaming as Cabal
import qualified Distribution.Types.LegacyExeDependency as Cabal
import qualified Distribution.Types.Library as Cabal
import qualified Distribution.Types.LibraryName as Cabal
import qualified Distribution.Types.LibraryVisibility as Cabal
import qualified Distribution.Types.Mixin as Cabal
import qualified Distribution.Types.ModuleReexport as Cabal
import qualified Distribution.Types.ModuleRenaming as Cabal
import qualified Distribution.Types.PackageDescription as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.PkgconfigDependency as Cabal
import qualified Distribution.Types.PkgconfigName as Cabal
import qualified Distribution.Types.PkgconfigVersion as Cabal
import qualified Distribution.Types.PkgconfigVersionRange as Cabal
import qualified Distribution.Types.SetupBuildInfo as Cabal
import qualified Distribution.Types.SourceRepo as Cabal
import qualified Distribution.Types.TestSuite as Cabal
import qualified Distribution.Types.TestSuiteInterface as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Version as Cabal
import qualified Language.Haskell.Extension as Cabal

import DhallLocation ( DhallLocation(..) )
import DhallToCabal ( sortExpr )
import DhallToCabal.ConfigTree ( ConfigTree(..) )


dhallString :: String -> Expr.Expr s a
dhallString = Expr.TextLit . Dhall.Core.Chunks [] . StrictText.pack


parseGenericPackageDescriptionThrows
  :: ByteString.ByteString
  -> IO Cabal.GenericPackageDescription
parseGenericPackageDescriptionThrows source =
  case Cabal.runParseResult ( Cabal.parseGenericPackageDescription source ) of
    (_warnings, Left e) -> do
      putStrLn "Could not parse Cabal file: "

      error ( show e )

    (_warnings, Right genericPackageDescription) ->
      return genericPackageDescription


cabalToDhall
  :: DhallLocation
  -> Cabal.GenericPackageDescription
  -> Expr.Expr Dhall.Parser.Src Dhall.Core.Import
cabalToDhall dhallLocation genericPackageDescription
  = Expr.Let
      ( Expr.makeBinding "prelude" ( Expr.Embed ( preludeLocation dhallLocation ) ) )
  $ Expr.Let
      ( Expr.makeBinding "types" ( Expr.Embed ( typesLocation dhallLocation ) ) )
  $ absurd <$>
      Dhall.embed
        genericPackageDescriptionToDhall
        genericPackageDescription


-- Note: the Show instance is used by --print-type.
data KnownDefault
  = CompilerOptions
  | BuildInfo
  | MainLibrary
  | NamedLibrary
  | Executable
  | Benchmark
  | TestSuite
  | Package
  | SourceRepo
  deriving ( Bounded, Enum, Eq, Ord, Read, Show )


data PreludeReference
  = PreludeDefault KnownDefault
  | PreludeConstructorsLicense
  | PreludeConstructorsRepoKind
  | PreludeConstructorsScope
  | PreludeConstructorsLibraryVisibility
  | PreludeV


resolvePreludeVar :: PreludeReference -> Expr.Expr s a
resolvePreludeVar = \case
  PreludeDefault typ ->
    Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "defaults" `Expr.Field` Dhall.Core.makeFieldSelection ( StrictText.pack ( show typ ) )
  PreludeV ->
    Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "v"
  PreludeConstructorsLicense ->
    Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "License"
  PreludeConstructorsRepoKind ->
    Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "RepoKind"
  PreludeConstructorsScope ->
    Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Scope"
  PreludeConstructorsLibraryVisibility ->
    Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "LibraryVisibility"


type Default s a
   = ( PreludeReference -> Expr.Expr s a )
   -> Map.Map StrictText.Text ( Expr.Expr s a )


getDefault
  :: ( Eq s )
  => Dhall.Core.Import
  -> ( PreludeReference -> Expr.Expr s Dhall.Core.Import )
  -> KnownDefault
  -> Expr.Expr s Dhall.Core.Import
getDefault typesLoc resolve typ = withTypesImport expr
  where
    withTypesImport =
      Expr.Let ( Expr.makeBinding "types" ( Expr.Embed typesLoc ) )

    factorBuildInfo fields =
      let
        shared = Map.filter id ( Map.intersectionWith (==) fields ( buildInfoDefault resolve ) )
      in
        if | null shared
             -> Expr.RecordLit (fmap Dhall.Core.makeRecordField fields)
           | null ( Map.difference fields shared )
             -> resolve ( PreludeDefault BuildInfo )
           | otherwise
             -> Expr.Prefer
                  Nothing
                  Dhall.Core.PreferFromCompletion
                  ( resolve ( PreludeDefault BuildInfo ) )
                  ( Expr.RecordLit ( fmap Dhall.Core.makeRecordField ( Map.difference fields shared ) ) )

    expr =
      case typ of
        CompilerOptions ->
          Expr.RecordLit ( Dhall.Core.makeRecordField <$> compilerOptionsDefault resolve )
        BuildInfo ->
          Expr.RecordLit ( Dhall.Core.makeRecordField <$> buildInfoDefault resolve )
        MainLibrary ->
          factorBuildInfo ( libraryDefault False resolve )
        NamedLibrary ->
          factorBuildInfo ( libraryDefault True resolve )
        Executable ->
          factorBuildInfo ( executableDefault resolve )
        Benchmark ->
          factorBuildInfo ( benchmarkDefault resolve )
        TestSuite ->
          factorBuildInfo ( testSuiteDefault resolve )
        Package ->
          Expr.RecordLit ( Dhall.Core.makeRecordField <$> packageDefault resolve )
        SourceRepo ->
          Expr.RecordLit ( Dhall.Core.makeRecordField <$> sourceRepoDefault resolve )


emptyListDefault
  :: StrictText.Text
  -> Expr.Expr s a
  -> ( StrictText.Text, Expr.Expr s a )
emptyListDefault name ty =
  ( name, Expr.ListLit ( Just ( Expr.App Expr.List ty ) ) mempty )


emptyOptionalDefault
  :: StrictText.Text
  -> Expr.Expr s a
  -> ( StrictText.Text, Expr.Expr s a )
emptyOptionalDefault name ty =
  ( name, Expr.App Expr.None ty )


textFieldDefault
  :: StrictText.Text
  -> StrictText.Text
  -> ( StrictText.Text, Expr.Expr s a )
textFieldDefault name def =
  ( name
  , Expr.TextLit ( Dhall.Core.Chunks [] def )
  )


generaliseDeclared =
  Dhall.Core.denote . fmap absurd . Dhall.declared


compilerOptionsDefault :: Default s a
compilerOptionsDefault _resolve =
  ( Map.fromList
    [ emptyListDefault "GHC" Expr.Text
    , emptyListDefault "GHCJS" Expr.Text
    ]
  )


buildInfoDefault :: Default s a
buildInfoDefault resolve = fields
  where
    fields = Map.fromList
      [ emptyListDefault "autogen-modules" Expr.Text
      , emptyListDefault "build-depends" ( generaliseDeclared dependency )
      , emptyListDefault "build-tool-depends" ( generaliseDeclared exeDependency )
      , emptyListDefault "build-tools"
          ( generaliseDeclared legacyExeDependency )
      , ( "buildable", Expr.BoolLit True )
      , emptyListDefault "c-sources" Expr.Text
      , emptyListDefault "cc-options" Expr.Text
      , ( "compiler-options", resolve ( PreludeDefault CompilerOptions ) )
      , emptyListDefault "cpp-options" Expr.Text
      , emptyListDefault "default-extensions" ( generaliseDeclared extension )
      , emptyOptionalDefault "default-language" ( generaliseDeclared language )
      , emptyListDefault "extra-framework-dirs" Expr.Text
      , emptyListDefault "extra-ghci-libraries" Expr.Text
      , emptyListDefault "extra-lib-dirs" Expr.Text
      , emptyListDefault "extra-libraries" Expr.Text
      , emptyListDefault "frameworks" Expr.Text
      , emptyListDefault "hs-source-dirs" Expr.Text
      , emptyListDefault "includes" Expr.Text
      , emptyListDefault "include-dirs" Expr.Text
      , emptyListDefault "install-includes" Expr.Text
      , emptyListDefault "js-sources" Expr.Text
      , emptyListDefault "ld-options" Expr.Text
      , emptyListDefault "other-extensions" ( generaliseDeclared extension )
      , emptyListDefault "other-languages" ( generaliseDeclared language )
      , emptyListDefault "other-modules" Expr.Text
      , emptyListDefault "pkgconfig-depends" ( generaliseDeclared pkgconfigDependency )
      , ( "profiling-options", resolve ( PreludeDefault CompilerOptions ) )
      , ( "shared-options", resolve ( PreludeDefault CompilerOptions ) )
      , ( "static-options", resolve ( PreludeDefault CompilerOptions ) )
      , emptyListDefault "mixins" ( generaliseDeclared mixin )
      , emptyListDefault "asm-options" Expr.Text
      , emptyListDefault "asm-sources" Expr.Text
      , emptyListDefault "cmm-options" Expr.Text
      , emptyListDefault "cmm-sources" Expr.Text
      , emptyListDefault "cxx-options" Expr.Text
      , emptyListDefault "cxx-sources" Expr.Text
      , emptyListDefault "virtual-modules" Expr.Text
      , emptyListDefault "extra-lib-flavours" Expr.Text
      , emptyListDefault "extra-bundled-libs" Expr.Text
      , emptyListDefault "autogen-includes" Expr.Text
      , emptyListDefault "extra-dyn-lib-flavours" Expr.Text
      ]


libraryVisibility :: Dhall.Encoder Cabal.LibraryVisibility
libraryVisibility =
  Dhall.Encoder
    { Dhall.embed = \case
        Cabal.LibraryVisibilityPublic ->
            Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "LibraryVisibility" `Expr.Field` Dhall.Core.makeFieldSelection "public"
        Cabal.LibraryVisibilityPrivate ->
            Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "LibraryVisibility" `Expr.Field` Dhall.Core.makeFieldSelection "private"
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "LibraryVisibility"
    }


libraryDefault :: Bool -> Default s a
libraryDefault named resolve = buildInfoDefault resolve <> specificFields
  where
    specificFields = Map.fromList
      [ emptyListDefault "exposed-modules" Expr.Text
      , emptyListDefault "other-modules" Expr.Text
      , emptyListDefault "reexported-modules"
          ( generaliseDeclared moduleReexport )
      , emptyListDefault "signatures" Expr.Text
      , ( "visibility"
        , ( resolve PreludeConstructorsLibraryVisibility `Expr.Field` visibility )
        )
      ]
    visibility = if named
      then Dhall.Core.makeFieldSelection "private"
      else Dhall.Core.makeFieldSelection "public"


benchmarkDefault :: Default s a
benchmarkDefault = buildInfoDefault


testSuiteDefault :: Default s a
testSuiteDefault = buildInfoDefault


executableDefault :: Default s a
executableDefault resolve = buildInfoDefault resolve <> specificFields
  where
    specificFields =
      Map.singleton "scope"
        ( resolve PreludeConstructorsScope `Expr.Field` Dhall.Core.makeFieldSelection "Public" )


packageDefault :: Default s a
packageDefault resolve = fields
  where
    named name typ = Expr.Record
      ( Dhall.Core.makeRecordField
        <$> Map.fromList
          [ ( "name"
            , Expr.Text
            )
          , ( name
            , Expr.Pi
                Nothing
                "config"
                ( Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Config" )
                ( generaliseDeclared typ )
            )
          ]
      )

    fields = Map.fromList
      [ textFieldDefault "author" ""
      , emptyListDefault "flags" ( generaliseDeclared flag )
      , emptyListDefault "benchmarks" ( named "benchmark" benchmark )
      , textFieldDefault "bug-reports" ""
      , emptyOptionalDefault "build-type"
          ( generaliseDeclared buildType )
      , ( "cabal-version"
        , Expr.App
            ( resolve PreludeV )
            ( Expr.TextLit ( Dhall.Core.Chunks [] "2.2" ) )
        )
      , textFieldDefault "category" ""
      , textFieldDefault "copyright" ""
      , textFieldDefault "data-dir" ""
      , emptyListDefault "data-files" Expr.Text
      , textFieldDefault "description" ""
      , emptyListDefault "executables" ( named "executable" executable )
      , emptyListDefault "extra-doc-files" Expr.Text
      , emptyListDefault "extra-source-files" Expr.Text
      , emptyListDefault "extra-tmp-files" Expr.Text
      , emptyListDefault "foreign-libraries" ( named "foreign-lib" foreignLibrary )
      , textFieldDefault "homepage" ""
      , emptyOptionalDefault "library"
          ( Expr.Pi
              Nothing
              "config"
              ( Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Config" )
              ( generaliseDeclared ( library False ) )
          )
      , ( "license"
        , resolve PreludeConstructorsLicense `Expr.Field` Dhall.Core.makeFieldSelection "AllRightsReserved"
        )
      , emptyListDefault "license-files" Expr.Text
      , textFieldDefault "maintainer" ""
      , textFieldDefault "package-url" ""
      , emptyListDefault "source-repos" ( generaliseDeclared sourceRepo )
      , textFieldDefault "stability" ""
      , emptyListDefault "sub-libraries" ( named "library" ( library True ) )
      , textFieldDefault "synopsis" ""
      , emptyListDefault "test-suites" ( named "test-suite" testSuite )
      , emptyListDefault "tested-with"
          ( Expr.Record
            ( Dhall.Core.makeRecordField
              <$> Map.fromList
                [ ( "compiler", generaliseDeclared compilerFlavor )
                , ( "version", generaliseDeclared versionRange )
                ]
            )
          )
      , emptyListDefault "x-fields"
          ( Expr.Record
            ( Dhall.Core.makeRecordField
              <$> Map.fromList
                [ ( "_1", Expr.Text ), ( "_2", Expr.Text ) ]
            )
          )
      , emptyOptionalDefault "custom-setup"
          ( generaliseDeclared setupBuildInfo )
      ]


sourceRepoDefault :: Default s a
sourceRepoDefault resolve = fields
  where
    fields = Map.fromList
      [ emptyOptionalDefault "type" ( generaliseDeclared repoType )
      , emptyOptionalDefault "location" Expr.Text
      , emptyOptionalDefault "module" Expr.Text
      , emptyOptionalDefault "branch" Expr.Text
      , emptyOptionalDefault "tag" Expr.Text
      , emptyOptionalDefault "subdir" Expr.Text
      , ( "kind"
        , resolve PreludeConstructorsRepoKind `Expr.Field` Dhall.Core.makeFieldSelection "RepoHead"
        )
      ]


data DefaultComparison s a
  = DefaultComparisonMatch
  | DefaultComparisonReplace ( Expr.Expr s a )
  deriving ( Show )


extractDefaultComparisonReplace
  :: DefaultComparison s a
  -> Maybe ( Expr.Expr s a )
extractDefaultComparisonReplace DefaultComparisonMatch =
  Nothing
extractDefaultComparisonReplace ( DefaultComparisonReplace expr ) =
  Just expr


nonDefaultFields
  :: ( Eq a )
  => Map.Map StrictText.Text ( Expr.Expr s a )
  -> Map.Map StrictText.Text ( Expr.Expr s a )
  -> Map.Map StrictText.Text ( Expr.Expr s a )
nonDefaultFields defs fields =
  let
    withoutDefaults = Map.difference fields defs
    compared = Map.intersectionWith compareToDefault defs fields
    changed = Map.mapMaybe extractDefaultComparisonReplace compared
  in
    withoutDefaults <> changed


compareToDefault :: ( Eq a ) => Expr.Expr s a -> Expr.Expr s a -> DefaultComparison s a
compareToDefault def expr | Dhall.Core.judgmentallyEqual def expr =
  DefaultComparisonMatch
compareToDefault _ expr =
  DefaultComparisonReplace expr


withDefault :: ( Eq a ) => KnownDefault -> Default s a -> Expr.Expr s a -> Expr.Expr s a
withDefault typ defs ( Expr.RecordLit fields ) =
  let
    nonDefaults = nonDefaultFields ( defs resolvePreludeVar ) ( Dhall.Core.recordFieldValue <$> fields )
    name = StrictText.pack ( show typ )
  in
    if null nonDefaults
    then Expr.Var ( Expr.V "prelude" 0 ) `Expr.Field` Dhall.Core.makeFieldSelection "defaults" `Expr.Field` Dhall.Core.makeFieldSelection name
    else Expr.Prefer
           Nothing
           Dhall.Core.PreferFromCompletion
           ( Expr.Var ( Expr.V "prelude" 0 ) `Expr.Field` Dhall.Core.makeFieldSelection "defaults" `Expr.Field` Dhall.Core.makeFieldSelection name )
           ( Expr.RecordLit ( Dhall.Core.makeRecordField <$> nonDefaults ) )
withDefault _ _ expr =
  expr


newtype RecordInputType a =
  RecordInputType
    { _unRecordInputType ::
        Map.Map Dhall.Text ( Dhall.Encoder a )
    }
  deriving ( Semigroup, Monoid )


instance Contravariant RecordInputType where
  contramap f ( RecordInputType map ) =
    RecordInputType ( fmap ( contramap f ) map )


recordField :: Dhall.Text -> Dhall.Encoder a -> RecordInputType a
recordField k v =
  RecordInputType ( Map.singleton k v )


runRecordInputType :: RecordInputType a -> Dhall.Encoder a
runRecordInputType ( RecordInputType m ) =
  Dhall.Encoder
    { Dhall.embed =
        \a -> sortExpr ( Expr.RecordLit ( fmap ( \t -> Dhall.Core.makeRecordField ( Dhall.embed t a ) ) m ) )
    , Dhall.declared = sortExpr ( Expr.Record ( fmap ( Dhall.Core.makeRecordField . Dhall.declared ) m ) )
    }


runRecordInputTypeWithDefault :: KnownDefault -> Default Dhall.Parser.Src Void -> RecordInputType a -> Dhall.Encoder a
runRecordInputTypeWithDefault typ def m =
  let
    Dhall.Encoder embed declared = runRecordInputType m
  in
    Dhall.Encoder ( withDefault typ def . embed ) declared


genericPackageDescriptionToDhall
  :: Dhall.Encoder Cabal.GenericPackageDescription
genericPackageDescriptionToDhall =
  let
    named k v =
      listOf
        ( runRecordInputType
            ( mconcat
                [ fst >$< recordField "name" unqualComponentName
                , snd >$< recordField k v
                ]
            )
        )

  in
  runRecordInputTypeWithDefault Package packageDefault
    ( mconcat
        [ Cabal.packageDescription >$< packageDescriptionToRecord
        , recordField "flags" ( Cabal.genPackageFlags >$< ( listOf flag ) )
        , recordField "library" ( Cabal.condLibrary >$< maybeToDhall ( condTree ( library False ) ) )
        , recordField "sub-libraries" ( Cabal.condSubLibraries >$< named "library" ( condTree ( library True ) ) )
        , recordField "foreign-libraries" ( Cabal.condForeignLibs >$< named "foreign-lib" ( condTree foreignLibrary ) )
        , recordField "executables" ( Cabal.condExecutables >$< named "executable" ( condTree executable ) )
        , recordField "test-suites" ( Cabal.condTestSuites >$< named "test-suite" ( condTree testSuite ) )
        , recordField "benchmarks" ( Cabal.condBenchmarks >$< named "benchmark" ( condTree benchmark ) )
        ]
    )


packageDescriptionToRecord
  :: RecordInputType Cabal.PackageDescription
packageDescriptionToRecord =
  mconcat
    [ contramap Cabal.package packageIdentifierToRecord
    , recordField "source-repos" ( contramap Cabal.sourceRepos ( listOf sourceRepo ) )
    , recordField "cabal-version" ( contramap Cabal.specVersionRaw specVersion )
    , recordField "build-type" ( contramap Cabal.buildTypeRaw ( maybeToDhall buildType ) )
    , recordField "license" ( contramap Cabal.licenseRaw licenseToDhall )
    , recordField "license-files" ( contramap Cabal.licenseFiles ( listOf stringToDhall ) )
    , recordField "copyright" ( contramap Cabal.copyright stringToDhall )
    , recordField "maintainer" ( contramap Cabal.maintainer stringToDhall )
    , recordField "author" ( contramap Cabal.author stringToDhall )
    , recordField "stability" ( contramap Cabal.stability stringToDhall )
    , recordField "tested-with" ( contramap Cabal.testedWith ( listOf compiler ) )
    , recordField "homepage" ( contramap Cabal.homepage stringToDhall )
    , recordField "package-url" ( contramap Cabal.pkgUrl stringToDhall )
    , recordField "bug-reports" ( contramap Cabal.bugReports stringToDhall )
    , recordField "synopsis" ( contramap Cabal.synopsis stringToDhall )
    , recordField "description" ( contramap Cabal.description stringToDhall )
    , recordField "category" ( contramap Cabal.category stringToDhall )
    , recordField "custom-setup" ( contramap Cabal.setupBuildInfo ( maybeToDhall setupBuildInfo ) )
    , recordField "data-files" ( contramap Cabal.dataFiles ( listOf stringToDhall ) )
    , recordField "data-dir" ( contramap Cabal.dataDir stringToDhall )
    , recordField "extra-source-files" ( contramap Cabal.extraSrcFiles ( listOf stringToDhall ) )
    , recordField "extra-tmp-files" ( contramap Cabal.extraTmpFiles ( listOf stringToDhall ) )
    , recordField "extra-doc-files" ( contramap Cabal.extraDocFiles ( listOf stringToDhall ) )
    , recordField
        "x-fields"
        ( Cabal.customFieldsPD
            >$<
              listOf
                ( runRecordInputType
                    ( mconcat
                        [ fst >$< recordField "_1" stringToDhall
                        , snd >$< recordField "_2" stringToDhall
                        ]
                    )
                )
        )
    ]


packageIdentifierToRecord
  :: RecordInputType Cabal.PackageIdentifier
packageIdentifierToRecord =
  mconcat
    [ recordField "name" ( contramap Cabal.pkgName packageNameToDhall )
    , recordField "version" ( contramap Cabal.pkgVersion versionToDhall )
    ]


packageNameToDhall :: Dhall.Encoder Cabal.PackageName
packageNameToDhall =
  contramap Cabal.unPackageName stringToDhall


versionToDhall :: Dhall.Encoder Cabal.Version
versionToDhall =
  Dhall.Encoder
    { Dhall.embed =
        Expr.App ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "v" )
          . Dhall.embed stringToDhall
          . show
          . Cabal.pretty
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Version"
    }


stringToDhall :: Dhall.Encoder String
stringToDhall =
  contramap StrictText.pack Dhall.inject

licenseToDhall :: Dhall.Encoder (Either SPDX.License Cabal.License)
licenseToDhall =
  Dhall.Encoder
    { Dhall.embed = \l ->
        case l of
          Right ( Cabal.GPL v ) ->
            license "GPL" ( Dhall.embed ( maybeToDhall versionToDhall ) v )
          Right ( Cabal.AGPL v ) ->
            license "AGPL" ( Dhall.embed ( maybeToDhall versionToDhall ) v )
          Right ( Cabal.LGPL v ) ->
            license "LGPL" ( Dhall.embed ( maybeToDhall versionToDhall ) v )
          Right Cabal.BSD2 ->
            licenseNullary "BSD2"
          Right Cabal.BSD3 ->
            licenseNullary "BSD3"
          Right Cabal.BSD4 ->
            licenseNullary "BSD4"
          Right Cabal.MIT ->
            licenseNullary "MIT"
          Right Cabal.ISC ->
            licenseNullary "ISC"
          Right ( Cabal.MPL v ) ->
            license "MPL" ( Dhall.embed versionToDhall v )
          Right ( Cabal.Apache v ) ->
            license "Apache" ( Dhall.embed ( maybeToDhall versionToDhall ) v )
          Right Cabal.PublicDomain ->
            licenseNullary "PublicDomain"
          Right Cabal.AllRightsReserved ->
            licenseNullary "AllRightsReserved"
          -- Note: SPDX.NONE is what Cabal reports for a file without
          -- a 'license' field, even for pre-2.2 spec versions.
          Left SPDX.NONE ->
            licenseNullary "AllRightsReserved"
          Right Cabal.UnspecifiedLicense ->
            licenseNullary "Unspecified"
          Right ( Cabal.UnknownLicense "UnspecifiedLicense" ) ->
            licenseNullary "Unspecified"
          Right ( Cabal.UnknownLicense l ) ->
            license "Unknown" ( Expr.TextLit (Expr.Chunks [] (StrictText.pack l)) )
          Right Cabal.OtherLicense ->
            licenseNullary "Other"
          Left ( SPDX.License x ) ->
            license "SPDX" ( Dhall.embed spdxLicenseExpressionToDhall x )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "License"
    }
  where
    license name =
      Expr.App
        ( Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "License"
                           `Expr.Field` Dhall.Core.makeFieldSelection name )
    licenseNullary name =
      Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "License"
                       `Expr.Field` Dhall.Core.makeFieldSelection name

spdxLicenseExpressionToDhall :: Dhall.Encoder SPDX.LicenseExpression
spdxLicenseExpressionToDhall =
    Dhall.Encoder
    { Dhall.embed =
        let
          go lexp = case lexp of
            SPDX.ELicense ( SPDX.ELicenseId ident ) exceptionMay ->
              Expr.App
                ( Expr.App
                    ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "SPDX" `Expr.Field` Dhall.Core.makeFieldSelection "license" )
                    ( Dhall.embed spdxLicenseIdToDhall ident )
                )
                ( Dhall.embed ( maybeToDhall spdxLicenseExceptionIdToDhall ) exceptionMay )
            SPDX.ELicense (SPDX.ELicenseIdPlus ident) exceptionMay ->
              Expr.App
                ( Expr.App
                    ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "SPDX" `Expr.Field` Dhall.Core.makeFieldSelection "licenseVersionOrLater" )
                    ( Dhall.embed spdxLicenseIdToDhall ident )
                )
                ( Dhall.embed ( maybeToDhall spdxLicenseExceptionIdToDhall ) exceptionMay )
            SPDX.ELicense (SPDX.ELicenseRef ref) exceptionMay ->
              case SPDX.licenseDocumentRef ref of
                Nothing ->
                  Expr.App
                    ( Expr.App
                        ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "SPDX" `Expr.Field` Dhall.Core.makeFieldSelection "ref" )
                        ( Dhall.embed stringToDhall ( SPDX.licenseRef ref ) )
                    )
                    ( Dhall.embed ( maybeToDhall spdxLicenseExceptionIdToDhall ) exceptionMay )
                Just file ->
                  Expr.App
                    ( Expr.App
                        ( Expr.App
                            ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "SPDX" `Expr.Field` Dhall.Core.makeFieldSelection "refWithFile" )
                            ( Dhall.embed stringToDhall ( SPDX.licenseRef ref ) )
                        )
                        ( Dhall.embed stringToDhall file )
                    )
                    ( Dhall.embed ( maybeToDhall spdxLicenseExceptionIdToDhall ) exceptionMay )
            SPDX.EOr a b ->
              Expr.App
                ( Expr.App
                    ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "SPDX" `Expr.Field` Dhall.Core.makeFieldSelection "or" )
                    ( go a )
                )
                ( go b )
            SPDX.EAnd a b ->
              Expr.App
                ( Expr.App
                    ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "SPDX" `Expr.Field` Dhall.Core.makeFieldSelection "and" )
                    ( go a )
                )
                ( go b )
        in go
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "SPDX"
    }

spdxLicenseIdToDhall :: Dhall.Encoder SPDX.LicenseId
spdxLicenseIdToDhall =
  Dhall.Encoder
    { Dhall.embed = \ident ->
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "LicenseId" `Expr.Field` Dhall.Core.makeFieldSelection ( identName ident )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "LicenseId"
    }

  where

  identName :: SPDX.LicenseId -> StrictText.Text
  identName e =
    StrictText.pack ( show e )

spdxLicenseExceptionIdToDhall :: Dhall.Encoder SPDX.LicenseExceptionId
spdxLicenseExceptionIdToDhall =
  Dhall.Encoder
    { Dhall.embed = \ident ->
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "LicenseExceptionId" `Expr.Field` Dhall.Core.makeFieldSelection ( identName ident )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "LicenseExceptionId"
    }

  where

  identName :: SPDX.LicenseExceptionId -> StrictText.Text
  identName e =
    StrictText.pack ( show e )


maybeToDhall :: Dhall.Encoder a -> Dhall.Encoder ( Maybe a )
maybeToDhall t =
  Dhall.Encoder
    { Dhall.embed =
        \a -> case a of
            Nothing -> Expr.App Expr.None (Dhall.declared t)
            Just x  -> Expr.Some ( Dhall.embed t x )
    , Dhall.declared = Expr.App Expr.Optional ( Dhall.declared t )
    }


listOf :: Dhall.Encoder a -> Dhall.Encoder [ a ]
listOf t =
  Dhall.Encoder
    { Dhall.embed =
        \a ->
          Expr.ListLit
            ( foldl ( \_ _ -> Nothing ) ( Just ( Expr.App Expr.List ( Dhall.declared t ) ) ) a )
            ( foldMap ( pure . Dhall.embed t ) a )
    , Dhall.declared = Expr.App Expr.List ( Dhall.declared t )
    }


compiler :: Dhall.Encoder ( Cabal.CompilerFlavor, Cabal.VersionRange )
compiler =
  runRecordInputType
    ( mconcat
        [ recordField "compiler" ( contramap fst compilerFlavor )
        , recordField "version" ( contramap snd versionRange )
        ]
    )


compilerFlavor :: Dhall.Encoder Cabal.CompilerFlavor
compilerFlavor =
  let
    constructor k v =
      Expr.App ( Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Compiler"
                                  `Expr.Field` Dhall.Core.makeFieldSelection k ) v
    nullary k =
      Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Compiler"
                       `Expr.Field` Dhall.Core.makeFieldSelection k

  in
  Dhall.Encoder
    { Dhall.embed = \case
        Cabal.Eta ->
          nullary "Eta"

        Cabal.GHC ->
          nullary "GHC"

        Cabal.GHCJS ->
          nullary "GHCJS"

        Cabal.HBC ->
          nullary "HBC"

        Cabal.HaskellSuite v ->
          constructor "HaskellSuite"
          ( Expr.Record ( Map.singleton "_1" ( Dhall.Core.makeRecordField ( dhallString v ) ) ) )

        Cabal.Helium ->
          nullary "Helium"

        Cabal.Hugs ->
          nullary "Hugs"

        Cabal.JHC ->
          nullary "JHC"

        Cabal.LHC ->
          nullary "LHC"

        Cabal.NHC ->
          nullary "NHC"

        Cabal.OtherCompiler v ->
          constructor "OtherCompiler"
          ( Expr.Record ( Map.singleton "_1" ( Dhall.Core.makeRecordField ( dhallString v ) ) ) )

        Cabal.UHC ->
          nullary "UHC"

        Cabal.YHC ->
          nullary "YHC"
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Compiler"
    }


versionRange :: Dhall.Encoder Cabal.VersionRange
versionRange =
  Dhall.Encoder
    { Dhall.embed =
        \versionRange0 ->
          let
            go = Cabal.foldVersionRange
              -- AnyVersion
              ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "anyVersion" )
              -- ThisVersion
              ( \v -> Expr.App
                  ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "thisVersion" )
                  ( Dhall.embed versionToDhall v )
              )
              -- LaterVersion
              ( \v -> Expr.App
                  ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "laterVersion" )
                  ( Dhall.embed versionToDhall v )
              )
              -- EarlierVersion
              ( \v -> Expr.App
                  ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "earlierVersion" )
                  ( Dhall.embed versionToDhall v )
              )
              -- UnionVersionRanges
              ( \a b -> Expr.App
                  ( Expr.App
                      ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "unionVersionRanges" )
                      a
                  )
                  b
              )
              -- IntersectVersionRanges
              ( \a b -> Expr.App
                  ( Expr.App
                      ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "intersectVersionRanges" )
                      a
                  )
                  b
              )

          in
          go ( Cabal.fromVersionIntervals ( Cabal.toVersionIntervals versionRange0 ) )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "VersionRange"
    }


sourceRepo :: Dhall.Encoder Cabal.SourceRepo
sourceRepo =
  ( runRecordInputTypeWithDefault SourceRepo sourceRepoDefault
      ( mconcat
          [ recordField "kind" ( contramap Cabal.repoKind repoKind )
          , recordField "type" ( contramap Cabal.repoType ( maybeToDhall repoType ) )
          , recordField "location" ( contramap Cabal.repoLocation ( maybeToDhall stringToDhall ) )
          , recordField "module" ( contramap Cabal.repoModule ( maybeToDhall stringToDhall ) )
          , recordField "branch" ( contramap Cabal.repoBranch ( maybeToDhall stringToDhall ) )
          , recordField "tag" ( contramap Cabal.repoTag ( maybeToDhall stringToDhall ) )
          , recordField "subdir" ( contramap Cabal.repoSubdir ( maybeToDhall stringToDhall ) )
          ]
      )
  )
  { Dhall.declared =
      Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "SourceRepo"
  }


repoKind :: Dhall.Encoder Cabal.RepoKind
repoKind =
  Dhall.Encoder
    { Dhall.embed = \case
        Cabal.RepoThis ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "RepoKind" `Expr.Field` Dhall.Core.makeFieldSelection "RepoThis"
        Cabal.RepoHead ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "RepoKind" `Expr.Field` Dhall.Core.makeFieldSelection "RepoHead"
        Cabal.RepoKindUnknown str ->
          Expr.App
            ( Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "RepoKind" `Expr.Field` Dhall.Core.makeFieldSelection "RepoKindUnknown" )
            ( Expr.RecordLit ( Map.singleton "_1" ( Dhall.Core.makeRecordField ( dhallString str ) ) ) )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "RepoKind"
    }


repoType :: Dhall.Encoder Cabal.RepoType
repoType =
  Dhall.Encoder
    { Dhall.embed = \case
        Cabal.Darcs ->
          constr "Darcs"
        Cabal.Git ->
          constr "Git"
        Cabal.SVN ->
          constr "SVN"
        Cabal.CVS ->
          constr "CVS"
        Cabal.Mercurial ->
          constr "Mercurial"
        Cabal.GnuArch ->
          constr "GnuArch"
        Cabal.Monotone ->
          constr "Monotone"
        Cabal.Bazaar ->
          constr "Bazaar"
        Cabal.OtherRepoType str ->
          Expr.App
            ( constr "OtherRepoType" )
            ( Expr.RecordLit ( Map.singleton "_1" ( Dhall.Core.makeRecordField ( dhallString str ) ) ) )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "RepoType"
    }
  where
    constr name =
      Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "RepoType"
                       `Expr.Field` Dhall.Core.makeFieldSelection name


specVersion :: Dhall.Encoder ( Either Cabal.Version Cabal.VersionRange )
specVersion =
  Dhall.Encoder
    { Dhall.embed = either ( Dhall.embed versionToDhall ) ( error "Only exact cabal-versions are supported" )
    , Dhall.declared = Dhall.declared versionToDhall
    }


buildType :: Dhall.Encoder Cabal.BuildType
buildType =
  Dhall.Encoder
    { Dhall.embed = \case
        Cabal.Simple ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "BuildType" `Expr.Field` Dhall.Core.makeFieldSelection "Simple"

        Cabal.Configure ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "BuildType" `Expr.Field` Dhall.Core.makeFieldSelection "Configure"

        Cabal.Custom ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "BuildType" `Expr.Field` Dhall.Core.makeFieldSelection "Custom"

        Cabal.Make ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "BuildType" `Expr.Field` Dhall.Core.makeFieldSelection "Make"

    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "BuildType"
    }


setupBuildInfo :: Dhall.Encoder Cabal.SetupBuildInfo
setupBuildInfo =
  ( runRecordInputType
      ( mconcat
          [ recordField "setup-depends" ( contramap Cabal.setupDepends ( listOf dependency ) )
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "SetupBuildInfo"
    }


libraryName :: Dhall.Encoder Cabal.LibraryName
libraryName =
  Dhall.Encoder
    { Dhall.embed = \case
        Cabal.LMainLibName ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "LibraryName" `Expr.Field` Dhall.Core.makeFieldSelection "main-library"
        Cabal.LSubLibName c ->
          Expr.App
            ( Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "LibraryName" `Expr.Field` Dhall.Core.makeFieldSelection "sub-library" )
            ( Dhall.embed unqualComponentName c )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "LibraryName"
    }


dependency :: Dhall.Encoder Cabal.Dependency
dependency =
  ( runRecordInputType
    ( mconcat
        [ recordField "package" ( contramap ( \( Cabal.Dependency p _ _ ) -> p ) packageNameToDhall )
        , recordField "bounds" ( contramap ( \( Cabal.Dependency _ a _ ) -> a ) versionRange )
        , recordField "library-names" ( contramap ( \( Cabal.Dependency _ _ ls ) -> toList ls ) ( listOf libraryName ) )
        ]
    )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Dependency"
    }


flag :: Dhall.Encoder Cabal.Flag
flag =
  runRecordInputType
    ( mconcat
        [ recordField "name" ( contramap Cabal.flagName flagName )
        , recordField "default" ( contramap Cabal.flagDefault Dhall.inject )
        , recordField "description" ( contramap Cabal.flagDescription stringToDhall )
        , recordField "manual" ( contramap Cabal.flagManual Dhall.inject )
        ]
    )


flagName :: Dhall.Encoder Cabal.FlagName
flagName =
  contramap Cabal.unFlagName stringToDhall


library :: Bool -> Dhall.Encoder Cabal.Library
library named =
  ( runRecordInputTypeWithDefault
      ( if named then NamedLibrary else MainLibrary )
      ( libraryDefault named )
      ( mconcat
          [ contramap Cabal.libBuildInfo buildInfoRecord
          , recordField
              "exposed-modules"
              ( contramap Cabal.exposedModules ( listOf moduleName ) )
          , recordField
              "reexported-modules"
              ( contramap Cabal.reexportedModules ( listOf moduleReexport ) )
          , recordField
              "signatures"
              ( contramap Cabal.signatures ( listOf moduleName ) )
          , recordField
              "visibility"
              ( contramap Cabal.libVisibility libraryVisibility )
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Library"
    }


unifyCondTree
  :: ( Monoid a )
  => Cabal.CondTree v x a
  -> ConfigTree ( Cabal.Condition v ) a
unifyCondTree =
  let
    branch
      :: ( Monoid a )
      => Cabal.CondBranch v x a
      -> ConfigTree ( Cabal.Condition v ) a
    branch ( Cabal.CondBranch cond true false ) =
      Branch cond ( tree true ) ( maybe mempty tree false )

    tree
      :: ( Monoid a )
      => Cabal.CondTree v x a
      -> ConfigTree ( Cabal.Condition v ) a
    tree ( Cabal.CondNode acc _ branches) =
      return acc `mappend` foldMap branch branches
  in
  tree


condTree
  :: ( Monoid a )
  => Dhall.Encoder a
  -> Dhall.Encoder ( Cabal.CondTree Cabal.ConfVar x a )
condTree t =
  let
    go = \case
      Leaf a ->
        Dhall.embed t a

      Branch cond a b ->
        Expr.BoolIf
          ( Dhall.embed condBranchCondition cond )
          ( go a )
          ( go b )

    configRecord =
      Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Config"

  in
  Dhall.Encoder
    { Dhall.embed =
        Expr.Lam Nothing ( Dhall.Core.makeFunctionBinding "config" configRecord )
          . go
          . unifyCondTree
    , Dhall.declared =
        Expr.Pi Nothing "_" configRecord ( Dhall.declared t )
    }


moduleName :: Dhall.Encoder Cabal.ModuleName
moduleName =
  contramap ( show . Cabal.pretty ) stringToDhall


condBranchCondition :: Dhall.Encoder (Cabal.Condition Cabal.ConfVar)
condBranchCondition =
  Dhall.Encoder
    { Dhall.declared = Expr.Bool
    , Dhall.embed =
        \a ->
          case a of
            Cabal.Var ( Cabal.OS os0 ) ->
              Expr.App ( Expr.Field ( Expr.Var "config" ) ( Dhall.Core.makeFieldSelection "os" ) ) ( Dhall.embed os os0 )

            Cabal.Var ( Cabal.Arch arch0 ) ->
              Expr.App ( Expr.Field ( Expr.Var "config" ) ( Dhall.Core.makeFieldSelection "arch" ) ) ( Dhall.embed arch arch0 )

            Cabal.Var ( Cabal.Flag flagName0 ) ->
              Expr.App ( Expr.Field ( Expr.Var "config" ) ( Dhall.Core.makeFieldSelection "flag" ) ) ( Dhall.embed flagName flagName0 )

            Cabal.Var ( Cabal.Impl c v ) ->
              Expr.App ( Expr.App ( Expr.Field ( Expr.Var "config" ) ( Dhall.Core.makeFieldSelection "impl" ) ) ( Dhall.embed compilerFlavor c ) ) ( Dhall.embed versionRange v )

            Cabal.Lit b ->
              Expr.BoolLit b

            Cabal.CNot c ->
              Expr.BoolEQ ( Expr.BoolLit False ) ( Dhall.embed condBranchCondition c )

            Cabal.CAnd a b ->
              Expr.BoolAnd ( Dhall.embed condBranchCondition a ) ( Dhall.embed condBranchCondition b )

            Cabal.COr a b ->
              Expr.BoolOr ( Dhall.embed condBranchCondition a ) ( Dhall.embed condBranchCondition b )
    }


os :: Dhall.Encoder Cabal.OS
os =
  Dhall.Encoder
    { Dhall.embed = \case
        Cabal.Linux ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "Linux"

        Cabal.Windows ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "Windows"

        Cabal.OSX ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "OSX"

        Cabal.FreeBSD ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "FreeBSD"

        Cabal.OpenBSD ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "OpenBSD"

        Cabal.NetBSD ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "NetBSD"

        Cabal.DragonFly ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "DragonFly"

        Cabal.Solaris ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "Solaris"

        Cabal.AIX ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "AIX"

        Cabal.HPUX ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "HPUX"

        Cabal.IRIX ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "IRIX"

        Cabal.HaLVM ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "HaLVM"

        Cabal.Hurd ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "Hurd"

        Cabal.IOS ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "IOS"

        Cabal.Android ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "Android"

        Cabal.Ghcjs ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "Ghcjs"

        Cabal.OtherOS os ->
          Expr.App
            ( Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS" `Expr.Field` Dhall.Core.makeFieldSelection "OtherOS" )
            ( Expr.RecordLit ( Map.singleton "_1" ( Dhall.Core.makeRecordField ( dhallString os ) ) ) )

    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "OS"
    }


arch :: Dhall.Encoder Cabal.Arch
arch =
  Dhall.Encoder
    { Dhall.embed = \case
        Cabal.I386 ->
          arch "I386"
        Cabal.X86_64 ->
          arch "X86_64"
        Cabal.PPC ->
          arch "PPC"
        Cabal.PPC64 ->
          arch "PPC64"
        Cabal.Sparc ->
          arch "Sparc"
        Cabal.Arm ->
          arch "Arm"
        Cabal.Mips ->
          arch "Mips"
        Cabal.SH ->
          arch "SH"
        Cabal.IA64 ->
          arch "IA64"
        Cabal.S390 ->
          arch "S390"
        Cabal.Alpha ->
          arch "Alpha"
        Cabal.Hppa ->
          arch "Hppa"
        Cabal.Rs6000 ->
          arch "Rs6000"
        Cabal.M68k ->
          arch "M68k"
        Cabal.Vax ->
          arch "Vax"
        Cabal.JavaScript ->
          arch "JavaScript"
        Cabal.AArch64 ->
          arch "AArch64"
        Cabal.OtherArch s ->
          Expr.App
            ( arch "OtherArch" )
            ( Expr.RecordLit ( Map.singleton "_1" ( Dhall.Core.makeRecordField ( dhallString s ) ) ) )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Arch"
    }
  where
  arch name =
    Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Arch"
                     `Expr.Field` Dhall.Core.makeFieldSelection name


buildInfoRecord :: RecordInputType Cabal.BuildInfo
buildInfoRecord =
  mconcat
    [ recordField "buildable" ( contramap Cabal.buildable Dhall.inject )
    , recordField "build-tools" ( contramap Cabal.buildTools ( listOf legacyExeDependency ) )
    , recordField "build-tool-depends" ( contramap Cabal.buildToolDepends ( listOf exeDependency ) )
    , recordField "cpp-options" ( contramap Cabal.cppOptions ( listOf stringToDhall ) )
    , recordField "cc-options" ( contramap Cabal.ccOptions ( listOf stringToDhall ) )
    , recordField "ld-options" ( contramap Cabal.ldOptions ( listOf stringToDhall ) )
    , recordField "pkgconfig-depends" ( contramap Cabal.pkgconfigDepends ( listOf pkgconfigDependency ) )
    , recordField "frameworks" ( contramap Cabal.frameworks ( listOf stringToDhall ) )
    , recordField "extra-framework-dirs" ( contramap Cabal.extraFrameworkDirs ( listOf stringToDhall ) )
    , recordField "c-sources" ( contramap Cabal.cSources ( listOf stringToDhall ) )
    , recordField "js-sources" ( contramap Cabal.jsSources ( listOf stringToDhall ) )
    , recordField "hs-source-dirs" ( contramap Cabal.hsSourceDirs ( listOf stringToDhall ) )
    , recordField "other-modules" ( contramap Cabal.otherModules ( listOf moduleName ) )
    , recordField "autogen-modules" ( contramap Cabal.autogenModules ( listOf moduleName ) )
    , recordField "default-language" ( contramap Cabal.defaultLanguage ( maybeToDhall language ) )
    , recordField "other-languages" ( contramap Cabal.otherLanguages ( listOf language ) )
    , recordField "default-extensions" ( Cabal.defaultExtensions >$< listOf extension )
    , recordField "other-extensions" ( Cabal.otherExtensions >$< listOf extension )
    , recordField "extra-libraries" ( Cabal.extraLibs >$< listOf stringToDhall )
    , recordField "extra-ghci-libraries" ( Cabal.extraGHCiLibs >$< listOf stringToDhall )
    , recordField "extra-lib-dirs" ( Cabal.extraLibDirs >$< listOf stringToDhall )
    , recordField "include-dirs" ( Cabal.includeDirs >$< listOf stringToDhall )
    , recordField "includes" ( Cabal.includes >$< listOf stringToDhall )
    , recordField "install-includes" ( Cabal.installIncludes >$< listOf stringToDhall )
    , recordField "compiler-options" ( Cabal.options >$< compilerOptions )
    , recordField "profiling-options" ( Cabal.profOptions >$< compilerOptions )
    , recordField "shared-options" ( Cabal.sharedOptions >$< compilerOptions )
    , recordField "static-options" ( Cabal.staticOptions >$< compilerOptions )
    , recordField "build-depends" ( Cabal.targetBuildDepends >$< listOf dependency )
    , recordField "mixins" ( Cabal.mixins >$< listOf mixin )
    , recordField "asm-options" ( Cabal.asmOptions >$< listOf stringToDhall)
    , recordField "asm-sources" ( Cabal.asmSources >$< listOf stringToDhall)
    , recordField "cmm-options" ( Cabal.cmmOptions >$< listOf stringToDhall )
    , recordField "cmm-sources" ( Cabal.cmmSources >$< listOf stringToDhall )
    , recordField "cxx-options" ( Cabal.cxxOptions >$< listOf stringToDhall )
    , recordField "cxx-sources" ( Cabal.cxxSources >$< listOf stringToDhall)
    , recordField "virtual-modules" ( Cabal.virtualModules >$< listOf moduleName )
    , recordField "extra-lib-flavours" ( Cabal.extraLibFlavours >$< listOf stringToDhall )
    , recordField "extra-bundled-libs" ( Cabal.extraBundledLibs >$< listOf stringToDhall )
    , recordField "autogen-includes" ( Cabal.autogenIncludes >$< listOf stringToDhall )
    , recordField "extra-dyn-lib-flavours" ( Cabal.extraDynLibFlavours >$< listOf stringToDhall )
    ]


moduleReexport :: Dhall.Encoder Cabal.ModuleReexport
moduleReexport =
  runRecordInputType
    ( mconcat
        [ recordField "original"
             ( ( \a -> ( Cabal.moduleReexportOriginalPackage a, Cabal.moduleReexportOriginalName a ) ) >$<
                runRecordInputType
                 ( mconcat
                     [ recordField "package" ( fst >$< maybeToDhall packageNameToDhall )
                     , recordField "name" ( snd >$< moduleName )
                     ]
                 )
             )
        , recordField "name" ( Cabal.moduleReexportName >$< moduleName )
        ]
    )


legacyExeDependency :: Dhall.Encoder Cabal.LegacyExeDependency
legacyExeDependency =
  runRecordInputType
    ( mconcat
        [ recordField "exe" ( ( \( Cabal.LegacyExeDependency exe _ ) -> exe ) >$< stringToDhall )
        , recordField "version" ( ( \( Cabal.LegacyExeDependency _ version ) -> version ) >$< versionRange )
        ]
    )

exeDependency :: Dhall.Encoder Cabal.ExeDependency
exeDependency =
  runRecordInputType
    ( mconcat
        [ recordField "package" ( ( \( Cabal.ExeDependency packageName _ _ ) -> packageName ) >$< packageNameToDhall )
        , recordField "component" ( ( \( Cabal.ExeDependency _ component _ ) -> component ) >$< unqualComponentName )
        , recordField "version" ( ( \( Cabal.ExeDependency _ _ version ) -> version ) >$< versionRange )
        ]
    )


unqualComponentName :: Dhall.Encoder Cabal.UnqualComponentName
unqualComponentName =
  show . Cabal.pretty >$< stringToDhall


pkgconfigDependency :: Dhall.Encoder Cabal.PkgconfigDependency
pkgconfigDependency =
  runRecordInputType
    ( mconcat
        [ recordField "name" ( ( \( Cabal.PkgconfigDependency a _version ) -> a ) >$< pkgconfigName )
        , recordField "version" ( ( \( Cabal.PkgconfigDependency _name a ) -> a ) >$< pkgconfigVersionRange )
        ]
    )


pkgconfigName :: Dhall.Encoder Cabal.PkgconfigName
pkgconfigName =
  show . Cabal.pretty >$< stringToDhall


-- PkgconfigVersion is restricted to ASCII-only characters.
pkgconfigVersion :: Dhall.Encoder Cabal.PkgconfigVersion
pkgconfigVersion = (\ ( Cabal.PkgconfigVersion a ) -> StrictText.decodeLatin1 a ) >$< Dhall.inject

pkgconfigVersionRange :: Dhall.Encoder Cabal.PkgconfigVersionRange
pkgconfigVersionRange =  Dhall.Encoder
    { Dhall.embed =
          let
            go = \case
              Cabal.PcAnyVersion ->
                Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "pkg-config" `Expr.Field` Dhall.Core.makeFieldSelection "anyVersion"
              Cabal.PcThisVersion v ->
                Expr.App
                  ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "pkg-config" `Expr.Field` Dhall.Core.makeFieldSelection "thisVersion" )
                  ( Dhall.embed pkgconfigVersion v )
              Cabal.PcLaterVersion v ->
                Expr.App
                  ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "pkg-config" `Expr.Field` Dhall.Core.makeFieldSelection "laterVersion" )
                  ( Dhall.embed pkgconfigVersion v )
              Cabal.PcEarlierVersion v ->
                Expr.App
                  ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "pkg-config" `Expr.Field` Dhall.Core.makeFieldSelection "earlierVersion" )
                  ( Dhall.embed pkgconfigVersion v )
              Cabal.PcOrLaterVersion v ->
                Expr.App
                  ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "pkg-config" `Expr.Field` Dhall.Core.makeFieldSelection "orLaterVersion" )
                  ( Dhall.embed pkgconfigVersion v )
              Cabal.PcOrEarlierVersion v ->
                Expr.App
                  ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "pkg-config" `Expr.Field` Dhall.Core.makeFieldSelection "orEarlierVersion" )
                  ( Dhall.embed pkgconfigVersion v )
              Cabal.PcUnionVersionRanges a b ->
                Expr.App
                  ( Expr.App
                    ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "pkg-config" `Expr.Field` Dhall.Core.makeFieldSelection "unionVersionRanges" )
                    ( go a )
                  )
                  ( go b )
              Cabal.PcIntersectVersionRanges a b ->
                Expr.App
                  ( Expr.App
                    ( Expr.Var "prelude" `Expr.Field` Dhall.Core.makeFieldSelection "pkg-config" `Expr.Field` Dhall.Core.makeFieldSelection "intersectVersionRanges" )
                    ( go a )
                  )
                  ( go b )
          in
          go
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "PkgconfigVersionRange"
    }


language :: Dhall.Encoder Cabal.Language
language =
  Dhall.Encoder
    { Dhall.embed = \case
        Cabal.Haskell2010 ->
          lang "Haskell2010"
        Cabal.Haskell98 ->
          lang "Haskell98"
        Cabal.UnknownLanguage s ->
          Expr.App
            ( lang "UnknownLanguage" )
            ( Expr.RecordLit ( Map.singleton "_1" ( Dhall.Core.makeRecordField ( dhallString s ) ) ) )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Language"
    }
  where
    lang name =
      Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Language"
                       `Expr.Field` Dhall.Core.makeFieldSelection name

extension :: Dhall.Encoder Cabal.Extension
extension =
  Dhall.Encoder
    { Dhall.embed =
        \a ->
          case a of
            Cabal.EnableExtension ext ->
              extWith True ext

            Cabal.DisableExtension ext ->
              extWith False ext

            _ ->
              error "Unknown extension"
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Extension"
    }

  where

  extName :: Cabal.KnownExtension -> StrictText.Text
  extName e =
    StrictText.pack ( show e )

  extWith :: Bool -> Cabal.KnownExtension -> Expr.Expr Dhall.Parser.Src a
  extWith trueFalse ext =
    Expr.App
      ( Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Extension" `Expr.Field` Dhall.Core.makeFieldSelection ( extName ext ) )
      ( Expr.BoolLit trueFalse )


compilerOptions :: Dhall.Encoder ( Cabal.PerCompilerFlavor [ String ] )
compilerOptions =
  ( runRecordInputTypeWithDefault CompilerOptions compilerOptionsDefault
      ( mconcat
          [ recordField "GHC" ( (\ ( Cabal.PerCompilerFlavor ghc _ ) -> ghc ) >$< listOf stringToDhall )
          , recordField "GHCJS" ( (\ ( Cabal.PerCompilerFlavor _ ghcjs ) -> ghcjs ) >$< listOf stringToDhall )
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "CompilerOptions"
    }


mixin :: Dhall.Encoder Cabal.Mixin
mixin =
  ( runRecordInputType
      ( mconcat
          [ recordField "package" ( Cabal.mixinPackageName >$< packageNameToDhall )
          , recordField "renaming" ( Cabal.mixinIncludeRenaming >$< includeRenaming )
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Mixin"

    }


includeRenaming :: Dhall.Encoder Cabal.IncludeRenaming
includeRenaming =
  runRecordInputType
    ( mconcat
        [ recordField "provides" ( Cabal.includeProvidesRn >$< moduleRenaming )
        , recordField "requires" ( Cabal.includeRequiresRn >$< moduleRenaming )
        ]
    )


moduleRenaming :: Dhall.Encoder Cabal.ModuleRenaming
moduleRenaming =
  Dhall.Encoder
    { Dhall.embed =
        \a ->
          case a of
            Cabal.ModuleRenaming renamed ->
              Expr.App
                ( Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "ModuleRenaming" `Expr.Field` Dhall.Core.makeFieldSelection "renaming" )
                ( Expr.ListLit
                    Nothing
                    ( fmap
                        (\ ( src, dst ) ->
                           Expr.RecordLit
                             ( Dhall.Core.makeRecordField
                               <$> Map.fromList
                                 [ ( "rename", Dhall.embed moduleName src )
                                 , ( "to", Dhall.embed moduleName dst )
                                 ]
                             )
                        )
                        ( Seq.fromList renamed )
                    )
                )
            Cabal.DefaultRenaming ->
              Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "ModuleRenaming" `Expr.Field` Dhall.Core.makeFieldSelection "default"
            Cabal.HidingRenaming hidden ->
              Expr.App
                ( Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "ModuleRenaming" `Expr.Field` Dhall.Core.makeFieldSelection "hiding" )
                ( Expr.ListLit
                    Nothing
                    ( Dhall.embed moduleName <$> Seq.fromList hidden )
                )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "ModuleRenaming"
    }


benchmark :: Dhall.Encoder Cabal.Benchmark
benchmark =
  (  runRecordInputTypeWithDefault Benchmark benchmarkDefault
       ( mconcat
           [ recordField "main-is" (
                ( \case Cabal.BenchmarkExeV10 _ fp -> fp
                        Cabal.BenchmarkUnsupported _ -> errorWithoutStackTrace "Unsupported benchmark type"
                )
              . Cabal.benchmarkInterface >$< stringToDhall
              )
           , Cabal.benchmarkBuildInfo >$< buildInfoRecord
           ]
       )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Benchmark"
    }


testSuite :: Dhall.Encoder Cabal.TestSuite
testSuite =
  ( runRecordInputTypeWithDefault TestSuite testSuiteDefault
      ( mconcat
          [ recordField "type" ( Cabal.testInterface >$< testSuiteInterface )
          , Cabal.testBuildInfo >$< buildInfoRecord
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "TestSuite"
    }


testSuiteInterface :: Dhall.Encoder Cabal.TestSuiteInterface
testSuiteInterface =
  Dhall.Encoder
    { Dhall.embed = \case
        Cabal.TestSuiteExeV10 _ main ->
          Expr.App
            ( interface "exitcode-stdio" )
            ( Dhall.embed
              ( runRecordInputType ( recordField "main-is" stringToDhall ) )
              main
            )
        Cabal.TestSuiteLibV09 _ m ->
          Expr.App
            ( interface "detailed" )
            ( Dhall.embed
              ( runRecordInputType ( recordField "module" moduleName ) )
              m
            )
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "TestType"
    }
  where
  interface name =
    Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "TestType"
                     `Expr.Field` Dhall.Core.makeFieldSelection name


executable :: Dhall.Encoder Cabal.Executable
executable =
  ( runRecordInputTypeWithDefault Executable executableDefault
      ( mconcat
          [ recordField "main-is" ( Cabal.modulePath >$< stringToDhall )
          , recordField "scope" ( Cabal.exeScope >$< executableScope )
          , Cabal.buildInfo >$< buildInfoRecord
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Executable"
    }


executableScope :: Dhall.Encoder Cabal.ExecutableScope
executableScope =
  Dhall.Encoder
    { Dhall.embed = \case
        Cabal.ExecutablePublic ->
            Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Scope" `Expr.Field` Dhall.Core.makeFieldSelection "Public"
        Cabal.ExecutablePrivate ->
            Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Scope" `Expr.Field` Dhall.Core.makeFieldSelection "Private"
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "Scope"
    }


foreignLibrary :: Dhall.Encoder Cabal.ForeignLib
foreignLibrary =
  ( runRecordInputType
      ( mconcat
          [ recordField "type" ( Cabal.foreignLibType >$< foreignLibType )
          , recordField "options" ( Cabal.foreignLibOptions >$< ( listOf foreignLibOption ) )
          , Cabal.foreignLibBuildInfo >$< buildInfoRecord
          , recordField "lib-version-info" ( Cabal.foreignLibVersionInfo >$< maybeToDhall versionInfo )
          , recordField "lib-version-linux" ( Cabal.foreignLibVersionLinux >$< maybeToDhall versionToDhall )
          , recordField "mod-def-files" ( Cabal.foreignLibModDefFile >$< listOf stringToDhall )
          ]
      )
  )
    { Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "ForeignLibrary"
    }


versionInfo :: Dhall.Encoder Cabal.LibVersionInfo
versionInfo =
  Cabal.libVersionInfoCRA >$<
  runRecordInputType
    ( mconcat
        [ recordField "current" ( ( \( a, _, _ ) -> fromIntegral a :: Natural ) >$< ( Dhall.inject ) )
        , recordField "revision" ( ( \( _, a, _ ) -> fromIntegral a :: Natural ) >$< ( Dhall.inject ) )
        , recordField "age" ( ( \( _, _, a ) -> fromIntegral a :: Natural ) >$< ( Dhall.inject ) )
        ]
    )


foreignLibOption :: Dhall.Encoder Cabal.ForeignLibOption
foreignLibOption =
  Dhall.Encoder
    { Dhall.embed = \case
        Cabal.ForeignLibStandalone ->
          Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "ForeignLibOption" `Expr.Field` Dhall.Core.makeFieldSelection "Standalone"
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "ForeignLibOption"
    }


foreignLibType :: Dhall.Encoder Cabal.ForeignLibType
foreignLibType =
  Dhall.Encoder
    { Dhall.embed = \case
        Cabal.ForeignLibNativeShared ->
          ty "Shared"
        Cabal.ForeignLibNativeStatic ->
          ty "Static"
    , Dhall.declared =
        Expr.Var "types" `Expr.Field` Dhall.Core.makeFieldSelection "ForeignLibType"
    }
  where
    ty name =
      Expr.Var "types"
        `Expr.Field` Dhall.Core.makeFieldSelection "ForeignLibType"
        `Expr.Field` Dhall.Core.makeFieldSelection name
