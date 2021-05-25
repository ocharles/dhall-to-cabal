{-# language ApplicativeDo #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module DhallToCabal
  ( dhallToCabal
  , genericPackageDescription
  , sourceRepo
  , repoKind
  , repoType
  , compiler
  , operatingSystem
  , library
  , extension
  , compilerOptions
  , guarded
  , arch
  , compilerFlavor
  , language
  , license
  , spdxLicense
  , spdxLicenseId
  , spdxLicenseExceptionId
  , executable
  , testSuite
  , benchmark
  , foreignLib
  , buildType
  , versionRange
  , version
  , configRecordType
  , buildInfoType
  , executableScope
  , moduleRenaming
  , foreignLibOption
  , foreignLibType
  , setupBuildInfo
  , dependency
  , testSuiteInterface
  , mixin
  , flag
  , libraryName
  , pkgconfigVersionRange
  , libraryVisibility

  , sortExpr
  ) where

import Data.List ( partition )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>) )
import Data.Void ( Void )

import qualified Data.Set as Set
import qualified Data.Text as StrictText
import qualified Data.Text.Encoding as StrictText
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Map as Map
import qualified Dhall.Parser
import qualified Distribution.Compiler as Cabal
import qualified Distribution.License as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.SPDX as SPDX
import qualified Distribution.System as Cabal ( Arch(..), OS(..) )
import qualified Distribution.Text as Cabal ( simpleParse )
import qualified Distribution.Types.CondTree as Cabal
import qualified Distribution.Types.Dependency as Cabal
import qualified Distribution.Types.ExeDependency as Cabal
import qualified Distribution.Types.ExecutableScope as Cabal
import qualified Distribution.Types.ForeignLib as Cabal
import qualified Distribution.Types.ForeignLibOption as Cabal
import qualified Distribution.Types.ForeignLibType as Cabal
import qualified Distribution.Types.IncludeRenaming as Cabal
import qualified Distribution.Types.LegacyExeDependency as Cabal
import qualified Distribution.Types.LibraryVisibility as Cabal
import qualified Distribution.Types.Mixin as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.PkgconfigDependency as Cabal
import qualified Distribution.Types.PkgconfigName as Cabal
import qualified Distribution.Types.PkgconfigVersion as Cabal
import qualified Distribution.Types.PkgconfigVersionRange as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Version as Cabal
import qualified Language.Haskell.Extension as Cabal

import qualified Dhall.Core as Expr
  ( Chunks(..), Const(..), Expr(..), Var(..) )

import Dhall.Extra
import DhallToCabal.ConfigTree ( ConfigTree(..), toConfigTree )
import DhallToCabal.Diff ( Diffable(..)  )



packageIdentifier :: Dhall.RecordDecoder Cabal.PackageIdentifier
packageIdentifier = do
  pkgName <-
    Dhall.field "name" packageName

  pkgVersion <-
    Dhall.field "version" version

  pure Cabal.PackageIdentifier { .. }



packageName :: Dhall.Decoder Cabal.PackageName
packageName =
  Cabal.mkPackageName <$> Dhall.string



packageDescription :: Dhall.RecordDecoder Cabal.PackageDescription
packageDescription = do
  package <-
    packageIdentifier

  benchmarks <-
    pure []

  testSuites <-
    pure []

  executables <-
    pure []

  foreignLibs <-
    pure []

  subLibraries <-
    pure []

  library <-
    pure Nothing

  customFieldsPD <-
    Dhall.field
      "x-fields"
      ( Dhall.list ( Dhall.pair Dhall.string Dhall.string ) )

  sourceRepos <-
    Dhall.field "source-repos" ( Dhall.list sourceRepo )

  specVersionRaw <-
    Left <$> Dhall.field "cabal-version" version

  buildTypeRaw <-
    Dhall.field "build-type" ( Dhall.maybe buildType )

  licenseRaw <-
    Dhall.field "license" license

  licenseFiles <-
    Dhall.field "license-files" ( Dhall.list Dhall.string )

  copyright <-
    Dhall.field "copyright" Dhall.string

  maintainer <-
    Dhall.field "maintainer" Dhall.string

  author <-
    Dhall.field "author" Dhall.string

  stability <-
    Dhall.field "stability" Dhall.string

  testedWith <-
    Dhall.field "tested-with" ( Dhall.list compiler )

  homepage <-
    Dhall.field "homepage" Dhall.string

  pkgUrl <-
    Dhall.field "package-url" Dhall.string

  bugReports <-
    Dhall.field "bug-reports" Dhall.string

  synopsis <-
    Dhall.field "synopsis" Dhall.string

  description <-
    Dhall.field "description" Dhall.string

  category <-
    Dhall.field "category" Dhall.string

  setupBuildInfo <-
    Dhall.field "custom-setup" ( Dhall.maybe setupBuildInfo )

  dataFiles <-
    Dhall.field "data-files" ( Dhall.list Dhall.string )

  dataDir <-
    Dhall.field "data-dir" Dhall.string

  extraSrcFiles <-
    Dhall.field "extra-source-files" ( Dhall.list Dhall.string )

  extraTmpFiles <-
    Dhall.field "extra-tmp-files" ( Dhall.list Dhall.string )

  extraDocFiles <-
    Dhall.field "extra-doc-files" ( Dhall.list Dhall.string )

  return ( adjustUnspecifiedLicense Cabal.PackageDescription { .. } )

  where
    -- Cabal reads a 2.0-spec file without a license as having
    -- SPDX.NONE, but outputting SPDX-style licenses with 2.0 doesn't
    -- work (and outputting legacy non-SPDX licenses with 2.2 also
    -- doesn't work). In CabalToDhall we map SPDX.NONE to
    -- AllRightsReserved, but we still have to switch between
    -- SPDX-style NONE and legacy-style AllRightsReserved here
    -- depending on cabal-version.
    adjustUnspecifiedLicense desc
      | Cabal.specVersion desc >= Cabal.mkVersion [2,2]
      , Cabal.licenseRaw desc == Right Cabal.AllRightsReserved
        = desc { Cabal.licenseRaw = Left SPDX.NONE }
      | otherwise
        = desc


version :: Dhall.Decoder Cabal.Version
version =
  let
    parse text =
      fromMaybe
        ( error "Could not parse version" )
        ( Cabal.simpleParse ( StrictText.unpack text ) )

    extract e =
      go
        ( Dhall.Core.normalize ( e `Expr.App` "Version" `Expr.App` "v" )
            `asTypeOf` e
        )

    go =
      \case
        Expr.App "v" ( Expr.TextLit ( Expr.Chunks [] text ) ) ->
          pure ( parse text )

        e ->
          Dhall.extractError ( StrictText.pack ( show e ) )

    expected =
      (\version -> Expr.Pi Nothing "Version" ( Expr.Const Expr.Type )
        $ Expr.Pi
            Nothing
            "v"
            ( Expr.Pi Nothing "_" version "Version" )
            "Version"
      )
      <$> Dhall.expected Dhall.string

  in Dhall.Decoder { .. }



benchmark :: Dhall.Decoder Cabal.Benchmark
benchmark =
  Dhall.record $ do
    mainIs <-
      Dhall.field "main-is" Dhall.string

    benchmarkName <-
      pure ""

    benchmarkBuildInfo <-
      buildInfo

    pure
      Cabal.Benchmark
        { benchmarkInterface =
            Cabal.BenchmarkExeV10 ( Cabal.mkVersion [ 1, 0 ] ) mainIs
        , ..
        }



buildInfo :: Dhall.RecordDecoder Cabal.BuildInfo
buildInfo = do
  buildable <-
    Dhall.field "buildable" Dhall.bool

  buildTools <-
    Dhall.field "build-tools" ( Dhall.list legacyExeDependency )

  buildToolDepends <-
    Dhall.field "build-tool-depends" ( Dhall.list exeDependency )

  cppOptions <-
    Dhall.field "cpp-options" ( Dhall.list Dhall.string )

  ccOptions <-
    Dhall.field "cc-options" ( Dhall.list Dhall.string )

  ldOptions <-
    Dhall.field "ld-options" ( Dhall.list Dhall.string )

  pkgconfigDepends <-
    Dhall.field "pkgconfig-depends" ( Dhall.list pkgconfigDependency )

  frameworks <-
    Dhall.field "frameworks" ( Dhall.list Dhall.string )

  extraFrameworkDirs <-
    Dhall.field "extra-framework-dirs" ( Dhall.list Dhall.string )

  cSources <-
    Dhall.field "c-sources" ( Dhall.list Dhall.string )

  jsSources <-
    Dhall.field "js-sources" ( Dhall.list Dhall.string )

  hsSourceDirs <-
    Dhall.field "hs-source-dirs" ( Dhall.list Dhall.string )

  otherModules <-
    Dhall.field "other-modules" ( Dhall.list moduleName )

  autogenModules <-
    Dhall.field "autogen-modules" ( Dhall.list moduleName )

  defaultLanguage <-
    Dhall.field "default-language" ( Dhall.maybe language )

  otherLanguages <-
    Dhall.field "other-languages" ( Dhall.list language )

  defaultExtensions <-
    Dhall.field "default-extensions" ( Dhall.list extension )

  otherExtensions <-
    Dhall.field "other-extensions" ( Dhall.list extension )

  oldExtensions <-
    pure []

  extraLibs <-
    Dhall.field "extra-libraries" ( Dhall.list Dhall.string )

  extraGHCiLibs <-
    Dhall.field "extra-ghci-libraries" ( Dhall.list Dhall.string )

  extraLibDirs <-
    Dhall.field "extra-lib-dirs" ( Dhall.list Dhall.string )

  includeDirs <-
    Dhall.field "include-dirs" ( Dhall.list Dhall.string )

  includes <-
    Dhall.field "includes" ( Dhall.list Dhall.string )

  installIncludes <-
    Dhall.field "install-includes" ( Dhall.list Dhall.string )

  options <-
    Dhall.field "compiler-options" compilerOptions

  profOptions <-
    Dhall.field "profiling-options" compilerOptions

  sharedOptions <-
    Dhall.field "shared-options" compilerOptions

  staticOptions <-
    Dhall.field "static-options" compilerOptions

  customFieldsBI <-
    pure []

  targetBuildDepends <-
    Dhall.field "build-depends" ( Dhall.list dependency )

  mixins <-
    Dhall.field "mixins" ( Dhall.list mixin )

  asmOptions <-
    Dhall.field "asm-options" ( Dhall.list Dhall.string )

  asmSources <-
    Dhall.field "asm-sources" ( Dhall.list Dhall.string )

  cmmOptions <-
    Dhall.field "cmm-options" ( Dhall.list Dhall.string )

  cmmSources <-
    Dhall.field "cmm-sources" ( Dhall.list Dhall.string )

  cxxOptions <-
    Dhall.field "cxx-options" ( Dhall.list Dhall.string )

  cxxSources <-
    Dhall.field "cxx-sources" ( Dhall.list Dhall.string )

  virtualModules <-
    Dhall.field "virtual-modules" ( Dhall.list moduleName )

  extraLibFlavours <-
    Dhall.field "extra-lib-flavours" ( Dhall.list Dhall.string )

  extraBundledLibs <-
    Dhall.field "extra-bundled-libs" ( Dhall.list Dhall.string )

  extraDynLibFlavours <-
    Dhall.field "extra-dyn-lib-flavours" ( Dhall.list Dhall.string )

  autogenIncludes <-
    Dhall.field "autogen-includes" ( Dhall.list Dhall.string )

  return Cabal.BuildInfo { .. }


buildInfoType :: Dhall.Expector ( Expr.Expr Dhall.Parser.Src Void )
buildInfoType =
  Dhall.expected ( Dhall.record buildInfo )


testSuite :: Dhall.Decoder Cabal.TestSuite
testSuite =
  Dhall.record $ do
    testName <-
      pure ""

    testBuildInfo <-
      buildInfo

    testInterface <-
      Dhall.field "type" testSuiteInterface

    pure
      Cabal.TestSuite
        { ..
        }



testSuiteInterface :: Dhall.Decoder Cabal.TestSuiteInterface
testSuiteInterface = Dhall.union
  ( mconcat
    [ Cabal.TestSuiteExeV10 ( Cabal.mkVersion [ 1, 0 ] )
        <$> Dhall.constructor "exitcode-stdio"
              ( Dhall.record ( Dhall.field "main-is" Dhall.string ) )
    , Cabal.TestSuiteLibV09 ( Cabal.mkVersion [ 0, 9 ] )
        <$> Dhall.constructor "detailed"
              ( Dhall.record ( Dhall.field "module" moduleName ) )
    ]
  )



unqualComponentName :: Dhall.Decoder Cabal.UnqualComponentName
unqualComponentName =
  Cabal.mkUnqualComponentName <$> Dhall.string



executable :: Dhall.Decoder Cabal.Executable
executable =
  Dhall.record $ do
    exeName <-
      pure ""

    modulePath <-
      Dhall.field "main-is" Dhall.string

    exeScope <-
      Dhall.field "scope" executableScope

    buildInfo <-
      buildInfo

    pure Cabal.Executable { .. }



foreignLib :: Dhall.Decoder Cabal.ForeignLib
foreignLib =
  Dhall.record $ do
    foreignLibName <-
      pure ""

    foreignLibType <-
      Dhall.field "type" foreignLibType

    foreignLibOptions <-
      Dhall.field "options" ( Dhall.list foreignLibOption )

    foreignLibBuildInfo <-
      buildInfo

    foreignLibVersionInfo <-
      Dhall.field "lib-version-info" ( Dhall.maybe versionInfo )

    foreignLibVersionLinux <-
      Dhall.field "lib-version-linux" ( Dhall.maybe version )

    foreignLibModDefFile <-
      Dhall.field "mod-def-files" ( Dhall.list Dhall.string )

    pure Cabal.ForeignLib { .. }



foreignLibType :: Dhall.Decoder Cabal.ForeignLibType
foreignLibType = Dhall.union
  ( mconcat
    [ Cabal.ForeignLibNativeShared <$ Dhall.constructor "Shared" Dhall.unit
    , Cabal.ForeignLibNativeStatic <$ Dhall.constructor "Static" Dhall.unit
    ]
  )



library :: Dhall.Decoder ( Cabal.LibraryName -> Cabal.Library )
library =
  Dhall.record $ do
    libBuildInfo <-
      buildInfo

    exposedModules <-
      Dhall.field "exposed-modules" ( Dhall.list moduleName )

    reexportedModules <-
      Dhall.field "reexported-modules" ( Dhall.list moduleReexport )

    signatures <-
      Dhall.field "signatures" ( Dhall.list moduleName )

    libExposed <-
      pure True

    libVisibility <-
      Dhall.field "visibility" libraryVisibility

    pure ( \ libName -> Cabal.Library { .. } )



subLibrary :: Dhall.Decoder ( Cabal.UnqualComponentName, Cabal.CondTree Cabal.ConfVar [ Cabal.Dependency ] Cabal.Library )
subLibrary =
  Dhall.Decoder {..}

  where

    extract = \case
      Expr.RecordLit fields -> Dhall.fromMonadic $ do
        name <- Dhall.toMonadic $
          maybe
            ( Dhall.extractError "Missing 'name' field of sub-library." )
            ( Dhall.extract unqualComponentName
            . Dhall.Core.recordFieldValue
            )
            ( Map.lookup "name" fields )
        tree <- Dhall.toMonadic $
          maybe
            ( Dhall.extractError "Missing 'library' field of sub-library." )
            ( Dhall.extract ( guarded ( ($ Cabal.LSubLibName name) <$> library ) )
            . Dhall.Core.recordFieldValue
            )
            ( Map.lookup "library" fields )
        return ( name, tree )
      e ->
        Dhall.typeError expected e

    expected = fmap Expr.Record
      ( sequenceA
        ( Map.fromList
          [ ( "name", Dhall.Core.makeRecordField <$> Dhall.expected unqualComponentName )
          , ( "library", Dhall.Core.makeRecordField <$> ( Expr.Pi Nothing "_" <$> configRecordType <*> Dhall.expected library ) )
          ]
        )
      )


sourceRepo :: Dhall.Decoder Cabal.SourceRepo
sourceRepo =
  Dhall.record $ do
    repoKind <-
      Dhall.field "kind" repoKind

    repoType <-
      Dhall.field "type" ( Dhall.maybe repoType )

    repoLocation <-
      Dhall.field "location" ( Dhall.maybe Dhall.string )

    repoModule <-
      Dhall.field "module" ( Dhall.maybe Dhall.string )

    repoBranch <-
      Dhall.field "branch" ( Dhall.maybe Dhall.string )

    repoTag <-
      Dhall.field "tag" ( Dhall.maybe Dhall.string )

    repoSubdir <-
      Dhall.field "subdir" ( Dhall.maybe filePath )

    pure Cabal.SourceRepo { .. }



repoKind :: Dhall.Decoder Cabal.RepoKind
repoKind =
  sortType Dhall.genericAuto



dependency :: Dhall.Decoder Cabal.Dependency
dependency =
  Dhall.record $ do
    packageName <-
      Dhall.field "package" packageName

    versionRange <-
      Dhall.field "bounds" versionRange

    libraryNames <-
      Set.fromList <$> Dhall.field "library-names" ( Dhall.list libraryName )

    pure ( Cabal.Dependency packageName versionRange libraryNames )



moduleName :: Dhall.Decoder Cabal.ModuleName
moduleName =
  validateDecoder $
    Cabal.simpleParse <$> Dhall.string



libraryName :: Dhall.Decoder Cabal.LibraryName
libraryName = Dhall.union
  ( mconcat
    [ Cabal.LMainLibName
        <$ Dhall.constructor "main-library" Dhall.unit
    , Cabal.LSubLibName
        <$> Dhall.constructor "sub-library" unqualComponentName
    ]
  )



dhallToCabal
  :: Dhall.InputSettings
  -> StrictText.Text
  -- ^ The Dhall to parse.
  -> IO Cabal.GenericPackageDescription
dhallToCabal settings =
  Dhall.inputWithSettings settings genericPackageDescription



-- Cabal only parses ASCII characters into a PkgconfigVersion.
pkgconfigVersion :: Dhall.Decoder Cabal.PkgconfigVersion
pkgconfigVersion = Cabal.PkgconfigVersion . StrictText.encodeUtf8 <$> Dhall.strictText



pkgconfigVersionRange :: Dhall.Decoder Cabal.PkgconfigVersionRange
pkgconfigVersionRange =
  let
    extract e =
      go
        ( Dhall.Core.normalize
            ( e
                `Expr.App` "PkgconfigVersionRange"
                `Expr.App` "anyVersion"
                `Expr.App` "thisVersion"
                `Expr.App` "laterVersion"
                `Expr.App` "earlierVersion"
                `Expr.App` "orLaterVersion"
                `Expr.App` "orEarlierVersion"
                `Expr.App` "unionVersionRanges"
                `Expr.App` "intersectVersionRanges"
            )
            `asTypeOf` e
        )

    go =
      \case
        "anyVersion" ->
          pure Cabal.PcAnyVersion

        Expr.App "thisVersion" components ->
          Cabal.PcThisVersion <$> Dhall.extract pkgconfigVersion components

        Expr.App "laterVersion" components ->
          Cabal.PcLaterVersion <$> Dhall.extract pkgconfigVersion components

        Expr.App "earlierVersion" components ->
          Cabal.PcEarlierVersion <$> Dhall.extract pkgconfigVersion components

        Expr.App "orLaterVersion" components ->
          Cabal.PcOrLaterVersion <$> Dhall.extract pkgconfigVersion components

        Expr.App "orEarlierVersion" components ->
          Cabal.PcOrEarlierVersion <$> Dhall.extract pkgconfigVersion components

        Expr.App ( Expr.App "unionVersionRanges" a ) b ->
          Cabal.PcUnionVersionRanges <$> go a <*> go b

        Expr.App ( Expr.App "intersectVersionRanges" a ) b ->
          Cabal.PcIntersectVersionRanges <$> go a <*> go b

        e ->
          Dhall.typeError expected e

    expected =
      let
        pkgconfigVersionRange =
          "PkgconfigVersionRange"

        versionToVersionRange =
          ( \versionRange ->
            Expr.Pi
              Nothing
              "_"
              versionRange
              pkgconfigVersionRange
          )
          <$> Dhall.expected pkgconfigVersion

        combine =
          Expr.Pi
            Nothing
            "_"
            pkgconfigVersionRange
            ( Expr.Pi Nothing "_" pkgconfigVersionRange pkgconfigVersionRange )

      in
      Expr.Pi Nothing "PkgconfigVersionRange" ( Expr.Const Expr.Type )
        . Expr.Pi Nothing "anyVersion" pkgconfigVersionRange
        <$> ( Expr.Pi Nothing "thisVersion" <$> versionToVersionRange
        <*> ( Expr.Pi Nothing "laterVersion" <$> versionToVersionRange
        <*> ( Expr.Pi Nothing "earlierVersion" <$> versionToVersionRange
        <*> ( Expr.Pi Nothing "orLaterVersion" <$> versionToVersionRange
        <*> ( Expr.Pi Nothing "orEarlierVersion" <$> versionToVersionRange
        <*> pure
            ( Expr.Pi Nothing "unionVersionRanges" combine
            $ Expr.Pi Nothing "intersectVersionRanges" combine
              pkgconfigVersionRange
            )
        )))))

  in Dhall.Decoder { .. }



versionRange :: Dhall.Decoder Cabal.VersionRange
versionRange =
  let
    extract e =
      go
        ( Dhall.Core.normalize
            ( e
                `Expr.App` "VersionRange"
                `Expr.App` "anyVersion"
                `Expr.App` "noVersion"
                `Expr.App` "thisVersion"
                `Expr.App` "notThisVersion"
                `Expr.App` "laterVersion"
                `Expr.App` "earlierVersion"
                `Expr.App` "orLaterVersion"
                `Expr.App` "orEarlierVersion"
                `Expr.App` "withinVersion"
                `Expr.App` "majorBoundVersion"
                `Expr.App` "unionVersionRanges"
                `Expr.App` "intersectVersionRanges"
                `Expr.App` "differenceVersionRanges"
                `Expr.App` "invertVersionRange"
            )
            `asTypeOf` e
        )

    go =
      \case
        "anyVersion" ->
          pure Cabal.anyVersion

        "noVersion" ->
          pure Cabal.noVersion

        Expr.App "thisVersion" components ->
          Cabal.thisVersion <$> Dhall.extract version components

        Expr.App "notThisVersion" components ->
          Cabal.notThisVersion <$> Dhall.extract version components

        Expr.App "laterVersion" components ->
          Cabal.laterVersion <$> Dhall.extract version components

        Expr.App "earlierVersion" components ->
          Cabal.earlierVersion <$> Dhall.extract version components

        Expr.App "orLaterVersion" components ->
          Cabal.orLaterVersion <$> Dhall.extract version components

        Expr.App "orEarlierVersion" components ->
          Cabal.orEarlierVersion <$> Dhall.extract version components

        Expr.App "invertVersionRange" components ->
          Cabal.invertVersionRange <$> go components

        Expr.App "withinVersion" components ->
          Cabal.withinVersion <$> Dhall.extract version components

        Expr.App "majorBoundVersion" components ->
          Cabal.majorBoundVersion <$> Dhall.extract version components

        Expr.App ( Expr.App "unionVersionRanges" a ) b ->
          Cabal.unionVersionRanges <$> go a <*> go b

        Expr.App ( Expr.App "intersectVersionRanges" a ) b ->
          Cabal.intersectVersionRanges <$> go a <*> go b

        Expr.App ( Expr.App "differenceVersionRanges" a ) b ->
          Cabal.differenceVersionRanges <$> go a <*> go b

        e ->
          Dhall.typeError expected e

    expected =
      let
        versionRange =
          "VersionRange"

        versionToVersionRange =
          ( \v ->
          Expr.Pi
            Nothing
            "_"
            v
            versionRange
          )
          <$> Dhall.expected version

        combine =
          Expr.Pi Nothing "_" versionRange ( Expr.Pi Nothing "_" versionRange versionRange )

      in
      Expr.Pi Nothing "VersionRange" ( Expr.Const Expr.Type )
        . Expr.Pi Nothing "anyVersion" versionRange
        . Expr.Pi Nothing "noVersion" versionRange
        <$> ( Expr.Pi Nothing "thisVersion" <$> versionToVersionRange
        <*> ( Expr.Pi Nothing "notThisVersion" <$> versionToVersionRange
        <*> ( Expr.Pi Nothing "laterVersion" <$> versionToVersionRange
        <*> ( Expr.Pi Nothing "earlierVersion" <$> versionToVersionRange
        <*> ( Expr.Pi Nothing "orLaterVersion" <$> versionToVersionRange
        <*> ( Expr.Pi Nothing "orEarlierVersion" <$> versionToVersionRange
        <*> ( Expr.Pi Nothing "withinVersion" <$> versionToVersionRange
        <*> ( Expr.Pi Nothing "majorBoundVersion" <$> versionToVersionRange
        <*> pure
            ( Expr.Pi Nothing "unionVersionRanges" combine
            $ Expr.Pi Nothing "intersectVersionRanges" combine
            $ Expr.Pi Nothing "differenceVersionRanges" combine
            $ Expr.Pi
                Nothing
                "invertVersionRange"
                ( Expr.Pi Nothing "_" versionRange versionRange )
                versionRange
            )))))))))

  in Dhall.Decoder { .. }



buildType :: Dhall.Decoder Cabal.BuildType
buildType =
  sortType Dhall.genericAuto



license :: Dhall.Decoder (Either SPDX.License Cabal.License)
license = Dhall.union
  ( mconcat
    [ Right . Cabal.GPL <$> Dhall.constructor "GPL" ( Dhall.maybe version )
    , Right . Cabal.AGPL <$> Dhall.constructor "AGPL" ( Dhall.maybe version )
    , Right . Cabal.LGPL <$> Dhall.constructor "LGPL" ( Dhall.maybe version )
    , Right Cabal.BSD2 <$ Dhall.constructor "BSD2" Dhall.unit
    , Right Cabal.BSD3 <$ Dhall.constructor "BSD3" Dhall.unit
    , Right Cabal.BSD4 <$ Dhall.constructor "BSD4" Dhall.unit
    , Right Cabal.MIT <$ Dhall.constructor "MIT" Dhall.unit
    , Right Cabal.ISC <$ Dhall.constructor "ISC" Dhall.unit
    , Right . Cabal.MPL <$> Dhall.constructor "MPL" version
    , Right . Cabal.Apache <$> Dhall.constructor "Apache" ( Dhall.maybe version )
    , Right Cabal.PublicDomain <$ Dhall.constructor "PublicDomain" Dhall.unit
    , Right Cabal.AllRightsReserved <$ Dhall.constructor "AllRightsReserved" Dhall.unit
    , Right Cabal.UnspecifiedLicense <$ Dhall.constructor "Unspecified" Dhall.unit
    , Right . Cabal.UnknownLicense <$> Dhall.constructor "Unknown" Dhall.string
    , Right Cabal.OtherLicense <$ Dhall.constructor "Other" Dhall.unit
    , Left . SPDX.License <$> Dhall.constructor "SPDX" spdxLicense
    ]
  )


spdxLicense :: Dhall.Decoder SPDX.LicenseExpression
spdxLicense =
  let
    extract e =
      go
        ( Dhall.Core.normalize
            ( e
                `Expr.App` "SPDX"
                `Expr.App` "license"
                `Expr.App` "licenseVersionOrLater"
                `Expr.App` "ref"
                `Expr.App` "refWithFile"
                `Expr.App` "and"
                `Expr.App` "or"
            )
            `asTypeOf` e
        )

    go =
      \case
        Expr.App ( Expr.App "license" identM ) exceptionMayM -> do
          ident <- Dhall.extract spdxLicenseId identM
          exceptionMay <- Dhall.extract ( Dhall.maybe spdxLicenseExceptionId ) exceptionMayM
          return ( SPDX.ELicense ( SPDX.ELicenseId ident ) exceptionMay )

        Expr.App ( Expr.App "licenseVersionOrLater" identM ) exceptionMayM -> do
          ident <- Dhall.extract spdxLicenseId identM
          exceptionMay <- Dhall.extract ( Dhall.maybe spdxLicenseExceptionId ) exceptionMayM
          return ( SPDX.ELicense ( SPDX.ELicenseIdPlus ident ) exceptionMay )

        Expr.App ( Expr.App "ref" identM ) exceptionMayM -> do
          ident <- Dhall.extract Dhall.string identM
          exceptionMay <- Dhall.extract ( Dhall.maybe spdxLicenseExceptionId ) exceptionMayM
          return ( SPDX.ELicense ( SPDX.ELicenseRef ( SPDX.mkLicenseRef' Nothing ident ) ) exceptionMay )

        Expr.App ( Expr.App ( Expr.App "refWithFile" identM ) filenameM) exceptionMayM -> do
          ident <- Dhall.extract Dhall.string identM
          filename <- Dhall.extract Dhall.string filenameM
          exceptionMay <- Dhall.extract ( Dhall.maybe spdxLicenseExceptionId ) exceptionMayM
          return ( SPDX.ELicense ( SPDX.ELicenseRef ( SPDX.mkLicenseRef' ( Just filename ) ident ) ) exceptionMay )

        Expr.App ( Expr.App ( Expr.Var (Expr.V "and" 0)) a ) b ->
          SPDX.EAnd <$> go a <*> go b

        Expr.App ( Expr.App ( Expr.Var (Expr.V "or"  0)) a ) b ->
          SPDX.EOr <$> go a <*> go b

        e ->
          Dhall.typeError expected e

    expected =
      let
        licenseType =
          "SPDX"

        licenseIdAndException =
          (\ id exception ->
              Expr.Pi Nothing "id" id
            $ Expr.Pi Nothing "exception" exception
              licenseType
          )
          <$> Dhall.expected spdxLicenseId
          <*> Dhall.expected ( Dhall.maybe spdxLicenseExceptionId )

        licenseRef =
          (\ ref exception ->
              Expr.Pi Nothing "ref" ref
            $ Expr.Pi Nothing "exception" exception
              licenseType
          )
          <$> Dhall.expected Dhall.string
          <*> Dhall.expected ( Dhall.maybe spdxLicenseExceptionId )

        licenseRefWithFile =
          (\ ref file exception ->
              Expr.Pi Nothing "ref" ref
            $ Expr.Pi Nothing "file" file
            $ Expr.Pi Nothing "exception" exception
              licenseType
          )
          <$> Dhall.expected Dhall.string
          <*> Dhall.expected Dhall.string
          <*> Dhall.expected ( Dhall.maybe spdxLicenseExceptionId )

        combine =
          Expr.Pi Nothing "_" licenseType ( Expr.Pi Nothing "_" licenseType licenseType )

      in
      Expr.Pi Nothing "SPDX" ( Expr.Const Expr.Type )
        <$> ( Expr.Pi Nothing "license" <$> licenseIdAndException
        <*> ( Expr.Pi Nothing "licenseVersionOrLater" <$> licenseIdAndException
        <*> ( Expr.Pi Nothing "ref" <$> licenseRef
        <*> ( Expr.Pi Nothing "refWithFile" <$> licenseRefWithFile
        <*> pure
          ( Expr.Pi Nothing "and" combine
          $ Expr.Pi Nothing "or" combine
            licenseType
          )))))

  in Dhall.Decoder { .. }



spdxLicenseId :: Dhall.Decoder SPDX.LicenseId
spdxLicenseId = Dhall.genericAuto



spdxLicenseExceptionId :: Dhall.Decoder SPDX.LicenseExceptionId
spdxLicenseExceptionId = Dhall.genericAuto



compiler :: Dhall.Decoder ( Cabal.CompilerFlavor, Cabal.VersionRange )
compiler =
  Dhall.record $
    (,)
      <$> Dhall.field "compiler" compilerFlavor
      <*> Dhall.field "version" versionRange



compilerFlavor :: Dhall.Decoder Cabal.CompilerFlavor
compilerFlavor =
  sortType Dhall.genericAuto



repoType :: Dhall.Decoder Cabal.RepoType
repoType =
  sortType Dhall.genericAuto



legacyExeDependency :: Dhall.Decoder Cabal.LegacyExeDependency
legacyExeDependency =
  Dhall.record $ do
    exe <-
      Dhall.field "exe" Dhall.string

    version <-
      Dhall.field "version" versionRange

    pure ( Cabal.LegacyExeDependency exe version )



compilerOptions :: Dhall.Decoder ( Cabal.PerCompilerFlavor [ String ] )
compilerOptions =
  Dhall.record $
    Cabal.PerCompilerFlavor
      <$> Dhall.field "GHC" options
      <*> Dhall.field "GHCJS" options

  where

    options =
      Dhall.list Dhall.string



exeDependency :: Dhall.Decoder Cabal.ExeDependency
exeDependency =
  Dhall.record $ do
    packageName <-
      Dhall.field "package" packageName

    component <-
      Dhall.field "component" unqualComponentName

    version <-
      Dhall.field "version" versionRange

    pure ( Cabal.ExeDependency packageName component version )



language :: Dhall.Decoder Cabal.Language
language =
  sortType Dhall.genericAuto



pkgconfigDependency :: Dhall.Decoder Cabal.PkgconfigDependency
pkgconfigDependency =
  Dhall.record $ do
    name <-
      Dhall.field "name" pkgconfigName

    version <-
      Dhall.field "version" pkgconfigVersionRange

    return
      ( Cabal.PkgconfigDependency
          name
          version
      )



pkgconfigName :: Dhall.Decoder Cabal.PkgconfigName
pkgconfigName =
  Cabal.mkPkgconfigName <$> Dhall.string



executableScope :: Dhall.Decoder Cabal.ExecutableScope
executableScope = Dhall.union
  ( mconcat
    [ Cabal.ExecutablePublic <$ Dhall.constructor "Public" Dhall.unit
    , Cabal.ExecutablePrivate <$ Dhall.constructor "Private" Dhall.unit
    ]
  )



moduleReexport :: Dhall.Decoder Cabal.ModuleReexport
moduleReexport =
  Dhall.record $ do
    original <-
      Dhall.field "original" $
      Dhall.record $ do
        package <-
          Dhall.field "package" ( Dhall.maybe packageName )

        name <-
          Dhall.field "name" moduleName

        pure ( package, name )

    moduleReexportName <-
      Dhall.field "name" moduleName

    pure
      Cabal.ModuleReexport
        { moduleReexportOriginalPackage = fst original
        , moduleReexportOriginalName = snd original
        , ..
        }


foreignLibOption :: Dhall.Decoder Cabal.ForeignLibOption
foreignLibOption = Dhall.union $
  Cabal.ForeignLibStandalone <$ Dhall.constructor "Standalone" Dhall.unit


versionInfo :: Dhall.Decoder Cabal.LibVersionInfo
versionInfo =
  Dhall.record $
  fmap Cabal.mkLibVersionInfo $
    (,,)
      <$> ( fromIntegral <$> Dhall.field "current" Dhall.natural )
      <*> ( fromIntegral <$> Dhall.field "revision" Dhall.natural )
      <*> ( fromIntegral <$> Dhall.field "age" Dhall.natural )



extension :: Dhall.Decoder Cabal.Extension
extension =
  let
    extName :: Cabal.KnownExtension -> StrictText.Text
    extName e =
      StrictText.pack ( show e )

    enableDisable ext enabled = if enabled
      then Cabal.EnableExtension ext
      else Cabal.DisableExtension ext

    constr :: Cabal.KnownExtension -> Dhall.UnionDecoder Cabal.Extension
    constr ext = Dhall.constructor
      ( extName ext )
      ( enableDisable ext <$> Dhall.bool )
  in
    Dhall.union ( foldMap constr [ minBound .. maxBound ] )



guarded
  :: ( Monoid a, Eq a, Diffable a )
  => Dhall.Decoder a
  -> Dhall.Decoder ( Cabal.CondTree Cabal.ConfVar [Cabal.Dependency] a )
guarded t =
  let
    extractConfVar body =
      case body of
        Expr.App ( Expr.App ( Expr.Field "config" ( Dhall.Core.FieldSelection _ "impl" _ ) ) compiler ) version ->
          Cabal.Impl
            <$> Dhall.extract compilerFlavor compiler
            <*> Dhall.extract versionRange version

        Expr.App ( Expr.Field "config" field ) x ->
          case Dhall.Core.fieldSelectionLabel field of
            "os" ->
              Cabal.OS <$> Dhall.extract operatingSystem x

            "arch" ->
              Cabal.Arch <$> Dhall.extract arch x

            "flag" ->
              Cabal.Flag <$> Dhall.extract flagName x

            _ ->
              error "Unknown field"

        _ ->
          error ( "Unexpected guard expression. This is a bug, please report this! I'm stuck on: " ++ show body )

    extract expr =
      configTreeToCondTree [] [] <$> extractConfigTree ( toConfigTree expr )

    extractConfigTree ( Leaf a ) =
      Leaf <$> Dhall.extract t a

    extractConfigTree ( Branch cond a b ) =
      Branch <$> extractConfVar cond <*> extractConfigTree a <*> extractConfigTree b

    configTreeToCondTree confVarsTrue confVarsFalse = \case
      Leaf a ->
        Cabal.CondNode a mempty mempty

      -- The condition has already been shown to hold. Consider only the true
      -- branch and discard the false branch.
      Branch confVar a _impossible | confVar `elem` confVarsTrue ->
        configTreeToCondTree confVarsTrue confVarsFalse a

      -- ...and here, the condition has been shown *not* to hold.
      Branch confVar _impossible b | confVar `elem` confVarsFalse ->
        configTreeToCondTree confVarsTrue confVarsFalse b

      Branch confVar a b ->
        let
          true =
            configTreeToCondTree ( pure confVar <> confVarsTrue ) confVarsFalse a

          false =
            configTreeToCondTree confVarsTrue ( pure confVar <> confVarsFalse ) b

          ( common, true', false' ) =
            diff ( Cabal.condTreeData true ) ( Cabal.condTreeData false )

          ( duplicates, true'', false'' ) =
            diff
              ( Cabal.condTreeComponents true )
              ( Cabal.condTreeComponents false )

        in
          Cabal.CondNode
            common
            mempty
            ( mergeCommonGuards
                ( Cabal.CondBranch
                    ( Cabal.Var confVar )
                    true
                      { Cabal.condTreeData = true'
                      , Cabal.condTreeComponents = true''
                      }
                    ( Just
                        false
                          { Cabal.condTreeData = false'
                          , Cabal.condTreeComponents = false''
                          }
                    )
                : duplicates
                )
            )

    expected =
        Expr.Pi Nothing "_" <$> configRecordType <*> Dhall.expected t

  in Dhall.Decoder { .. }



catCondTree
  :: ( Monoid c, Monoid a )
  => Cabal.CondTree v c a -> Cabal.CondTree v c a -> Cabal.CondTree v c a
catCondTree a b =
  Cabal.CondNode
    { Cabal.condTreeData =
        Cabal.condTreeData a <> Cabal.condTreeData b
    , Cabal.condTreeConstraints =
        Cabal.condTreeConstraints a <> Cabal.condTreeConstraints b
    , Cabal.condTreeComponents =
        Cabal.condTreeComponents a <> Cabal.condTreeComponents b
    }



emptyCondTree :: ( Monoid b, Monoid c ) => Cabal.CondTree a b c
emptyCondTree =
  Cabal.CondNode mempty mempty mempty



mergeCommonGuards
  :: ( Monoid a, Monoid c, Eq v )
  => [Cabal.CondBranch v c a]
  -> [Cabal.CondBranch v c a]
mergeCommonGuards [] =
  []

mergeCommonGuards ( a : as ) =
  let
    ( sameGuard, differentGuard ) =
      partition
        ( ( Cabal.condBranchCondition a == ) . Cabal.condBranchCondition )
        as

  in
    a
      { Cabal.condBranchIfTrue =
          catCondTree
            ( Cabal.condBranchIfTrue a )
            ( foldl
                catCondTree
                emptyCondTree
                ( Cabal.condBranchIfTrue <$> sameGuard )
            )
      , Cabal.condBranchIfFalse =
          Just
            ( catCondTree
              ( fromMaybe emptyCondTree ( Cabal.condBranchIfFalse a ) )
              ( foldl
                  catCondTree
                  emptyCondTree
                  ( fromMaybe emptyCondTree
                      . Cabal.condBranchIfFalse
                      <$> sameGuard
                  )
              )
            )
      }
      : mergeCommonGuards differentGuard



configRecordType :: Dhall.Expector ( Expr.Expr Dhall.Parser.Src Void )
configRecordType =
  let
    predicate on = Dhall.Core.makeRecordField
      $ Expr.Pi Nothing "_" on Expr.Bool

  in
    Expr.Record
      <$> sequenceA
        ( Map.fromList
            [ ( "os", predicate <$> Dhall.expected operatingSystem )
            , ( "arch", predicate <$> Dhall.expected arch )
            , ( "flag", predicate <$> Dhall.expected flagName )
            , ( "impl"
              , Dhall.Core.makeRecordField <$>
                  ( Expr.Pi
                    Nothing
                    "_"
                    <$> Dhall.expected compilerFlavor
                    <*> ( Expr.Pi Nothing "_" <$> Dhall.expected versionRange <*> pure Expr.Bool )
                  )
              )
            ]
        )



genericPackageDescription :: Dhall.Decoder Cabal.GenericPackageDescription
genericPackageDescription =
  let
    namedList k t =
      Dhall.list
        ( Dhall.record
            ( (,)
                <$> Dhall.field "name" unqualComponentName
                <*> Dhall.field k ( guarded t )
            )
        )

  in
    Dhall.record $ do
      packageDescription <-
        packageDescription

      genPackageFlags <-
        Dhall.field "flags" ( Dhall.list flag )

      condLibrary <-
        Dhall.field "library" ( Dhall.maybe ( guarded ( ($ Cabal.LMainLibName) <$> library ) ) )

      condSubLibraries <-
        Dhall.field "sub-libraries" ( Dhall.list subLibrary )

      condForeignLibs <-
        Dhall.field "foreign-libraries" ( namedList "foreign-lib" foreignLib )

      condExecutables <-
        Dhall.field "executables" ( namedList "executable" executable )

      condTestSuites <-
        Dhall.field "test-suites" ( namedList "test-suite" testSuite )

      condBenchmarks <-
        Dhall.field "benchmarks" ( namedList "benchmark" benchmark )

      return Cabal.GenericPackageDescription { .. }



operatingSystem :: Dhall.Decoder Cabal.OS
operatingSystem =
  sortType Dhall.genericAuto



arch :: Dhall.Decoder Cabal.Arch
arch =
  sortType Dhall.genericAuto



flag :: Dhall.Decoder Cabal.Flag
flag =
  Dhall.record $ do
    flagName <-
      Dhall.field "name" flagName

    flagDefault <-
      Dhall.field "default" Dhall.bool

    flagDescription <-
      Dhall.field "description" Dhall.string

    flagManual <-
      Dhall.field "manual" Dhall.bool

    return Cabal.MkFlag { .. }



flagName :: Dhall.Decoder Cabal.FlagName
flagName =
  Cabal.mkFlagName <$> Dhall.string



setupBuildInfo :: Dhall.Decoder Cabal.SetupBuildInfo
setupBuildInfo =
  Dhall.record $ do
    setupDepends <-
      Dhall.field "setup-depends" ( Dhall.list dependency )

    defaultSetupDepends <-
      pure False

    return Cabal.SetupBuildInfo { .. }



filePath :: Dhall.Decoder FilePath
filePath =
  Dhall.string



mixin :: Dhall.Decoder Cabal.Mixin
mixin =
  Dhall.record $ do
    mixinPackageName <-
      Dhall.field "package" packageName

    mixinIncludeRenaming <-
      Dhall.field "renaming" includeRenaming

    pure Cabal.Mixin { .. }



includeRenaming :: Dhall.Decoder Cabal.IncludeRenaming
includeRenaming =
  Dhall.record $ do
    includeProvidesRn <-
      Dhall.field "provides" moduleRenaming

    includeRequiresRn <-
      Dhall.field "requires" moduleRenaming

    pure Cabal.IncludeRenaming { .. }



moduleRenaming :: Dhall.Decoder Cabal.ModuleRenaming
moduleRenaming = Dhall.union
  ( mconcat
    [ Cabal.ModuleRenaming
        <$> Dhall.constructor "renaming"
              ( Dhall.list
                ( Dhall.record
                  ( (,) <$> Dhall.field "rename" moduleName <*> Dhall.field "to" moduleName )
                )
              )
    , Cabal.DefaultRenaming
        <$ Dhall.constructor "default" Dhall.unit
    , Cabal.HidingRenaming
        <$> Dhall.constructor "hiding" ( Dhall.list moduleName )
    ]
  )


libraryVisibility :: Dhall.Decoder Cabal.LibraryVisibility
libraryVisibility = Dhall.union
  ( mconcat
    [ Cabal.LibraryVisibilityPublic <$ Dhall.constructor "public" Dhall.unit
    , Cabal.LibraryVisibilityPrivate <$ Dhall.constructor "private" Dhall.unit
    ]
  )


sortType :: Dhall.Decoder a -> Dhall.Decoder a
sortType t =
  t { Dhall.expected = sortExpr <$> Dhall.expected t }
