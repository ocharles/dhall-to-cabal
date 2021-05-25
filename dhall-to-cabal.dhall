let prelude = ./dhall/prelude.dhall

let types = ./dhall/types.dhall

let v = prelude.v

let anyVersion = prelude.anyVersion

let package =
        λ(package : Text)
      → λ(version-range : types.VersionRange)
      → { bounds = version-range, package = package, library-names = [ types.LibraryName.main-library ] }

let majorVersions = prelude.utils.majorVersions

let deps =
      { Cabal =
          majorVersions "Cabal" [ v "3.0" ] [ types.LibraryName.main-library ]
      , Diff =
          majorVersions "Diff" [ v "0.3.4", v "0.4.0" ] [ types.LibraryName.main-library ]
      , base =
          majorVersions "base" [ v "4.11", v "4.12" ] [ types.LibraryName.main-library ]
      , bytestring =
          majorVersions "bytestring" [ v "0.10" ] [ types.LibraryName.main-library ]
      , containers =
          majorVersions "containers" [ v "0.5", v "0.6" ] [ types.LibraryName.main-library ]
      , directory =
          majorVersions "directory" [ v "1.3.0.2" ] [ types.LibraryName.main-library ]
      , dhall =
          majorVersions "dhall" [ v "1.26.0", v "1.27.0" ] [ types.LibraryName.main-library ]
      , dhall-to-cabal =
          package "dhall-to-cabal" anyVersion
      , filepath =
          majorVersions "filepath" [ v "1.4" ] [ types.LibraryName.main-library ]
      , microlens =
          majorVersions
          "microlens"
          [ v "0.1.0.0", v "0.2.0.0", v "0.3.0.0", v "0.4.0.0" ]
          [ types.LibraryName.main-library ]
      , optparse-applicative =
          majorVersions
          "optparse-applicative"
          [ v "0.13.2", v "0.14", v "0.15" ]
          [ types.LibraryName.main-library ]
      , prettyprinter =
          majorVersions "prettyprinter" [ v "1.2.0.1", v "1.3.0", v "1.4.0", v "1.5.0" ] [ types.LibraryName.main-library ]
      , contravariant =
          majorVersions "contravariant" [ v "1.4", v "1.5" ] [ types.LibraryName.main-library ]
      , tasty =
          majorVersions
          "tasty"
          [ v "0.11", v "0.12", v "1.0", v "1.1", v "1.2" ]
          [ types.LibraryName.main-library ]
      , tasty-golden =
          majorVersions "tasty-golden" [ v "2.3" ] [ types.LibraryName.main-library ]
      , tasty-hunit =
          majorVersions "tasty-hunit" [ v "0.10.0.1" ] [ types.LibraryName.main-library ]
      , text =
          majorVersions "text" [ v "1.2" ] [ types.LibraryName.main-library ]
      , transformers =
          majorVersions "transformers" [ v "0.5.2" ] [ types.LibraryName.main-library ]
      , vector =
          majorVersions "vector" [ v "0.12" ] [ types.LibraryName.main-library ]
      }

let warning-options =
      [ "-Weverything"
      , "-Wno-safe"
      , "-Wno-unsafe"
      , "-Wno-implicit-prelude"
      , "-Wno-missed-specialisations"
      , "-Wno-all-missed-specialisations"
      , "-Wno-missing-import-lists"
      , "-Wno-missing-local-signatures"
      , "-Wno-monomorphism-restriction"
      , "-fno-warn-name-shadowing"
      ]

let addCommonBuildInfo =
        λ(component : types.BuildInfo)
      →   component
        ⫽ { compiler-options =
                component.compiler-options
              ⫽ { GHC = component.compiler-options.GHC # warning-options }
          , build-depends =
              [ deps.base, deps.dhall ] # component.build-depends
          , default-language =
              Some types.Language.Haskell2010
          }

in  prelude.utils.mapBuildInfo
    addCommonBuildInfo
    (   prelude.utils.GitHub-project
        { owner = "ocharles", repo = "dhall-to-cabal" }
      ⫽ { cabal-version =
            v "2.4"
        , synopsis =
            "Compile Dhall expressions to Cabal files"
        , description =
            ''
            dhall-to-cabal takes Dhall expressions and compiles them into Cabal
            files. All of the features of Dhall are supported, such as let
            bindings and imports, and all features of Cabal are supported
            (including conditional stanzas).
            ''
        , category =
            "Distribution"
        , build-type =
            Some types.BuildType.Simple
        , maintainer =
            "ollie@ocharles.org.uk"
        , author =
            "Ollie Charles <ollie@ocharles.org.uk>"
        , extra-source-files =
            [ "Changelog.md"
            , "README.md"
            , "dhall/**/*.dhall"
            , "golden-tests/dhall-to-cabal/*.dhall"
            , "golden-tests/dhall-to-cabal/*.cabal"
            , "golden-tests/cabal-to-dhall/*.dhall"
            , "golden-tests/cabal-to-dhall/*.cabal"
            ]
        , license =
            types.License.MIT
        , license-files =
            [ "LICENSE" ]
        , version =
            v "1.3.4.0"
        , library =
            prelude.unconditional.library
            (   prelude.defaults.MainLibrary
              ⫽ { build-depends =
                    [ deps.Cabal
                    , deps.bytestring
                    , deps.containers
                    , deps.contravariant
                    , deps.filepath
                    , deps.microlens
                    , deps.text
                    , deps.transformers
                    , deps.vector
                    ]
                , autogen-modules =
                    [ "Paths_dhall_to_cabal" ]
                , exposed-modules =
                    [ "CabalToDhall"
                    , "DhallLocation"
                    , "DhallToCabal"
                    , "DhallToCabal.FactorType"
                    , "DhallToCabal.Util"
                    ]
                , hs-source-dirs =
                    [ "lib" ]
                , other-extensions =
                    [ types.Extension.ApplicativeDo True
                    , types.Extension.GADTs True
                    , types.Extension.GeneralizedNewtypeDeriving True
                    , types.Extension.LambdaCase True
                    , types.Extension.OverloadedStrings True
                    , types.Extension.RecordWildCards True
                    , types.Extension.TypeApplications True
                    ]
                , other-modules =
                    [ "DhallToCabal.ConfigTree"
                    , "DhallToCabal.Diff"
                    , "Dhall.Extra"
                    , "Paths_dhall_to_cabal"
                    ]
                }
            )
        , executables =
            [ prelude.unconditional.executable
              "dhall-to-cabal"
              (   prelude.defaults.Executable
                ⫽ { build-depends =
                      [ deps.Cabal
                      , deps.containers
                      , deps.dhall-to-cabal
                      , deps.directory
                      , deps.filepath
                      , deps.microlens
                      , deps.optparse-applicative
                      , deps.prettyprinter
                      , deps.text
                      , deps.transformers
                      ]
                  , hs-source-dirs =
                      [ "exe" ]
                  , main-is =
                      "Main.hs"
                  , other-extensions =
                      [ types.Extension.NamedFieldPuns True ]
                  , other-modules =
                      [ "Paths_dhall_to_cabal" ]
                  , autogen-modules =
                      [ "Paths_dhall_to_cabal" ]
                  }
              )
            , prelude.unconditional.executable
              "cabal-to-dhall"
              (   prelude.defaults.Executable
                ⫽ { build-depends =
                      [ deps.bytestring
                      , deps.dhall-to-cabal
                      , deps.optparse-applicative
                      , deps.prettyprinter
                      , deps.text
                      ]
                  , hs-source-dirs =
                      [ "cabal-to-dhall" ]
                  , main-is =
                      "Main.hs"
                  , other-extensions =
                      [ types.Extension.NamedFieldPuns True ]
                  , other-modules =
                      [ "Paths_dhall_to_cabal" ]
                  , autogen-modules =
                      [ "Paths_dhall_to_cabal" ]
                  }
              )
            , prelude.unconditional.executable
              "dhall-to-cabal-meta"
              (   prelude.defaults.Executable
                ⫽ { scope =
                      types.Scope.Private
                  , build-depends =
                      [ deps.directory
                      , deps.dhall-to-cabal
                      , deps.filepath
                      , deps.optparse-applicative
                      , deps.prettyprinter
                      ]
                  , hs-source-dirs =
                      [ "meta" ]
                  , main-is =
                      "Main.hs"
                  }
              )
            ]
        , test-suites =
            [ prelude.unconditional.test-suite
              "golden-tests"
              (   prelude.defaults.TestSuite
                ⫽ { build-depends =
                      [ deps.Cabal
                      , deps.Diff
                      , deps.bytestring
                      , deps.dhall-to-cabal
                      , deps.filepath
                      , deps.microlens
                      , deps.prettyprinter
                      , deps.tasty
                      , deps.tasty-golden
                      , deps.text
                      ]
                  , hs-source-dirs =
                      [ "golden-tests" ]
                  , type =
                      types.TestType.exitcode-stdio
                      { main-is = "GoldenTests.hs" }
                  }
              )
            , prelude.unconditional.test-suite
              "unit-tests"
              (   prelude.defaults.TestSuite
                ⫽ { build-depends =
                      [ deps.Cabal
                      , deps.dhall-to-cabal
                      , deps.tasty
                      , deps.tasty-hunit
                      , deps.text
                      ]
                  , hs-source-dirs =
                      [ "tests" ]
                  , type =
                      types.TestType.exitcode-stdio { main-is = "Tests.hs" }
                  , other-modules =
                      [ "DhallToCabal.Tests" ]
                  }
              )
            ]
        }
    )
