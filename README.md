# `dhall-to-cabal` - generate Cabal files from Dhall expression

`dhall-to-cabal` takes [Dhall](https://dhall-lang.org) expressions and compiles
them into [Cabal](https://www.haskell.org/cabal/) files. All of the features of
Dhall are supported, such as `let` bindings and imports, and all features of
Cabal are supported (including conditional stanzas).

## Getting Started

To get started with `dhall-to-cabal`, first install the `dhall-to-cabal`
executable. This can be done using cabal:

```sh
cabal install dhall-to-cabal
```

(You may need to run `cabal update` first).

Next, write the Dhall expression that you would like to compile to a Cabal file.
For some example Dhall files, see

* [`dhall-to-cabal.dhall`](https://github.com/ocharles/dhall-to-cabal/blob/1.0-RC1/dhall-to-cabal.dhall) -
  the Dhall expression building the Cabal file of this very project. 
* [`lens.dhall`](https://github.com/ocharles/dhall-to-cabal/blob/1.0-RC1/golden-tests/lens.dhall) - 
  a subset of the `lens` library's Cabal file, translated to Dhall.
* [`str-sig.dhall`](https://github.com/ocharles/dhall-to-cabal/blob/1.0-RC1/golden-tests/str-sig.dhall) -
  an example of using Backpack.

To see the full schema supported by `dhall-to-cabal`, you can run
`dhall-to-cabal --print-type`. (In the future, we will have more human-ready
documentation - we'd love your help here!)

Once you have your Dhall file ready, point `dhall-to-cabal` at it:

``` shell
dhall-to-cabal input.dhall
```

(Replace the name `input.dhall` with your actual input file).

That's it! `dhall-to-cabal` does the rest - it will figure out what
the `.cabal` file should be called, and write it out next to the
input.

If you'd rather put the file somewhere else, have a look at
`dhall-to-cabal --help` and use one of the `--output-*` options.

### cabal-to-dhall

Good news! `dhall-to-cabal` is currently running a two-for-one deal! By
installing `dhall-to-cabal` we'll throw in the `cabal-to-dhall` executable
*absolutely free*! `cabal-to-dhall` does the reverse of `dhall-to-cabal` -
taking `.cabal` files and transforming them into appropriate Dhall expressions.
This can be a great way to get started with `dhall-to-cabal`.

## The Details

As the name suggests, `dhall-to-cabal` takes
[Dhall](https://dhall-lang.org) expressions and compiles them into
[Cabal](https://www.haskell.org/cabal/) files. There are two moving pieces here,
so let's break it down.

## Dhall?

Dhall is a relatively new language [started by Gabriel
Gonzales](http://www.haskellforall.com/2016/12/dhall-non-turing-complete-configuration.html)
in late 2016. The language bills itself as "a configuration language guaranteed
to terminate". In terms of features, we have a language with:

* Let bindings and lambdas for abstraction
* Built-in support for primitive types - naturals, doubles, integers, text, booleans
* Composite types - optional values (like `Maybe` in Haskell), lists and records
* An import mechanism to import expressions from foreign sources (local files or
  over HTTP)
* A type system to type check all of the above
* [Standards](https://github.com/dhall-lang/dhall-lang/tree/master/standard#semantics)
  to promote adoption in multiple languages.

To give you an example of a Dhall expression, let's jump straight in and see an
example of a Dhall expression that can be used with `dhall-to-cabal`:

```dhall
let GitHub-project =
      https://raw.githubusercontent.com/dhall-lang/dhall-to-cabal/1.3.4.0/dhall/utils/GitHub-project.dhall

let prelude =
      https://raw.githubusercontent.com/dhall-lang/dhall-to-cabal/1.3.4.0/dhall/prelude.dhall

in    GitHub-project { owner = "ocharles", repo = "example" }
    ⫽ { version =
          prelude.v "1.0.0"
      , library =
          prelude.unconditional.library
          (   prelude.defaults.MainLibrary
            ⫽ { build-depends =
                  [ { package =
                        "base"
                    , bounds =
                        prelude.majorBoundVersion (prelude.v "4")
                    }
                  ]
              , exposed-modules =
                  [ "Hello.World" ]
              }
          )
      }
```

We can see quite a few features in use here. Ignoring what this file actually
does (for now), let's focus on the language features.

At the start of we can see a few `let` bindings whose values themselves are
imports over HTTP. Finally, we get into the body of the expression which is
formed from two parts - function application of `GitHub-project` which is given
a record as argument (with `owner` and `repo` `Text` fields), and another larger
record. The result of `GitHub-project` and the `library`-containing record are
then "merged" together using the `⫽` operator. In this case, you can think of
this as overriding or extending the result of `GitHub-project` with an extra
`library` field.

It's OK if you don't follow all of the above - my goal is to simply get you
familiar with what a Dhall expression would look like. The Dhall language itself
has a [much more detailed
tutorial](https://hackage.haskell.org/package/dhall-1.9.1/docs/Dhall-Tutorial.html)
and [other documentation](https://github.com/dhall-lang/dhall-lang/wiki).

## Cabal?

Cabal is something that you might already be familiar with, but if you're not, Cabal is:

> a system for building and packaging Haskell libraries and programs. It defines
> a common interface for package authors and distributors to easily build their
> applications in a portable way. Cabal is part of a larger infrastructure for
> distributing, organizing, and cataloging Haskell libraries and programs.

(taken from [the Cabal homepage](https://www.haskell.org/cabal/)).

Almost all Haskell libraries are built using some parts of the Cabal system.

For the purposes of this project, we're interested in `.cabal` files themselves.
Cabal is a build-system, but it's also a domain specific language for *driving*
this build system.

A small example Cabal file is

```cabal
cabal-version: 2.2
name: example
version: 1.0.0
homepage: https://github.com/ocharles/example
bug-reports: https://github.com/ocharles/example/issues

source-repository head
    type: git
    location: https://github.com/ocharles/example

library
    exposed-modules:
        Hello.World
    build-depends:
        base ^>=4
```

In this, we see some leading metadata about the Haskell package itself - the
name of the package, its homepage and its license, and so on. Towards the end of
the file, we add a library component to the package. To build this library, we
depend on the `base` library (which contains the standard Haskell prelude) and
will expose the `Hello.World` module as our API.

## dhall-to-cabal?

`dhall-to-cabal` tries to bridge the gap between these two separate worlds by
allowing users to drive the Cabal build system via Dhall expressions. If you
hadn't noticed by now, you've already seen an input Dhall-to-Cabal expression,
and you've also already seen the corresponding output!

### Getting Started

You can get `dhall-to-cabal` [from
Hackage](https://hackage.haskell.org/package/dhall-to-cabal). For usage, run
`dhall-to-cabal --help`. For some example Dhall expressions, see
[`dhall-to-cabal.dhall`](https://github.com/ocharles/dhall-to-cabal/blob/1.0-RC1/dhall-to-cabal.dhall),
[`lens.dhall`](https://github.com/ocharles/dhall-to-cabal/blob/1.0-RC1/golden-tests/lens.dhall)
or
[`str-sig.dhall`](https://github.com/ocharles/dhall-to-cabal/blob/1.0-RC1/golden-tests/str-sig.dhall).
These are all fairly big expressions to test the project itself... if you have a
simpler example that you think would act as good reference, that would make a
great starting pull request!

## Why?

I wrote dhall-to-cabal for a few reasons. Firstly, let's start with the
self-centered arguments:

* I wanted to learn more about Dhall. Specifically, I wanted to get comfortable
  with what I could do with the language, and how I could use it to solve
  problems in a specific domain. I'm beginning to view it in the same light as I
  would view [Lua](https://www.lua.org/) and would like to explore that more in
  the future.

* I wanted to help drive Dhall development, as before even getting stuck in I
  believed it was a worth-while project. I think even the 1.0 release of
  dhall-to-cabal has been of value - I've reported 18 issues (14 of which have
  been closed, usually fixed!), and I think we're starting to have discussions
  in Dhall that are helping drive the overall project forward.

* [It was requested](https://github.com/dhall-lang/dhall-haskell/issues/78)
  and I agree that it sounds like a project that should exist. The existence of
  [`hpack-dhall`](https://github.com/sol/hpack-dhall) strengthens this argument.

These are enough for me to justify the work, but it doesn't necessarily imply
that the project is of value. However, I think there *is* some value in this
project.

* Cabal (the language) is limited in expressivity. We only just got `common`
  blocks that allow us to abstract *some* details out and share them between
  stanzas, but they still don't even come close to the amount of expressivity
  given with `let`, function abstraction and remote imports.

* What a Cabal file "is" has historically been somewhat vague. Thankfully, there
  has been a recent effort to start moving stuff into [an excellent Read the
  Docs user's guide](https://www.haskell.org/cabal/users-guide/), but this is
  still incomplete. A search for "backpack" and "mixins" returns no results,
  though these *are* valid keywords in the library stanza.
  
  Dhall-to-Cabal does not suffer from the mismatch between documentation and
  internal implementation because as Dhall is type-checked we *always* have a
  canonical type of Cabal packages.

* Cabal is one-size fits all, but is that actually true? Some languages require
  special treatment, and the best we can do right now is to document that in the
  README. Take the [`network`
  library](https://hackage.haskell.org/package/network):
  
  > In network-2.6 the Network.URI module was split off into its own package, network-uri-2.6. If you're using the Network.URI module you can automatically get it from the right package by adding this to your .cabal file:

  That's right - people have to copy and paste some code. I couldn't imagine
  requiring people do that in actual Haskell code, so why do we treat Cabal
  files differently?
  
  In Dhall-to-Cabal, we can hide away the fiddly details in a separate Dhall
  expression, and provide it as something that users can import.
  
* We can go beyond Cabal files. If Cabal is a domain specific language for
  building Haskell projects, what does a domain specific language for building
  Haskell *web applications* look like? Does the separate of `library`,
  `executable`, and `test-suite` make sense here? Maybe we'd rather:
  
  ```haskell
  servant-project {
    api-route = "My.API.Route"
    server = "My.API.Server"
    models = [ "My.API.Pancake", "My.API.Waffle" ]
  }
  ```
  
  and have this take care of some other details.
  
  I don't know what this bit of the future looks like, but I think there is
  certainly something there.

  We can also see the desire to have this import/reuse functionality in core
  libraries. The [`network`](https://hackage.haskell.org/package/network)
  library opens with a stanza about how you have to add the library to your
  cabal file which is totally non-standard. Just give me an expression to
  import, and then I don't have to worry about the details!
