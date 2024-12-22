+++
title = "GHC 9.12 & cabal 3.14 releases"
date = 2024-12-22
[taxonomies]
authors = ["Hécate"]
categories = ["Cabal", "GHC"]
tags = ["Release"]
+++

To conclude the year 2024, the GHC and Cabal teams are happy to announce the release of GHC 9.12 and cabal 3.14.

<!-- more -->


Here are some highlights:

## GHC 9.12

### Language Extensions

This release brings its lot of new and exciting extensions:
* [Multiline string literals][MultilineStrings] to spread a piece of text over several lines;
* [Or-Patterns][OrPatterns], allowing you to match on several patterns in a single `case` branch:

```haskell
data Sweet = Cupcake | Liquorice | Cookie | Raisins

-- Without Or-Patterns
tasty Cupcake = True
tasty Cookie = True
tasty _ = False

-- With Or-Patterns
tasty (Cupcake; Cookie) = True
tasty (Liquorice; Raisins) = False
```

* [NamedDefaults][NamedDefaults] allows you to create type defaulting declarations for literals other than for the `Num` class. This mechanism is used to make sure that in the expression `print (6 + 7)`, the multiplication gets a concrete type like `Integer`, `Int` or `Double`, instead of the vague  `Num a => a`.  
You can now apply this mechanism to other typeclasses, like:

```haskell
default IsString (Text, String)
```

Which means that in a module with [`OverloadedStrings`][OverloadedStrings], string literals `"like this"` will default to [`Text`][Text] instead of a vague polymorphic type.

### Base Library

* Improvements to backtraces and exceptions. Lots of them.

#### ⚠️ Deprecations

* The deprecation cycle of [GHC.Pack][GHC.Pack] has come to an end. Goodbye!
* [GHC.Desugar][GHC.Desugar] will be removed in GHC 9.14.

### Code Generation

* Experimental support for the [RISC-V Platform](https://gitlab.haskell.org/ghc/ghc/-/issues/16783);
* SIMD! In the x86: Most floating-point operations and some integer vector operations are supported 128-bit vectors **without LLVM**. [Get in touch](https://matrix.to/#/#ghc:matrix.org) to help with this effort
* You can try the new experimental [`-fobject-determinism`][-fobject-determinism] flag to enable deterministic object code generation.

#### ⚠️ Deprecations

* Support 32-bit Windows & macOS/iOS has been dropped;
* As a result, friendship with the `stdcall` calling convention for FFI is ended. Now `ccall` is your best friend.

You can read the full release notes [here](https://downloads.haskell.org/ghc/9.12.1/docs/users_guide/9.12.1-notes.html).

## Cabal 3.14

### New things

* (Spec v3.14 only) New field: `extra-files` allows you to bundle files in the source distribution (*sdist*) of your cabal package. It serves as an alternative when the files you want to bundle do not fit in the existing `data-files` (for runtime data), `extra-source-files` (built by cabal) or `extra-doc-files` (shipped with Haddocks). This field has no inherent meaning, to avoid misuse of the already existing fields. For instance, you can use `extra-files` to ship `stack.yaml` files, which are not used either by the program at run-time, nor by Cabal or Haddock.

* You can now compile projects dynamically with profiling enabled. The following options are now available:
  * cabal.project: `profiling-shared: <Boolean>`;
  * cabal file: `ghc-prof-shared-options` for passing options when building in profiling dynamic way;
  * Command-line arguments: `--enable-profiling --enable-executable-dynamic`.

* [New GHC options and extensions](https://downloads.haskell.org/ghc/9.12.1/docs/users_guide/9.12.1-notes.html) are supported.

* New build type: Hooks. This build type, intended to eventually replace the Custom build type, integrates better with the rest of the ecosystem (cabal-install, Haskell Language Server).

* The experimental `haddock-project` command supports sub-components.

### Changed

* `cabal init` remembers the chosen language within current session (`Haskell2010`, `GHC2021`, etc.).

* `cabal check` will warn about the insecure `git://` protocol in source-repository.

* Enable recompilation avoidance during Haddock generation.

* Clarify error message when `pkg-config` is not found.

* Print out which project file(s) we are using.

### Fixed

* The `--promised-dependency` flag now accepts the version of a package in the package name. Previously you could only call it with an argument like `void=void-0.5.8`.
Now, it is possible to write: `--promised-dependency=void-0.5.8=void-0.5.8`.

* Always pass `ghc-options` to GHC.

* Enhance error detection for cabal root project files, including broken symlinks.

* Don't pass `--coverage-for` for non-dependency libs of testsuite.

* Fix a bug that causes `cabal init` to crash if `git` is not installed.

### Unresolved

* Recompilation avoidance during Haddock generation sometimes does not work on Windows ([haskell/cabal#9177](https://github.com/haskell/cabal/pull/9177#issuecomment-2167768305)).

You can see the full changelogs for [Cabal & Cabal-syntax](https://github.com/haskell/cabal/blob/master/release-notes/Cabal-3.14.1.0.md), and for [cabal-install and cabal-install-solver](https://github.com/haskell/cabal/blob/master/release-notes/cabal-install-3.14.0.0.md)

As always, the Cabal team is always welcoming of new contributors. We have a nice back catalogue of bugs, oddities and feature requests. Stop by our [matrix channel](https://matrix.to/#/#hackage:matrix.org)!

[MultilineStrings]: https://downloads.haskell.org/ghc/9.12.1/docs/users_guide/exts/multiline_strings.html#multiline-string-literals
[OrPatterns]: https://downloads.haskell.org/ghc/9.12.1/docs/users_guide/exts/or_patterns.html#or-patterns
[NamedDefaults]: https://downloads.haskell.org/ghc/9.12.1/docs/users_guide/exts/named_defaults.html#named-default-declarations
[OverloadedStrings]: https://downloads.haskell.org/ghc/9.12.1/docs/users_guide/exts/overloaded_strings.html
[Text]: https://hackage.haskell.org/package/text/docs/Data-Text.html#t:Text
[-fobject-determinism]: https://downloads.haskell.org/ghc/9.12.1/docs/users_guide/using-optimisation.html#ghc-flag-fobject-determinism
[GHC.Pack]: https://gitlab.haskell.org/ghc/ghc/-/issues/21461
[GHC.Desugar]: https://hackage.haskell.org/package/base-4.21.0.0/docs/GHC-Desugar.html
