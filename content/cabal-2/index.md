+++
title = "What's new in Cabal/cabal-install 2.0"
date = 2017-09-11
[taxonomies]
authors = ["23Skidoo"]
categories = ["Cabal"]
tags = ["Announcement"]
+++

This release brings improved new-build, Backpack and foreign libraries.

<!-- more -->

A couple of weeks ago we've quietly released versions 2.0 of both
Cabal and `cabal-install` after approximately a year of
development. The 2.0 release incorporates more than 1500 commits by
[64 different
contributors](https://gist.github.com/23Skidoo/7109479c43a8de44f8e29fa335e9645c). This
post serves as a formal release announcement and describes what's new
and improved in version 2.0.

There is a number of backwards-incompatible Cabal library API changes
in this release that affect packages with Custom setup
scripts. Therefore `cabal-install` will by default use a previous
version of Cabal to build setup scripts that don't [explicitly declare
compatibility with Cabal
2.0](https://www.well-typed.com/blog/2015/07/cabal-setup-deps/). The
[2.0 migration
guide](https://github.com/haskell/cabal/wiki/2.0-migration-guide)
gives advice for package authors on how to adapt Custom setup scripts
to backwards-incompatible changes in this release.

## Major new features

- Much improved [`new-build`
  feature](https://www.haskell.org/cabal/users-guide/nix-local-build-overview.html)
  (also known as nix-style local builds), that solves many
  long-standing problems and is going to become the default mode of
  operation of `cabal-install` in version 3.0 (tentative release date:
  Autumn 2018). Killer features of `new-build` are reproducible
  isolated builds with global dependency caching and multi-package
  projects. For a more extensive introduction to `new-build`, see
  [this blog post by Edward
  Z. Yang](http://blog.ezyang.com/2016/05/announcing-cabal-new-build-nix-style-local-builds/).

- Support for Backpack, a new system for mix-in packages. See [this
  article by Edward
  Z. Yang](https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst)
  for an introduction to Backpack and its features.

- Native suport for [foreign
  libraries](https://github.com/haskell/cabal/pull/2540): Haskell
  libraries that are intended to be used by non-Haskell code. See
  [this section of the user
  guide](https://www.haskell.org/cabal/users-guide/developing-packages.html#foreign-libraries)
  for an introduction to this feature.

- Convenience/internal libraries are now supported
  ([#269](https://github.com/haskell/cabal/issues/269)). An internal
  library is declared using the stanza `library 'libname'` and can be
  only used by other components inside a package.

- Package components can be now built and installed in parallel. This
  is especially handy when compiling packages with large numbers of
  independent components (usually those are executables). As a
  consequence, the `Setup.hs` command-line interface [now allows to
  specify the component to be
  configured](https://github.com/ghc-proposals/ghc-proposals/pull/4).

- [Nix package manager integration](https://www.haskell.org/cabal/users-guide/nix-integration.html)
  ([#3651](https://github.com/haskell/cabal/issues/3651)).

- [New `cabal-install` command: `outdated`](https://www.haskell.org/cabal/users-guide/developing-packages.html?highlight=outdated#listing-outdated-dependency-version-bounds), for listing outdated
  version bounds in a `.cabal` file or a freeze file
  ([#4201](https://github.com/haskell/cabal/issues/4207)). Work on
  this feature was sponsored by [Scrive AB](https://scrive.com/).

- [New `cabal-install` command
  `reconfigure`](https://www.haskell.org/cabal/users-guide/installing-packages.html?highlight=reconfigure),
  which re-runs `configure` with the most recently used flags
  ([#2214](https://github.com/haskell/cabal/issues/2214)).

- Package repos are now assumed to be `hackage-security`-enabled by
  default. If a `remote-repo` section in `~/.cabal/config` doesn't
  have an explicit `secure` field, it now defaults to `secure: True`,
  unlike in `cabal-install` 1.24. See [this post on the Well-Typed
  blog](https://www.well-typed.com/blog/2015/04/improving-hackage-security/)
  for an introduction to `hackage-security` and what benefits it
  brings.

- New caret-style version range operator `^>=`
  ([#3705](https://github.com/haskell/cabal/issues/3705)) that is
  equivalent to `>=` intersected with an automatically inferred major
  upper bound. For example, `foo ^>= 1.3.1` is equivalent to `foo >=
1.3.1 && < 1.4`. Besides being a convenient syntax sugar, `^>=`
  allows to distinguish "strong" and "weak" upper bounds: `foo >=
1.3.1 && < 1.4` means "I know for sure that my package doesn't work
  with `foo-1.4`", while `foo ^>= 1.3.1` means "I don't know whether
  `foo-1.4`, which is not out yet, will break my package, but I want
  to be cautious and follow [PVP](https://pvp.haskell.org/)". In the
  future, this feature will allow to implement automatic version
  bounds relaxation in a formally sound way (work on this front is
  progressing on
  [`matrix.hackage.haskell.org`](https://matrix.hackage.haskell.org/)). See
  [this section of the
  manual](https://www.haskell.org/cabal/users-guide/developing-packages.html?highlight=caret#pkg-field-build-depends)
  for more information.

- Changed `cabal upload` to upload a package candidate by default
  ([#3419](https://github.com/haskell/cabal/issues/3419)). Same
  applies to uploading documentation. Also added a new `cabal upload`
  flag `--publish` for publishing a package on Hackage instead of
  uploading a candidate
  ([#3419](https://github.com/haskell/cabal/issues/3419)).

- [Support for
  `--allow-older`](https://www.haskell.org/cabal/users-guide/nix-local-build.html?highlight=allow%20older#cfg-flag---allow-older)
  (dual to `--allow-newer`)
  ([#3466](https://github.com/haskell/cabal/issues/3466)).

- [New `build-tool-depends`
  field](https://www.haskell.org/cabal/users-guide/developing-packages.html?highlight=build%20tool%20depends#pkg-field-build-tool-depends)
  that replaces `build-tools` and has a better defined semantics
  ([#3708](https://github.com/haskell/cabal/issues/3708),
  [#1541](https://github.com/haskell/cabal/issues/1541)). `cabal-install`
  will now install required build tools and add them to PATH
  automatically.

- [New `autogen-modules`
  field](https://www.haskell.org/cabal/users-guide/developing-packages.html?highlight=autogen%20modules#pkg-field-custom-setup-autogen-modules)
  for automatically generated modules (like `Paths_PACKAGENAME`) that
  are not distributed inside the package tarball
  ([#3656](https://github.com/haskell/cabal/issues/3656)).

- [Added a new `scope`
  field](https://www.haskell.org/cabal/users-guide/developing-packages.html?highlight=scope#pkg-field-executable-scope)
  to the `executable` stanza
  ([#3461](https://github.com/haskell/cabal/issues/3461)). Executable
  scope can be either `public` or `private`; private executables are
  those that are expected to be run by other programs rather than
  users and get installed into
  `$libexecdir/$libexecsubdir`. Additionally, `$libexecdir` now has a
  subdir structure similar to `$lib(sub)dir` to allow installing
  private executables of different packages and package versions
  alongside one another.

- New `--index-state` flag for requesting a specific version of
  the package index
  ([#3893](https://github.com/haskell/cabal/issues/3893),
  [#4115](https://github.com/haskell/cabal/issues/4115)).

- Added `CURRENT_PACKAGE_VERSION` CPP constant to `cabal_macros.h`
  ([#4319](https://github.com/haskell/cabal/issues/4319)).

## Minor improvements and bug fixes

- Dropped support for versions of GHC earlier than 6.12
  ([#3111](https://github.com/haskell/cabal/issues/3111)). Also, GHC
  compatibility window for the Cabal library has been extended to five
  years ([#3838](https://github.com/haskell/cabal/issues/3838)).

- Added a technical preview version of the 'cabal doctest' command
  ([#4480](https://github.com/haskell/cabal/issues/4480)).

- Cabal now invokes GHC with `-Wmissing-home-modules`, if that flag is
  supported (added in version 8.2). This means that you'll get a
  warning if you forget to list a module in `other-modules` or
  `exposed-modules`
  ([#4254](https://github.com/haskell/cabal/pull/4254)).

- Verbosity `-v` now takes an extended format which allows specifying
  exactly what you want to be logged. The format is
  `[silent|normal|verbose|debug] flags`, where `flags` is a space
  separated list of flags. At the moment, only the flags `+callsite`
  and `+callstack` are supported; these report the call site/stack of
  a logging output respectively (these are only supported if Cabal is
  built with GHC 8.0/7.10.2 or greater, respectively).

- The `-v/--verbosity` option no longer affects GHC verbosity (except
  in the case of `-v0`). Use `--ghc-options=-v` to enable verbose GHC
  output ([#3540](https://github.com/haskell/cabal/issues/3540),
  [#3671](https://github.com/haskell/cabal/issues/3671)).

- Packages which use internal libraries can result in multiple
  registrations; thus `--gen-pkg-config` can now output a directory of
  registration scripts rather than a single file.

- Changed the default logfile template from `.../$pkgid.log` to
  `.../$compiler/$libname.log`
  ([#3807](https://github.com/haskell/cabal/issues/3807)).

- Macros in 'cabal_macros.h' are now `#ifndef`'d, so that they don't
  cause an error if the macro is already defined
  ([#3041](https://github.com/haskell/cabal/issues/3041)).

- Added qualified constraints for setup dependencies. For example,
  `--constraint="setup.bar == 1.0"` constrains all setup dependencies
  on bar, and `--constraint="foo:setup.bar == 1.0"` constrains foo's
  setup dependency on bar (part of
  [#3502](https://github.com/haskell/cabal/issues/3502)).

- Non-qualified constraints, such as `--constraint="bar == 1.0"`, now
  only apply to top-level dependencies. They don't constrain setup or
  build-tool dependencies.

  The new syntax `--constraint="any.bar ==1.0"` constrains all uses of bar.

- Added a new solver flag, `--allow-boot-library-installs`, that
  allows normally non-upgradeable packages like `base` to be installed
  or upgraded
  ([#4209](https://github.com/haskell/cabal/issues/4209)). Made the
  'template-haskell' package non-upgradable again
  ([#4185](https://github.com/haskell/cabal/issues/4185)).

- Fixed password echoing on MinTTY
  ([#4128](https://github.com/haskell/cabal/issues/4128)).

- Added optional solver output visualisation support via the
  `tracetree` package
  ([#3410](https://github.com/haskell/cabal/issues/3410)). Mainly
  intended for debugging.

- New `./Setup configure` flag `--cabal-file`, allowing multiple
  .cabal files in a single directory
  ([#3553](https://github.com/haskell/cabal/issues/3553)). Primarily
  intended for internal use.

- Removed the `--check` option from `cabal upload`
  ([#1823](https://github.com/haskell/cabal/issues/1823)). It was
  replaced by [Hackage package
  candidates](https://hackage.haskell.org/upload#candidates).

- Removed the `--root-cmd` parameter of the 'install' command and
  deprecated `cabal install --global`
  ([#3356](https://github.com/haskell/cabal/issues/3356)).

- Removed the top-down solver
  ([#3598](https://github.com/haskell/cabal/issues/3598)).

- Cabal no longer supports using a version bound to disambiguate
  between an internal and external package
  ([#4020](https://github.com/haskell/cabal/issues/4020)). This
  should not affect many people, as this mode of use already did not
  work with the dependency solver.

- Miscellaneous minor and/or internal bug fixes and improvements.

See the full [Cabal
2.0](https://github.com/haskell/cabal/blob/2.0/Cabal/changelog) and
[`cabal-install`
2.0](https://github.com/haskell/cabal/blob/2.0/cabal-install/changelog)
changelogs for the complete list of changes in the 2.0 release.

## Acknowledgements

Thanks to everyone who contributed code and bug reports. Full list of
people who contributed patches to Cabal/`cabal-install` 2.0 is
available
[here](https://gist.github.com/23Skidoo/7109479c43a8de44f8e29fa335e9645c).

## Looking forward

We plan to make a new release of Cabal/`cabal-install` before the end
of the year -- that is, around December 2018. We want to decouple the
Cabal release cycle from the GHC one; that'll allow us to release a
new version of Cabal/`cabal-install` approximately every six months in
the future. Main features that are currently targeted at the 2.2
milestone are:

- Further improvements in `new-build`, incorporating [work done by
  Francesco Gazzetta during HSOC
  2017](https://github.com/haskell/cabal/projects/4). Currently we are
  planning to make `new-build` the default in the 3.0 release
  (tentative release date: Autumn 2018).

- New Parsec-based parser for `.cabal` files, joint work by Oleg
  Grenrus and Duncan Coutts. Faster, less memory hungry, and easier to
  extend.

- [A revamped homepage for
  Cabal](https://github.com/haskell/cabal/issues/4013), [rewritten
  user manual](https://github.com/haskell/cabal/labels/documentation),
  and [automated build bots for binary
  releases](https://github.com/haskell/cabal/issues/4637). Help in
  this area would be appreciated!

We would like to encourage people considering contributing to take a
look at [the bug tracker on
GitHub](https://github.com/haskell/cabal/issues/), take part in
discussions on tickets and pull requests, or submit their own. The bug
tracker is reasonably well maintained and it should be relatively
clear to new contributors what is in need of attention and which tasks
are considered relatively easy. Additionally, [the list of potential
projects from the latest
hackathon](https://github.com/haskell/cabal/wiki/ZuriHac-2017) and the
tickets marked
["easy"](https://github.com/haskell/cabal/issues?q=is%3Aopen+is%3Aissue+label%3A%22meta%3A+easy%22)
and
["newcomer"](https://github.com/haskell/cabal/issues?q=is%3Aopen+is%3Aissue+label%3Anewcomer)
can be used as a source of ideas for what to work on.

For more in-depth discussion there is also the [`cabal-devel` mailing
list](https://mail.haskell.org/mailman/listinfo/cabal-devel) and the
[`#hackage` IRC channel](https://wiki.haskell.org/IRC_channel) on
FreeNode.
