+++
title = "Haskell Language Server 2.14.0.0 release"
date = 2026-04-30
[taxonomies]
authors = ["VeryMilkyJoe"]
categories = ["HLS"]
tags = ["Release"]
+++


The HLS team is excited to announce the `2.14.0.0` Haskell Language Server release!

You can install it today with GHCup: `$ ghcup install hls --set 2.14.0.0`


## Features

This new HLS version adds support for the [`ExplicitLevelImports`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/template_haskell.html#extension-ExplicitLevelImports) language extension, which was added in GHC 9.14.1 in [#4846](https://github.com/haskell/haskell-language-server/pull/4846).

HLS now starts up faster in `sessionLoading: multipleComponents` mode and handles out-of-project files more gracefully thanks to [#4445](https://github.com/haskell/haskell-language-server/pull/4445).

[#4791](https://github.com/haskell/haskell-language-server/pull/4791) adds lots of great quality of life improvements when using Notes such as hover information and autocompletions for note references and declarations.

Additionally, this version introduces smart-case matching when completing modules and file paths in cabal files.

As of [#4621](https://github.com/haskell/haskell-language-server/pull/4621) is now able to recognise file types based on the provided language kind instead of by file ending only.

### Some notable bug fixes

- HLS will now only show linear arrows when the LinearTypes language extension is enabled [#4862](https://github.com/haskell/haskell-language-server/pull/4862).
- HLS will no longer offer 'Add Argument' for identifiers which include qualified names [#4852](https://github.com/haskell/haskell-language-server/pull/4852).
- When expanding nested records, HLS will now expand the record the cursor is pointing to instead of the outermost one [#4813](https://github.com/haskell/haskell-language-server/pull/4813).
- HLS now names the code action for `record syntax conversion` as such instead of it being called  `wildcard expansion` [#4819](https://github.com/haskell/haskell-language-server/pull/4819).

### Supported GHC versions

- 9.14.1
- 9.12.4
- 9.12.2
- 9.10.3
- 9.8.4
- 9.6.7

## Things to look forward to - GSOC 2026

We are very happy to announce that HLS has two exciting upcoming projects our Google Summer of Code students will be working on this summer!

@vidit-od will be working on [goto third-party definition](https://summer.haskell.org/ideas.html#goto-third-party-deps), which will enable developers to view source code of external libraries by clicking goto definition on them. Within external libraries, many of the usual intellisense will keep working such as hover or goto references.

@Aster89 will be working on the much sought after [case split plugin](https://summer.haskell.org/ideas.html#casesplit). This plugin will add refactoring options for `case X of` which supplies holes for all possible constructors of `X`.

## Thank you, Haskell Community

We wish all Haskellers happy hacking while using the latest releases of the Haskell Language Server, and hope to see you in the issue tracker or even in some pull requests!

Finally, a reminder that you can donate to the development of HLS via [OpenCollective](https://opencollective.com/haskell-language-server). The OpenCollective money pays for tedious, but important maintenance work and, sometimes, for getting new features over the finish line.
