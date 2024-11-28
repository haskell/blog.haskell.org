+++
title = "How to configure a Cabal project for profiling"
date = 2024-11-27
[taxonomies]
authors = ["Hécate"]
categories = ["Cabal"]
tags = ["Performance", "Profiling"]
+++

The [Cabal Manual][Cabal Manual] now has a guide on how to analyse the performance of Haskell applications.

<!-- more -->

This guide was contributed very generously by Malte Neuss. It describes the various options you can set in your `cabal.project` file in order to get a time or space profile of your application.

## Project Configuration Guide

It is the recommended entry point for newcomers into the world of Haskell performance analysis.
As you progress through it, you will be pointed to other resources like the [Profiling section of the GHC Users Guide][GHC] and the [Haskell Optimization Handbook][HsOpt],
which present you with more in-depth techniques and configuration options.

You will moreover be shown how to produce time and space profiles that can be rendered and analysed with third-party tools like [Speedscope](https://speedscope.app).

<img src=./speedscope.png />

You can read it there: <https://cabal.readthedocs.io/en/latest/how-to-analyze-haskell-code-performance.html>.

## Contribute to the documentation

The manual follows the [documentation best practices](@/documentation-best-practices-in-2024/index.md) of the Haddock team, and the Cabal team is eager to receive more contributions to improve the life
of our users. Do not hesitate to open a pull request at <https://github.com/haskell/cabal>.


[Cabal Manual]: https://cabal.readthedocs.io/en/latest/
[GHC]: https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html
[HsOpt]: https://haskell.foundation/hs-opt-handbook.github.io/
