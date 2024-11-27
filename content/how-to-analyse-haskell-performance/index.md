+++
title = "A Beginner's Guide To Analysing Haskell Application Performance"
date = 2024-11-27
[taxonomies]
authors = ["Hécate"]
categories = ["Cabal"]
tags = ["Performance", "Profiling"]
+++

The Cabal manual now has a guide on how to analyse the performance of Haskell applications.

<!-- more -->

This guide was contributed very generously by Malte Neuss. It describes the various options you can set in your `cabal.project` file in order to get a time or space profile of your application.

As you read through it, you will be pointed to other resources like the [Profiling section of the GHC Users Guide][GHC] and the [Haskell Optimization Handbook][HsOpt].
It is the recommended entry point for newcomers into the world of Haskell performance analysis.

Read it at: <https://cabal.readthedocs.io/en/latest/how-to-analyze-haskell-code-performance.html>

[GHC]: https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html
[HsOpt]: https://haskell.foundation/hs-opt-handbook.github.io/
