+++
title = "Quick tips for fast iteration in Haskell"
description = "Quick tips about tools and techniques for fast iteration when developing Haskell"
date = 2026-07-22
[taxonomies]
authors = ["Tom Ellis", "Laurent P. René de Cotret"]
categories = ["Ecosystem"]
tags = ["Practices", "Tooling"]
+++

# Quick tips for fast iteration in Haskell

## Introduction

The Haskell community has recently been discussing how to achieve fast
iteration times when type checking and compiling Haskell code.  Fast
feedback has always been important in software development, and we're
seeing increased interest in the topic in recent months due to the
rise in prominence of agentic coding.

This article describes two techniques for fast iteration: using `ghci`
to get fast feedback on the result of type checking an compilation,
and speeding up builds with a careful choice of flags to `ghc` and
`cabal`.  These techniques can be run on any existing codebase. They
don't require you to restructure your code in any way, so you can get
an instant improvement to your iteration times. Let's dive in!

## `ghci`-based

GHCi is [GHC's interactive
interpreter](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/ghci.html)
(executable name `ghci`) which comes bundled with every installation
of GHC.  It is a REPL ("[read-eval-print
loop](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)"),
which means you can type expressions into it and it will run them,
printing the result.  `ghci` is a good starting point for fast
feedback because it doesn't have to fully compile code, rather it
generates executable bytecode and skips machine code generation,
making it inherently faster than a normal build.

There are a variety of ways to use `ghci`, from using barebones `ghci`
itself directly to featureful wrappers.

### `ghci` itself

You can invoke `ghci` with a collection of source paths and it will
compile and load those modules, for example:

```
ghci src/Path/Module1.hs src/Path/Module2.hs
```

Or, perhaps more useful, use a `ghci` invocation wrapper that comes
with your build tool, `cabal` or `stack`:

```
stack ghci
cabal repl
```

For a simple `ghci` workflow, make some changes to the files in your
project, then navigate to your `ghci` window and type `:reload` (or
`:r` for short). `ghci` will type check and compiles your changes,
printing any errors or warnings from GHC.  Simple!

You might feel that continually typing `:r` ought to be automated
away. If so then check out the next section on `ghcid`.

### `ghcid`

`ghcid` is a wrapper around `ghci` that automates issuing `:r` when
any file in your project changes, so its workflow is even easier than
that of `ghci`: make some changes to the files in your project and
then merely *look at* your `ghcid` window; `ghcid` will have detected
your changes and the result of type check and compilation will appear
automatically.

#### Automatically running tests

`ghcid` also allows you to get more quick feedback than just the
result of type checking and compilation: you can run tests (or indeed
any code) when your source files change.  There are two ways to do
this:

1. Embed comments with expressions to be evaluated. For example add
   this comment to a source file to evaluate `expr` after loading:

   ```
   -- $> expr
   ```

   (Evaluating embedded comment expressions requires using the
   `--allow-eval` flag.)

2. Pass an expression to the command line option `--test`, to run
   after the code is loaded successfully, for example:

   ```
   ghcid --test myTestFun
   ```

   (By default the test expression will only run if the code is
   warning-free.  To run the test expression even if there are
   warnings, also pass `--warnings`.)

For more information on these features, see the
[Evaluation](https://github.com/ndmitchell/ghcid#evaluation) section
of the `ghcid` README.

#### Installation

`ghcid` is a normal executable package on Hackage, so you can install
it with, for example, `cabal install ghcid`.

### `ghcid-check`

One limitation of `ghcid`'s default behaviour is that it is not
*programatic*.  When you change source files, `ghcid`'s updates appear
in the terminal in which it is already running.  That's no good if you
want to give fast feedback to a coding agent; ideally the coding agent
would run a command line executable to obtain the feedback.

That's fine, because `ghcid` supports that too, with its `--reload` and
`--outputfile` options. But setting that up is a little fiddly, so
here's a simple `ghcid` wrapper `bash` script called `ghcid-check`:

* <https://github.com/tomjaguarpaw/ghcid-check/>

Yes, that's right, there are two layers of wrapping: `ghcid-check` is
a shell script which wraps `ghcid` which is Haskell program which
wraps `ghci`!  `ghcid-check` is very basic and you might want to
customize it to meet your own particular needs.

Beyond `ghcid`, there are another couple of projects based on a
similar idea that bring new features: `ghciwatch` and Tricorder.

### `ghciwatch`

[`ghciwatch`](https://github.com/mercurytechnologies/ghciwatch#ghciwatch)
is an updated take on `ghcid`, from the Haskell developers at
[Mercury](https://mercury.com/blog/topics/engineering-blog).  Compared
to `ghcid` it has more sophisticated checking for new, removed and new
modules, which sometimes trip up `ghcid`, as well as other new
features.

### Tricorder

[Tricorder](https://github.com/tweag/tricorder#tricorder) is from
Tweag and provides even more live status and diagnostic updates than
`ghcid` or `ghciwatch`, including access to documentation and
machine-readable output, and is designed from the ground up to be
usable programatically by coding agents as well as interactively by
humans.

Check out the [announcement
post](https://discourse.haskell.org/t/ann-tricorder-a-new-development-tool-for-haskell-and-llms/14208)
on the Haskell Discourse, which includes a short video of Tricorder
functionality. `tricorder` is a normal executable package on Hackage,
so you can install it with, for example, `cabal install tricorder`

#### References

Lots of people have already written up their experience of workflows
that `ghci`, including for fast iterative development. You might want
to check them out.

* [Using GHCi &mdash; GHC Users Guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/ghci.html)
* [Fast Feedback](https://haskellweekly.news/episode/6.html)
* [Haskell Development Workflows (4 ways)](https://academy.fpblock.com/blog/2018/08/haskell-development-workflows-4-ways/)
* [ghcid for the win!](https://www.parsonsmatt.org/2018/05/19/ghcid_for_the_win.html)
* [ghcid for Web App Development](https://functor.tokyo/blog/2019-04-07-ghcid-for-web-app-dev)
* [Announcing ghciwatch 1.0](https://mercury.com/blog/announcing-ghciwatch)
* [Haskell dev workflow with ghcid and neovim](https://jeancharles.quillet.org/posts/2024-09-04-Haskell-dev-workflow-with-ghcid-and-neovim.html)
* [Multiple Component support for cabal repl](https://www.well-typed.com/blog/2023/03/cabal-multi-unit/)
* [Cheaper: producing a program with less developer time](https://github.com/alexfmpe/semantic-satiation/blob/main/posts/002-cheaper.md)
* [Rapid](https://hackage.haskell.org/package/rapid/docs/Rapid.html)

## `cabal`-based

Let's now see how to speed up development builds using the [`cabal`](https://www.haskell.org/cabal/) build system. `cabal` has existed for a very long time, and you may be surprised by some of the things it offers!

Speeding up development builds using `cabal` involves three broad optimizations:

* Tuning GHC's runtime options;
* Decreasing the amount of work `cabal` and GHC have to do by disabling compile-time optimizations;
* Increasing resource-sharing for more effective build parallelism.

Before we describe the build optimizations above, it is worth understanding how cabal can be configured. Cabal uses _project_ files to bundle configuration options related to multiple packages. The default configuration file, which is picked up automatically by `cabal`, is `cabal.project`. A typical `cabal.project` file for a project with three packages looks like:

```
packages:
    packageA
    packageB
    packageC
```

Recent versions of cabal allow project files to import other projects files. Therefore, without changing our default `cabal.project` file, we'll create a new configuration file specifically for development builds. Let's call it `cabal.fast.project`:

```
import: cabal.project
```

As we go through build optimizations below, we'll append to our `cabal.fast.project`. Then, a fast build will specify to use `cabal.fast.project`:

```shell
$ cabal build all --project-file=cabal.fast.project
```

That way, we have an easy set-up for fast builds. We could have project files for profiling builds, release builds, etc.

Running the above command, we got a baseline compilation time on a private, commercial Haskell codebase of more than 150 000 lines of Haskell code. As we go through build optimizations below, we will report on the speedups achieved from this baseline.

### Tuning GHC's runtime options

GHC is a wonderful piece of technology. In some ways, it might as well be from the future. However, compiling Haskell programs into performant executables is hard work, and GHC is itself a Haskell program. This means we must tune its runtime system to squeeze out more performance.

Take a look at the [GHC 9.14.1 user guide's section on runtime control](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/runtime_control.html). In our experience, however, simply tuning the garbage collection allocation size is a great first step if your computer is somewhat recent. In particular, increasing the allocation size from the default of 4MB to 64MB, 128MB, er even 256MB may dramatically increase compilation throughput!

Thus, we update our `cabal.fast.project` project file to pass the appropriate flag to GHC. We'll use an allocation area of 64MB, but do experiment on your own workloads:

```
import cabal.project

-- Step 1: tuning GHC
program-options:
   ghc-options: +RTS -A64m -RTS
```

Building with the above project file, we achieve a speedup of approximately 10%. Not huge, but not marginal either. Big gains remain!

### Disabling compile-time optimizations

Even if GHC is given the appropriate resources to do its job more effectively, it still does a lot of work by default, since GHC's default optimization level is `-O1`. This means that GHC performs *some* optimizations, which necessarily requires more compilation time.

But this is merely the _default_ behavior, and this article is all about speed. Instead, we can disable optimizations via the `optimization` option in cabal project files:

```
import cabal.project

-- Step 1: tuning GHC
program-options:
   ghc-options: +RTS -A64m -RTS

-- Step 2: disable optimizations
optimization: false
```

Note that this optimization flag only applies to your locally-built packages. Your third-party dependencies will be compiled using the default optimization level, `-O1`. This is generally considered acceptable since your third-party dependencies are rarely built, and it allows you to re-use them for other build profiles.

At this stage, the codebase is building 2x faster!

### Better resource-sharing for build parallelism

We've tuned GHC, and minimized the amount of work it needs to do. What is left is speed up the build across _multiple packages_.

`cabal` has an option for parallel builds called `--jobs`. You can either specify a number of capabilities to dedicate to the build (e.g. `--jobs=2`), or let `cabal` decide based on your hardware (using a bare `--jobs`). Higher is not _always_ better; this depends on your build graph!

In our experience, a number from 4 - 8 is appropriate to start with. We therefore update our `cabal.fast.project` project file:

```
import cabal.project

-- Step 1: tuning GHC
program-options:
   ghc-options: +RTS -A64m -RTS

-- Step 2: disable optimizations
optimization: false

-- Step 3: build parallelism
jobs: 6
```

We can do even better! If you use `cabal` 3.12+ in combination with GHC 9.8+, you can take advantage of a new flag, `--semaphore`, which allows `cabal` to share resources better with GHC. While [Well-Typed has a full breakdown of the why and how](https://well-typed.com/blog/2023/08/reducing-haskell-parallel-build-times/), we summarize the result here. In short, if cabal can take advantage of multiple parallel invocations of GHC, it will do so; however, under the `--semaphore` option, if cabal _cannot_ take advantage of all the cores it was allocated, it can tell GHC to use more cores. This can result in compilation time reductions of up to 30%!

We update our `cabal.fast.project` one final time:


```
import cabal.project

-- Step 1: tuning GHC
program-options:
   ghc-options: +RTS -A64m -RTS

-- Step 2: disable optimizations
optimization: false

-- Step 3: build parallelism
jobs: 6
semaphore: True
```

Finally, our builds will be much faster:

```shell
$ cabal build all --project-file=cabal.fast.project
```

Compared to our baseline, we achieve a cumulative build speedup of 2.6x!

## Going further

If you're eager for even more build time improvements and are willing
to spend time adjusting the structure of your codebase, we can point
you where to look next: watch of Teo Camarasu's talk at the 2026
Haskell Ecosystem Workshop, [Optimising for fast builds with
GHC](https://www.youtube.com/watch?v=nmE6aa_I5TU).

## Conclusion

We hope you enjoyed our brief tour of using `ghci` and `cabal` for
fast iterative development in Haskell. If you have any comments or
questions, or have your own tips and techniques you can share with the
community, then please post on [the Haskell
Discourse](https://discourse.haskell.org/).
