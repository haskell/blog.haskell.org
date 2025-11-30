+++
title = "GHC 9.14.1-alpha1 is now available"
date = 2025-08-19
[taxonomies]
authors = ["Ben Gamari"]
categories = ["GHC"]
tags = ["Release"]
+++

The GHC developers are very pleased to announce the availability of the
first alpha prerelease of GHC 9.14.1. Binary distributions, source
distributions, and documentation are available at [downloads.haskell.org][].

GHC 9.14 will bring a number of new features and improvements, including:

* Significant improvements in specialisation:

   * The `SPECIALISE` pragma now allows use of type application syntax

   * The `SPECIALISE` pragma can be used to specialise for expression arguments
     as well as type arguments.

   * Specialisation is now considerably more reliable in the presence of
     `newtype`s

   * the specialiser is now able to produce specialisations with
     polymorphic typeclass constraints, considerably broadening its scope.

* Significant improvements in the GHCi debugger

* Record fields can be defined to be non-linear when `LinearTypes` is enabled.

* `RequiredTypeArgments` can now be used in more contexts

* SSE/AVX support in the x86 native code generator backend 

* A major update of the Windows toolchain

* ... and many more

A full accounting of changes can be found in the [release notes][]. Given the
many specialisation improvements and their potential for regression, we would
very much appreciate testing and performance characterisation on downstream
workloads.

Due to unexpected complications, this initial prerelease comes a bit later than
expected. Consequently, we expect to have three condensed alphas prior to the
release candidate. We expect the next alpha
will come the week of 9 Sept. 2025, while the third will come 23 Sept. 2025,
with the release candidate coming 7 Oct. 2025.

We would like to thank the Zw3rk stake pool,
Well-Typed, Mercury, Channable, Tweag I/O, Serokell, SimSpace, the Haskell
Foundation, and other anonymous contributors whose on-going financial
and in-kind support has facilitated GHC maintenance and release
management over the years. Finally, this release would not have been
possible without the hundreds of open-source contributors whose work
comprise this release.

As always, do give this release a try and open a [ticket][] if you see
anything amiss.


[downloads.haskell.org]: https://downloads.haskell.org/ghc/9.14.1-alpha1
[release notes]: https://downloads.haskell.org/ghc/9.14.1-alpha1/docs/users_guide/9.14.1-notes.html
[ticket]: https://gitlab.haskell.org/ghc/homepage/-/issues/new

