+++
title = "Cabal 3.16 release"
date = 2025-07-24
[taxonomies]
authors = ["Artem Pelenitsyn"]
categories = ["Cabal"]
tags = ["Release"]
+++

The Cabal release team brings you a new release of the cabal-install tool and accompanying libraries, version 3.16.0.0. This release supports the (not yet available) GHC 9.14, including its upcoming alpha. We expect to publish cabal-install 3.16.1.0 soon after GHC 9.14 is out and address any discovered issues and incompatibilities that can be fixed in the respective timeframe.

The binaries for cabal-install are available:

- In the GHCup main channel (we thank the GHCup maintainers for the swift support of our release).
- [On our website](https://downloads.haskell.org/cabal/cabal-install-3.16.0.0/) (the same set of binaries can also be installed via the GHCup vanilla channel); these binaries are signed by "Francesco Ariis <francesco@ariis.it>" (fingerprint: `DAFB 4D8A F684 1435 18D5  051F A9AF 0AAA 6B87 EC51`; the key is hosted on keyserver.ubuntu.com).
- As usual, `cabal update && cabal install cabal-install-3.16.0.0` is an option too.

### What's new

Some of the cool features in the release:
  * The new [`cabal target`](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-target) command;
  * Faster Git clones;
  * `cabal haddock-project` handles user-provided CSS;
  * Multiple `flags` stanzas in `cabal.project` accumulate values instead of using the last one;
  * `cabal gen-bounds` works with multi-package projects (i.e., embraces the `v2`-commands infrastructure);
  * Cabal multi-repl supports reexported-modules with renaming for GHC >= 9.12, and more!

See the [cabal-install release notes on Github](https://github.com/haskell/cabal/blob/master/release-notes/cabal-install-3.16.0.0.md) for the full changelog. Power users may be interested in [Cabal-the-library notes](https://github.com/haskell/cabal/blob/master/release-notes/Cabal-3.16.0.0.md) too.


### 3.16.0.0 Contributors

The credits go to: Andreas Abel, Andreas Klebinger, Artem Pelenitsyn, Benjamin, Benjamin McRae, Berk Özkütük, Bodigrim, brandon s allbery kf8nh, Bryan Richter, Francesco Ariis, Francesco Gazzetta, Gleb Popov, Hécate Moonlight, James Blackburn, Jaro, Jasper Van der Jeugt, Javier Sagredo, Jens Petersen, Kazu Yamamoto, Kevin Quick, Leonid Znamenok, malteneuss, Matthew Pickering, Matt Parsons, Mike Pilgrem, Mikolaj Konarski, Moritz Angermann, noiioiu, parsonsmatt, Peter Becich, Phil de Joux, PHO, Praneya Kumar, Rebecca Turner, Rodrigo Mesquita, Sdywolf, Serge S. Gulin, Sergey Vinokurov, sheaf, Sylvain Henry, Teo Camarasu, theGhostJW, Tom Smeding, Trevis, Troels Henriksen, Yi Fang, Yuto Takano, zlonast, Zubin Duggal.

We thank all the contributors as well as our reviewers, QA testers, devops, and others without whom this release wouldn’t be possible.

### Feedback

Please report any issues you notice with the 3.16.0.0 release on our GitHub: <https://github.com/haskell/cabal/issues>

— Cabal release team (Artem, Brandon, Francesco, Mikołaj)
