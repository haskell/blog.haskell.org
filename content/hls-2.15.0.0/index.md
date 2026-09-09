+++
title = "Haskell Language Server 2.15.0.0 release"
date = 2026-09-06
[taxonomies]
authors = ["VeryMilkyJoe"]
categories = ["HLS"]
tags = ["Release"]
+++


The HLS team is excited to announce the `2.15.0.0` Haskell Language Server release!

You can install it today with GHCup: `$ ghcup install hls --set 2.15.0.0`

## Two successful GSOC 2026 projects!

Before we dive into the technical details and new features we want to announce a fruitful Google Summer of Code for HLS.
We are very happy to announce that our two projects were a success, and one of them is already in this very release!

@Aster89 has implemented the highly anticipated [hls-case-split-plugin](https://github.com/haskell/haskell-language-server/pull/5014) with the support of mentors @MangoIV, @AndreasPK and @fendor. This plugin can be used with HLS 2.15.0.0 but only with GHC 9.14.
It adds refactoring options for `case x of` which supplies holes for all possible constructors of `data X`.

<video controls width=70%>
  <source
    src="case_split.webm"
    type="video/webm"
    alt="A demo with a data X showing that a case statement can be generated for X with all its constructors being represented." />
</video>

@vidit-od finished his project: [goto third-party definition](https://summer.haskell.org/ideas.html#goto-third-party-deps) with mentors @wz1000 and @fendor, which will enable developers to view source code of external libraries by clicking goto definition on them. Within external libraries, many of the usual intellisense will keep working such as hover or goto references.

This feature is not yet merged, but we expect the first part of this project (Type Rules [#5025](https://github.com/haskell/haskell-language-server/pull/5025)) to land before the next HLS release.
So keep your eyes peeled for it in future HLS releases!

## Features

### Export Plugin

This new HLS version introduces the `hls-export-plugin` which will encompass utility for module export lists.
Currently, this plugin supports code actions for adding and removing symbols to and from the import list but more features are planned as you can see in this [summary issue](https://github.com/haskell/haskell-language-server/issues/4948).

<video controls width=70%>
  <source
    src="export_plugin.webm"
    type="video/webm"
    alt="A simple demo where a function 'someFunc' is added to the export list of the module and then deleted." />
</video>

### Improved Component Loading

- In the past, HLS would often get confused when multiple components where open in your editor, which you might recognise as a red squiggle at the top of the file with a confusing error message. This could be due to some components of your project being loaded into scope while others that were needed for HLS to make sense of things were not loaded. We have now enabled multi-component loading by default, which allows HLS to load all needed components as-needed and which will hopefully improve your IDE experience!

- Related to the above issue, HLS now also supports [whole project loading](https://www.well-typed.com/blog/2026/09/whole-project-loading-for-haskell-ide-tooling/) which means, all components of a project will be loaded into HLS' scope upfront instead of as-needed. This can especially be useful when using the new
[Haskell debugger](https://well-typed.github.io/haskell-debugger/) in combination with HLS.

### Performance Improvements

HLS 2.15.0.0 comes with considerable startup performance improvements as well as import resolution correctness [#4600](https://github.com/haskell/haskell-language-server/pull/4600).
The module discovery now avoids repeated on-disk lookups during import resolution speeding up initial startup time and reducing memory usage. One user reports startup time reducing from a few minutes to 3 seconds.
Moreover, we fixed the handling of source modules imports.
Lastly, we now add newly created modules to the most likely GHC session on creation, which should smooth out the IDE behaviour for new modules.
With pending improvements to `hls-cabal-plugin` [#4961](https://github.com/haskell/haskell-language-server/pull/4961), we will soon be capable of adding new modules to `.cabal` files as well.

### Some notable bug fixes

- HLS now also supports formatting with ormolu and fourmolu for projects using GHC 9.14 [#5009](https://github.com/haskell/haskell-language-server/pull/5009).

- Another source of the infamous `Overlapping instances` bug has been squashed [#4936](https://github.com/haskell/haskell-language-server/pull/4936)!

- The `hls-splice-plugin` has been revived, to work with all of our supported GHC versions and has also seen some improvements [#4915](https://github.com/haskell/haskell-language-server/pull/4915).

- When clicking on documentation links on hover, the links will now send you to the correct hackage page [#4746](https://github.com/haskell/haskell-language-server/pull/4746).

<video controls width=70%>
  <source
    src="documentation.webm"
    type="video/webm"
    alt="A simple demo showing that a hackage link leads to the correct page when clicked." />
</video>

## Supported GHC versions

- 9.14.1
- 9.12.4
- 9.12.2
- 9.10.3
- 9.8.4
- 9.6.7

## Thank you, Haskell Community

We wish all Haskellers happy hacking while using the latest release of the Haskell Language Server, and hope to see you in the issue tracker or even in some pull requests!

As MuniHac is drawing near, this could be a great opportunity for your first HLS contribution.
There will be people hacking on HLS there, so simply approach us! We don't bite but we might show you an issue or two you could work on :upside_down_face:

Finally, a big thank you to @crtschin for taking care of this release and a reminder that you can donate to the development of HLS via [OpenCollective](https://opencollective.com/haskell-language-server). The OpenCollective money pays for tedious, but important maintenance work and, sometimes, for getting new features over the finish line.
