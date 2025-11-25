+++
title = "Google Summer of Code 2025 Wrap-up"
date = 2025-11-24
[taxonomies]
authors = ["Aaron Allen"]
categories = ["Haskell.org"]
tags = ["Summer of Code", "Announcement", "GSoC"]
+++

The Haskell.org committee is pleased to present the results of Haskell's
participation in the Google Summer of Code 2025. This marks our 14th time
taking part in GSoC!

<!-- more -->

<a href="https://summerofcode.withgoogle.com/">
<img src="gsoc-logo.svg" alt="GSoC 2025" width=600px>
</a>

Of the four projects alloted to our organization, three were successfully
completed:

- [Implement Signature Help for Haskell Language Server](#implement-signature-help-for-haskell-language-server)
- [Qualified Imports and Alias Resolution in Liquid Haskell](#qualified-imports-and-alias-resolution-in-liquid-haskell)
- [Documenting and Improving CMM](#documenting-and-improving-cmm)

<br>

**Congratulations to all the contributors and a huge thank you to our wonderful mentors!**

<br>
<br>

---

<br>

## Implement Signature Help for Haskell Language Server

- Contributor: Jian Lin
- Mentors: Michael Peyton Jones and Fendor

This project implemented the "signature help" feature of LSP for the [Haskell Language Server (HLS)](https://haskell-language-server.readthedocs.io/en/stable/).
This allows users to view function signatures and documentation when the cursor
is inside a function application. It can also highlight the part of the
signature related to the parameter at the current cursor location.

*[Read more in the final code submission](https://github.com/haskell/haskell-language-server/pull/4626).*

<br>

---

<br>

## Qualified Imports and Alias Resolution in Liquid Haskell

- Contributor: Xavier Góngora
- Mentor: Facundo Domínguez

This project introduced an enhancement to [Liquid Haskell](https://ucsd-progsys.github.io/liquidhaskell/)
to improve name resolution by enabling qualified imports for both (logic) type
and predicate aliases. By using qualifiers, users are able to disambiguate
between identical aliases which would previously result in confusing error
messages.

*[Read more in the project results write-up](https://gist.github.com/ninioArtillero/9658ebd4b806bb82ab3494457897e23e).*

<br>

---

<br>

## Documenting and Improving Cmm

- Contributor: Diego Antonio Rosario Palomino
- Mentor: Csaba Hruska

The goal of this project was to improve documentation and tooling around Cmm,
which is an internal representation used by GHC as part of the compilation
pipeline. In addition to producing documentation, Diego created a GHC compiler
plugin that can serialize and deserialize Cmm expressions using a JSON representation.

*[Read more in the final code submission](https://github.com/GunpowderGuy/cmm-documentation).*
