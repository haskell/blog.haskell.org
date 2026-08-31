+++
title = "Google Summer of Code 2026 Wrap-up"
date = 2026-08-30
[taxonomies]
authors = ["Aaron Allen"]
categories = ["Haskell.org"]
tags = ["Summer of Code", "Announcement", "GSoC"]
+++

The Haskell.org committee is pleased to present the results of Haskell's
participation in the Google Summer of Code 2026. This marks our 15th time
taking part in GSoC!

<!-- more -->

<a href="https://summerofcode.withgoogle.com/">
<img src="gsoc-logo.svg" alt="GSoC 2026" width=600px>
</a>

Of the four projects alloted to our organization, three concluded successfully:

- [Case Split Plugin for Haskell Language Server](#case-split-plugin-for-haskell-language-server)
- [Restoring Typeclass Refinement Support in Liquid Haskell](#restoring-typeclass-refinement-support-in-liquid-haskell)
- [Goto Dependency Definition](#goto-dependency-definition)

<br>

**Congratulations to all the contributors and a huge thank you to our wonderful mentors!**

<br>
<br>

---

<br>

## Case Split Plugin for Haskell Language Server

- Contributor: Enrico Maria De Angelis
- Mentors: Fendor, MangoIV, Andreas Klebinger

This project produced a plugin for the [Haskell Language Server (HLS)](https://haskell-language-server.readthedocs.io/en/stable/)
that implements case splitting functionality. This provides users with a code
action that automatically inserts any missing pattern matches in case
statements.

*[Read more in the final code submission](https://gist.github.com/Aster89/69a57c364ec2d0cc89f474cc3d194371).*

<br>

---

<br>

## Restoring Typeclass Refinement Support in Liquid Haskell

- Contributor: Juan Pablo Yamamoto
- Mentor: Facundo Domínguez

This project restores type class elaboration in [Liquid Haskell](https://ucsd-progsys.github.io/liquidhaskell/).
This allows for verification of type class laws and instances, for example
proving that a Monoid instance adheres to the identity and associativity laws.

*[Read more in the project results write-up](https://gist.github.com/jpyamamoto/ac7ab4a0eb7b68ab0a6deb4cb0011e0d).*

<br>

---

<br>

## Goto Dependency Definition

- Contributor: Vidit Odedra
- Mentor: Fendor, Zubin Duggal

The goal is this project is to extend the goto definition functionality in [Haskell Language Server (HLS)](https://haskell-language-server.readthedocs.io/en/stable/)
to work for external dependencies. Although the pull request implementing it is
still under review, the ground work has been laid for this oft-requested feature.

*[Read more in the final code submission](https://gist.github.com/vidit-od/032f03afac51d0e3507b2b9d1bd9a277).*
