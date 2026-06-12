+++
title = "Making GHC upgrades easy"
date = 2026-06-12
[taxonomies]
authors = ["Simon Peyton Jones"]
categories = ["GHC"]
tags = ["hackage", "cabal"]
+++

### **What this post is about**

When a shiny new version of GHC comes out, it should be easy to upgrade.  After all, the new compiler should be more capable than the previous version\!

But in practice that isn't even nearly true.  The upgrade path is so hard that many companies are using versions of GHC from many years back; it's just too much work for them to upgrade.  This is bad in many ways:

* It denies those users access to both bug-fixes and cutting-edge work that are embodied in newer GHCs
* It strongly inhibits those users from spending resources *contributing* to GHC, because their work would land in an old version and would not propagate to the HEAD.
* It places a maintenance burden on GHC's implementers to support more old versions.

The GHC team has been working hard on this issue, and has made lots of progress.  This post summarises what we have done, what remains to be done, and invites your help.

## **1\. Goals**

We have two big goals.  The most important is this one::

**(STABILITY goal) The Big Stability Goal.** Suppose a package P compiles successfully with GHC 10.0.  When GHC 10.2 is released, it should be possible to use GHC 10.2 to compile package P, and all its dependencies, *without modification.*

It is impossible to provide a 100.0% promise of (STABILITY goal): see Section 4.1 below.  But we can get very close.

There is a complementary goal, which relates to the `base` package.  The `base` package provides core functionality to every Haskell program, including all the modules specified in the Haskell Report, especially the `Prelude` module. Almost every Haskell package in existence depends, directly or indirectly, on `base`.  (Hence its name.)

**(BASE goal) The Base Package Goal.** The `base` package should be a package like any other:

* In its own repository
* With its own maintainers
* With its own changelog, independent of GHC's changelog.
* Released on a schedule independent of GHC
* Can be refactored without reference to GHC
* The version of `base` that you use must obviously be compilable with the version of GHC that you are using; but you can use any version of `base` that has that property.

We are now getting very close to achieving these goals.  This post explains why it is harder than it looks, what we have done recently, and what we mean by "very close".

The two goals look independent, but in fact overcoming one set of obstacles will unlock both goals, which is why I am treating them together here.  Section 2 describes the problem.

## **2\. Background: the problem**

In the past, each version of GHC came with a new version of the `base` package.  For example:

* GHC 9.8 came with base-4.19.0
* GHC 9.10 came with base-4.20.2
* GHC 9.12 came with base-4.21.1
* GHC 9.14 came with base-4.22.0

**Moreover, each version of GHC was indissolubly tied to one, and only one, version of \`base\`.**  For example, every program compiled with (say) GHC 9.10, say, must be compiled against `base-4.20.2`.  No other version of `base` will do.

### **2.1 Why tight coupling is a problem**

This tight coupling was convenient for the implementers of early versions of GHC, well before we were thinking about stability, even before Cabal and Hackage even existed. But with the benefit of hindsight, the tight coupling is highly undesirable:

* Suppose package P, is compiled with GHC 9.10, against `base-4.20.2`.
* To compile P with GHC 9.12, I must use `base-4.21.1`, because GHC 9.12 insists on that.
* So at the very least, I must update P's upper bound on `base`.
* The reason that the `base` version number changed was because of changes approved by the Core Libraries Committee, perhaps adding new functions, or removing some.  These changes may force changes in P beyond just the dependency-bound change.
* Even if P is fine with the new `base`, perhaps P depends on Q; so P cannot be compiled until Q is adapted to the new `base`.  But the maintainer of Q might be otherwise engaged so it may take a while to adapt.
* In practice P may depend, directly or indirectly, on dozens of packages, all of which must be adapted (perhaps only by bumping a version bound, but perhaps more) to the new `base`.

This is very, very bad.  It may take months for a wave of version bumps and to sweep through Hackage.   It directly contradicts (STABILITY goal).

### **2.2 The problem of known entities**

But *why* does GHC 9.10, say, insist on `base-4.20.2` and nothing else?

The main reason is that GHC needs to "know about" hundreds of functions, types, and classes defined in `base`.   Example: when generating the code for `deriving(Show)` for a new data type, the generated code needs to refer to auxiliary functions defined in `base`.  By "know about" I mean that GHC needs to know the precise module in which the `Show` class (and many other auxiliary functions) is defined.

There are many, many other examples: desugaring list comprehensions, or arrow notation, or record accesses.  Collectively these functions, types, and classes are called "**known entities**".

This tight coupling between `base` and GHC directly contradicts (BASE goal).

## **3\.  The Glorious Plan, and progress so far**

We have made a lot of progress towards meeting (STABILITY goal) and (BASE goal).  This section lays out the steps we either have taken or propose to take.

This is a multi-year project involving contributions from many people; see Section 6 for a timeline and credits.

### **3.1 Splitting `base` and `ghc-internal`**

The first step was to split the old `base` library into two libraries, `ghc-internal` and `base`:

* `ghc-internal` should really be thought of as part of GHC, a part that just happens to be implemented in library code rather than in the compiler itself.  Its API is not stable, and every version of GHC comes tightly coupled to a new version of `ghc-internal`.  GHC and the `ghc-internal` library should be thought of as a single piece of software, living in a single repository, and with a single version number.

* `base` depends on `ghc-internal`.  Unlike the latter, however, `base` has a very stable API, carefully curated by the Core Libraries Committee.  One way to think of `base` is that it is a shim that hides changes in `ghc-internal` behind a stable `base` API.

This architecture has a major advantages, in principle anyway:

* A new version of GHC always comes with a new version of `ghc-internal`, but it can now come with the same version of `base`.   This addresses (STABILITY goal).   By "the same version" I mean that its API is unchanged; of course its *implementation* may change quite a bit, to accommodate the changes in `ghc-internal`.

  This API stability is reflected in the PVP version `base-A.B.C.D`.  The new version (with unchanged API) should differ only in its final components `C` or `D`. Typically, packages have dependencies like `base-4.22`, allowing upgrades to `4.22.0.1` or even `4.22.1` *without modification;* and that compile-without-modification is (STABILITY goal).

  (Side note: the [Haskell PVP](https://pvp.haskell.org/) is rather silent about version bumps when fixing bugs or doing internal refactoring.)

* The `base` library can be independently maintained, and could be released independently of GHC, like any other package.  This addresses (BASE goal).

  For example if the Core Libraries Committee decides to add a function `wombat` to `base`, the maintainer can add the function and make a new release of `base`, just like any other package.

  Moreover, like any other package, the maintainers of `base` may be able to make it compilable with multiple versions of GHC, so that the user can upgrade or choose its version regardless of compiler version.     That is, `base` becomes "re-installable".


This separation was achieved in GHC 9.14.  Doing it was trickier than it seemed; first `ghc-internal` and `base` had to be separated, and then `base` had to be made reinstallable.   (Timeline in Section 6 below.)

Even after this all "known entities" (see Section 2\) had to be defined in `ghc-internal`.   In practice that pins a lot of library code in `ghc-internal` and means that `base` is largely just a shim.  (Still useful\!  But without much functionality of its own.).  See Section 3.3 for the next step.

### **3.2  Template Haskell**

Template Haskell allows you to create a source Haskell AST (Abstract Syntax Tree), and then to pattern match on it.  Since every release of GHC has changes to its Haskell AST, any package that does pattern-matching on a Template Haskell AST cannot possibly compile with a new version of GHC.

This directly threatens (STABILITY goal).   More concretely:

* Modules that use Template Haskell depend on a library `template-haskell`.
* The API of `template-haskell` includes a data type for the Haskell AST, which necessarily changes in every GHC release, forcing a major bump in the version of `template-haskell`.
* Many many libraries depend transitively on some library L  that uses Template Haskell.
* So those libraries cannot work with a new version of GHC until the author of L has at least updated the version bound for their `template-haskell` dependency.

This results in a lot of breakage when a new version of GHC comes out.  And most of that breakage is unnecessary\!  Most use only quotations and splices (which are perfectly portable), rather than using the AST directly (which is not).    For more detail on different classes of Template Haskell usage, with different stability properties, see [Teo's blog post](https://informal.codes/posts/stabilising-th/), or [their talk at the 2025 Haskell Ecosystem Workshop](https://informal.codes/talks/hew25/).

Thus motivated, Teo has been busy splitting up the previously-monolithic `template-haskell` package, whose API necessarily changed with each version of GHC, into several packages:

* `template-haskell-lift`
* `template-haskell-quasiquoter`

These new packages have very stable APIs.   There is still a package `template-haskell` that exposes the TH AST data type, and that is necessarily unstable.  But very few clients need to depend on it.  (Side note: you might think it should be called  `template-haskell-internal` and you'd be right; but that's a disruptive change and we won't make it yet.)

These new packages already exist in GHC 9.14, and all the boot libraries now depend on them rather than on `template-haskell`.  (Side note: while these changes have landed in the relevant boot-library repos, they have not all been released at the time of writing.)  By GHC 10.2, the `ghc` package itself will no longer transitively depend on `template-haskell`.

To get the benefits, however, library authors who use Template Haskell must update their packages to depend on stable APIs, namely:

* TH splices and quotations
* The stable libraries `template-haskell-lift` and `template-haskell-quasiquoter`

and remove dependencies on `template-haskell`.

More background in :

* [Stabilising Template Haskell](https://informal.codes/posts/stabilising-th) (blog post 2024).
* [Announcing template-haskell-lift and template-haskell-quasiquoter](https://informal.codes/posts/ann-th-lift-and-quasi/) (blog post 2025\)
* [Template Haskell: a base study in (in)-stability](https://informal.codes/talks/hew25/), Haskell Ecosystem Workshop 2025
* [The abstract Q proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0700-abstract-q.rst) (will land in GHC 10.2, not user facing but opens up new opportunities)

### **3.3 Known entities**

Up to and including GHC 10.0, for every "known entity" E (function, type, or class; see Section 2.2), GHC insists that

* E is defined in `ghc-internal`
* GHC knows, baked into GHC's source code, the module in which E is defined.

Our recent work (which will be in 10.2) means that, for the first time, these "known entities" no longer need to be defined in a known module.  Instead, E can be defined in any module of `base` or of `ghc-internal`.

Moreover, changes in `base` can move E from one module to another, without changing GHC.  For example, using GHC 10.2 you can compile a module M against two versions of `base` that define E in different modules, without changing GHC itself.  So how does GHC find E?  It looks in the export list of a `base` module called `GHC.Essentials`.   The author of `base` must simply ensure that `GHC.Essentials` *exports* all the known entities; but they do not need to be *defined* in `GHC.Essentials`.

This architecture properly supports (BASE goal) because it allows the maintainer of `base` to refactor code freely, *including* moving known entities from one module to another.

Better still, it *also* allows us to move code from `ghc-internal` to `base`.   That is good because bug fixes to code in `ghc-internal` can only come coupled to a new GHC release, whereas bug fixes to code in `base` can be made and released independently of the compiler.  The more code we can remove from `ghc-internal` and put in `base`, the better\!

### **3.4 Cleaning up the `base` API.**

For historical reasons, `base` exports quite a few functions that should properly be considered internal to GHC \-- they have been "grandfathered" into `base`.   For example `base:GHC.Base` exports `mapFB`, a function that is used only inside GHC's implementation of fusion for lists. Another more foundational example: `base:GHC.IO` exposes the *representation* of `IO`, not just the API of `IO`.   [Here is a list of all base modules with an indication of their stability and status](https://docs.google.com/spreadsheets/d/1WmyYLbJIMk9Q-vK4No5qvKIIdIZwhhFFlw6iVWd1xNQ/edit?gid=1315971213#gid=1315971213).

Even though those exports may be historical and somewhat accidental, packages may nevertheless depend on them.   That is bad all round:

* Those package authors are exposed to sometimes-unavoidable changes in GHC's internals.
* It inhibits GHC's implementers from changing what should properly be internal to GHC, even when it would make sense to do so.
* It forces a major-version bump to `base` even when GHC makes a change to an exotic corner of a GHC-internal function.  That major version bump forces a wave of changes through the ecosystem even though 99.9% of them neither know nor care about this GHC-internal function.
* If `base` exposes GHC-specific functions that makes it hard or impossible for other compilers (e.g. [MicroHs](https://hackage.haskell.org/package/MicroHs)) to support `base`.

The obvious question is: why not just remove these accidental exports from the API of `base`?  Two reasons:

* They may expose genuinely useful facilities.  **The right thing to do is to carefully design a stable API, and commit to that in the future**.  GHC's internals can change, provided they can still support this API.

* Regardless of design or utility, some existing packages may depend on these functions. What would be the impact of removing function `foo`?  The CLC responds to proposals and will ask for such an impact analysis if someone makes such a proposal.

So there is a task here, on which we have not made much progress.  We need to

* Identify which functions, or groups of functions should be removed.
* Find out what the impact would be, by looking at all the packages in Hackage.
* Decide whether to remove them, keep them, or design a new, stable API to provide that functionality.
* Make a CLC proposal or proposals that embodies the changes.

A topical example is [the discussion in CLC ticket \#405](https://github.com/haskell/core-libraries-committee/issues/405).  One comment explains that `GHC.IO.Encoding` and `GHC.IO.Exception` both say "The API of this module is unstable and not meant to be consumed by the general public", and yet these modules are imported by 132 and 287 packages respectively.  So it's not easy just to remove them\!

### **3.5 Decoupling \`base\` from GHC**

Ultimately we can move to the situation where *`base` is a separate package like any other, with its own maintainer, repository, and release cycle*.   In particular, the release cycle of `base` no longer needs to be coupled to that of GHC.

Moreover, because known entities can now be defined in `base` (not just in `ghc-internal`) lots of code can move from `ghc-internal` into `base`, so that `base` is no longer just a shim.  This process has started but there is plenty more to do.

## **4\. What is now possible**

Because \`base\` is now reinstallable, it becomes possible to do the following.

* GHC 9.14 came with `base-4.22.0.0`
* GHC 10.0 comes with `base-4.23.0.0`.  There is a major version bump from 4.22 to 4.23 because the CLC has agreed to changes in the `base` API in the time between GHC 9.14 and GHC 10.0.
* After releasing GHC 10.0, it would be possible to also release `base-4.22.0.1`, whose API is identical to `base-4.22.0.0`
* This new `base-4.22.0.1` uses CPP magic to allow it to be compiled with *either* GHC 9.14 *or* GHC 10.0.

Now any package P that compiles with GHC 9.14 against `base-4.22.0.0` can *also* be compiled with the shiny new GHC 10.0, against `base-4.22.0.1`.  That is: we can meet (STABILITY goal).  Victory\!  (NB: provided the user bounds are `base-4.22.0.*`, P can also be compiled by ghc 9.14 against the self-same `base-4.22.0.1`.)

Moreover, this same pattern can be repeated when GHC 10.2 is released.  Then, again in principle, one could release `base-4.22.0.2` which can be compiled with GHC 9.14, or 10.0, or 10.2.  Then package P can be compiled with GHC 10.2.

### **4.1 Caveats**

There are caveats, of course

* If P had a hard upper bound of `base-4.22.0.0`, and refused to compile with even a minor version bump to `base-4.22.0.1` then P won't compile.  Solution: packages should allow a version bump in at least the `d` field of `base-a.b.c.d`.

* For historical reasons, the `base` API includes many functions that should properly be considered as GHC internal functions that have no business being in the `base` API.  The stability of these functions are more vulnerable to changes in GHC internals, of course; in consultation with the CLC we should try to remove them from the `base` API.

* Packages that do pattern matching on Template Haskell syntax trees are always going to need updating when moving to a new GHC (see Section 3.2).  Happily, such packages are only a tiny proportion of the packages that use TH.

* A new release of GHC could outright change some behaviour.  GHC's developers make strenuous efforts to avoid changes in existing behaviour, but it can happen.   For example, suppose a bug meant that 1+3 evaluated to 5, and we fix the bug; a package relying on that behaviour might break, and everyone would probably agree that's fine.

  Sometimes there are compelling reasons for a behaviour change, but we try hard to offer a deprecation period, so that package authors have a release or two to adapt.

* GHC 10.0 could make changes in `ghc-internal` that make it impossible to implement the same API as `base-4.22.0.0`.   As an extreme case, suppose GHC 10.0 changed the definition of the class `Num`.  We couldn't shim over that\!   Happily, fundamental changes like this are now vanishingly rare.

  A less extreme, and hence more troubling case is where `ghc-internal` changes the behaviour of a method in some class *instance*.  (Here's [an example that happened between 9.14 and 10.0](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/14413/diffs).)  A shortcoming of instances is that you can't shim over them.

* `P` might depend transitively on the `ghc` package, the so-called "GHC API" that lets you use GHC itself as a library.  Unfortunately, no one has written down a stable API for `ghc` so in practice it exposes practically of GHC, and its version necessarily increases with each release.

* Package collections like Stackage can only have a single version of `base`; but each Stackage release will be free to choose which version of `base` to incorporate. (There are some tricky corners to do with the fact that Stackage includes the `ghc` package itself, which may depend on a particular `base` version.)

But the biggest caveat is that all this takes work.

* Someone has to make `base-4.22.0.1`
* They need to check that it is compilable with both GHC 9.14 and 10.0.
* They need to test that it has the same API as `base-4.22.0.0`

Some of this work will be simple and routine.   But it will take some care.  And the more versions of GHC are supposed to compile the same `base` version, the more care this will take.

The good news is that

* This work can be done entirely decoupled from the GHC release cycle.   `base-4.22.0.1` can, for example, be released after GHC 10.0.
* It does not require any detailed knowledge of GHC.

## **5\.  How you can help**

GHC is an open source project.  It relies utterly on the contributions of volunteers.  There are a few people whose day job involves working on GHC, but most of them are working on specific projects for specific customers.  Cycles are scarce.

We want to deliver on our goals: (STABILITY goal) and (BASE goal).  The groundwork, which can only really be done by people deeply familiar with GHC, has now been completed.  We are now in a phase where you can help; and indeed further progress relies on your help.  Specifically

* **We need someone (or a small group) to become an active maintainer of `base`; and in particular to implement the `base` releases that can be compiled with multiple versions of GHC (see Section 4).**
  * An example of this work in practice: [\!16070.](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/16070/commits)
  * A very helpful document is the [changelog for base](https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/base/changelog.md).

* **We need people to make CLC proposals to remove GHC-internal APIs from `base` (Section 3.4).   Doing so will require some kind of impact analysis; and perhaps the design of stable APIs to replace them.**

None of this is rocket science. It does not require intimate knowledge of GHC.  But it does require care, judgement, discussion, and negotiation.

You would not be on your own.  There is a small community of people to consult and discuss with; the [Haskell Foundation Stability Working Group](https://github.com/haskellfoundation/stability) is very supportive.  You would be very welcome at the weekly GHC Team video call.

If you are willing to help, please write to Rodrigo: rodrigo@well-typed.com.

## **6\. Credits**

I hope it has become clear that although the goals are very simple and clear, the path to achieving them has been far from simple.  It has taken several years, involved interactions across the ecosystem (not just GHC internals), needed lots of discussion and communication, and is still on-going.

I am hugely grateful to those who have made it all possible.  Specifically:

* **2022 onwards**.  **John Ericson** was a tireless advocate for the "split-base" plan, [articulated in HF proposal \#47](https://github.com/Ericson2314/tech-proposals/blob/standard-library-reform/proposals/accepted/047-standard-library-reform.rst).
* **2023 onwards**.   **John Ericson, Ben Gamari, Adam Gundry, Andrew Lelechenko, Julian Ospald,** and myself co-authored the subsequent [HF proposal \#51](https://github.com/haskellfoundation/tech-proposals/blob/main/proposals/accepted/051-ghc-base-libraries.rst) which described the `base`/ `ghc-internal` split.
* **2023\.**  Executing on that proposal, by splitting `base` into `base` and `ghc-internal` was done mainly by **Ben Gamari**, funded by Well-Typed  (Section 3.1).   The split first appeared in GHC 9.10.
* **2024-25.** The next step was to make `base` and `template-haskell` into fully-reinstallable packages (Section 3.1).  This involved changes to both GHC and [Cabal](https://www.google.com/url?q=https://github.com/haskell/cabal/pull/10982&sa=D&source=docs&ust=1781006171722758&usg=AOvVaw3n-ePBoBHfwf7CmjVpolJQ), and was a collaboration between **Matthew Pickering** (funded by Well-Typed) and **Teo Camarasu**.  It happened in GHC 9.14.
* **2026**.   Refactoring the `template-haskell` library into libraries with much more stable interfaces (Section 3.2) has been almost entirely driven by **Teo Camarasu** with support from CircuitHub.
* **2026**.  Making it possible for known entities to be defined in any module (Section 3.3) was a project initiated by **Matthew Pickering,** and initially executed by me.  Then **Rodrigo Mesquita and Wolfgang Jeltsch** took it over and pushed it to completion, funded by Well-Typed.
* **2026**.  **Wolfgang Jeltsch** has done quite a bit of work, in negotiation with the CLC and supported by Well-Typed, to clean up the interface of `base` (Section 3.4).  This is very much on-going work, and needs help (Section 5).

You can find some more background [here](https://github.com/well-typed/reinstallable-base) and [here](https://discourse.haskell.org/t/what-are-the-next-steps-for-reinstallable-base/13319).

Although many people have contributed, often supported by their employers, the above list makes it clear that the direct support of Well-Typed has been particularly critical to success.  Thank you Well-Typed\!

