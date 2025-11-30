+++
title = "Analyzing language extension semantics"
date = 2025-11-26
[taxonomies]
authors = ["Jappie Klooster"]
categories = ["Haskell Foundation"]
tags = ["Community", "Stability"]
+++

Hi I'm [Jappie](https://jappie.me) and I volunteer for the [Haskell Foundation Stability Working Group](https://blog.haskell.org/stability-working-group/).
Recently we analyzed the [head.hackage](https://gitlab.haskell.org/ghc/head.hackage) patches to understand 
why code breaks on new GHC releases.
"head.hackage" is a repository of patches for Hackage. 
GHC engineers use these to test out new GHC builds on a wide range of 
Hackage packages without having to upstream[^upstream] a patch, which can take time. 
Instead, they can put the patch in "head.hackage" 
and immediately test it on a wide range of packages.
Surprisingly, most breakage wasn’t caused by 
[Template Haskell](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/template_haskell.html),
it came from deeper semantic changes in language extensions.
The meaning of (some) language extensions changed between GHC releases.
This post walks through the main categories of breakage, 
why they happened, and what they tell us about long-term stability.
If you care about a smoother upgrade path for Haskell users, 
we invite you to participate in the [Haskell Foundation Stability Working Group](https://blog.haskell.org/stability-working-group/).


Extending our initial [investigation](https://jappie.me/analyzing-haskell-stability.html),
We're also interested in understanding *why* breakage occurs.
So we've recently done further analysis on [head.hackage](https://ghc.gitlab.haskell.org/head.hackage/),
and learned surprisingly enough that the root cause
of a lot of breakage isn't Template Haskell,
but seems to be from language extension semantics[^meaning].
We're doing this investigation to understand better where efforts
should be focused in improving stability.

This gave us the following table:

| Name                                | Cause                                        | Had warnings? |
|-------------------------------------|----------------------------------------------|---------------|
| Cabal-2.4.1.0.patch                 | simplified subsumption                       | no            |
| Cabal-3.0.2.0.patch                 | simplified subsumption                       | no            |
| Cabal-3.2.1.0.patch                 | simplified subsumption                       | no            |
| data-r-tree-0.6.0.patch             | parser change (see 1)                        | no            |
| drinkery-0.4.patch                  | simplified subsumption                       | no            |
| ghc-lib-parser-9.8.1.20231121.patch | rename forall identifiers (2)                | yes           |
| hgeometry-ipe-0.13.patch            | Instances moved due to splice enforcement    | no            |
| singletons-3.0.2.patch              | add TypeAbstractions as a language extension | yes           |
| singletons-base-3.1.1.patch         | add TypeAbstractions as a language extension | yes           |
| vector-space-0.16.patch             | Star is type (4)                             | yes           |

`th-compat-0.1.4.patch` was miscounted so I left that out.
Simplified subsumption appears a lot but 3 are for Cabal,
so it's only 2 real occurrences.
I'd expect that to appear a lot however,
because it was one of *the* motivating changes for a [stability working group](https://blog.haskell.org/stability-working-group/).

## Simplified subsumption
For the blissfully ignorant reader simplified subsumption causes you
to do this under certain existential conditions:

```diff
--- a/Distribution/Simple/Utils.hs
+++ b/Distribution/Simple/Utils.hs
@@ -1338,7 +1338,7 @@ withTempFileEx opts tmpDir template action =
     (\(name, handle) -> do hClose handle
                            unless (optKeepTempFiles opts) $
                              handleDoesNotExist () . removeFile $ name)
-    (withLexicalCallStack (uncurry action))
+    (withLexicalCallStack (\x -> uncurry action x))
```

You have to insert a lambda, which apparently has some performance impact.
This had a big impact on [Yesod stacks](https://www.yesodweb.com/book), 
whose code generation helpfully created 
the database alias in the template:
```haskell
type DB a = forall (m :: Type -> Type).
    (MonadUnliftIO m) => ReaderT SqlBackend m a
```

Normally this is quite convenient, 
however with the simplified subsumption change,
any code that interacts with the database now has to insert those lambdas.
As you can imagine this would in many places for a commercial code base.
Causing a lot of compile errors for industrial users.
Instead of inserting lambdas, you can also delete those existential aliases to solve the problem.
Or you can enable the language extension: [DeepSubsumption](https://downloads.haskell.org/~ghc/9.12.2/docs/users_guide/exts/rank_polymorphism.html#extension-DeepSubsumption).
Which restores the original behavior.

## Moving of instances due to Template Haskell
This change forces you to put the instances above the splice where
they are being used in the same module.
A dear colleague decided to generate instances in Template Haskell.
That was quite the puzzle!
I asked the GHC devs why they did this,
and it turns out this was a soundness issue in the typechecker.
Here, soundness means the type system can't be tricked into allowing invalid programs.
So the community is better off, despite this causing a fair bit of work.

```diff
--- a/src/Ipe/Content.hs
+++ b/src/Ipe/Content.hs
@@ -288,6 +288,14 @@ 
 
+instance Fractional r => IsTransformable (IpeObject r) where
+  transformBy t (IpeGroup i)     = IpeGroup     $ i&core %~ transformBy t
+ ...
 makePrisms ''IpeObject
 
@@ -303,14 +311,6 @@ 
 
-instance Fractional r => IsTransformable (IpeObject r) where
-  transformBy t (IpeGroup i)     = IpeGroup     $ i&core %~ transformBy t
- ...
```

## (1) Parser change

The parser is the component of the compiler that transforms text
into a memory structure the compiler can work with. 
This structure is called an abstract syntax tree.

```diff
-      Node4 {getMBB :: {-# UNPACK #-} ! MBB, getC1 :: ! (RTree a), getC2 :: ! (RTree a), getC3 :: ! (RTree a), getC4 :: ! (RTree a) }
-    | Node3 {getMBB :: {-# UNPACK #-} ! MBB, getC1 :: ! (RTree a), getC2 :: ! (RTree a), getC3 :: ! (RTree a) }
-    | Node2 {getMBB :: {-# UNPACK #-} ! MBB, getC1 :: ! (RTree a), getC2 :: ! (RTree a) }
+      Node4 {getMBB :: {-# UNPACK #-} !MBB, getC1 :: !(RTree a), getC2 :: !(RTree a), getC3 :: !(RTree a), getC4 :: !(RTree a) }
+    | Node3 {getMBB :: {-# UNPACK #-} !MBB, getC1 :: !(RTree a), getC2 :: !(RTree a), getC3 :: !(RTree a) }
+    | Node2 {getMBB :: {-# UNPACK #-} !MBB, getC1 :: !(RTree a), getC2 :: !(RTree a) }
     | Node  {getMBB ::                  MBB, getChildren' :: [RTree a] }
-    | Leaf  {getMBB :: {-# UNPACK #-} ! MBB, getElem :: a}
+    | Leaf  {getMBB :: {-# UNPACK #-} !MBB, getElem :: a}
     | Empty

```

This is a change from all the way back in 2020, where the *core* language changed by
disallowing `!` before parens.
Here the bang `!` indicates strict fields.
Technically this doesn't fit into the category
because the core language isn't a language extension.
But semantics did change! 
Actually I don't think we expected to find something like this at all.
I'm not sure how relevant this is to discuss further because it appears
quite rare for someone to do this.
You can enable [StrictData](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/strict.html#extension-StrictData)
in your Cabal file and delete all those bangs!

## (2) Rename forall identifiers
This changes the forall identifier into a keyword at term level.
It already was at the type level.
The issue is discussed [here](https://gitlab.haskell.org/ghc/ghc/-/issues/23719)

```diff
 hintExplicitForall :: Located Token -> P ()
 hintExplicitForall tok = do
-    forall   <- getBit ExplicitForallBit
+    forAll   <- getBit ExplicitForallBit
     rulePrag <- getBit InRulePragBit
-    unless (forall || rulePrag) $ addError $ mkPlainErrorMsgEnvelope (getLoc tok) $
+    unless (forAll || rulePrag) $ addError $ mkPlainErrorMsgEnvelope (getLoc tok) $
       (PsErrExplicitForall (isUnicode tok))
```

## (3) TypeAbstractions

From what I understand from the [manual](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_abstractions.html#type-abstractions)
is that part of the syntax for type abstractions landed in GHC 9.2,
however 9.8 and onwards requires you to enable this language extension.
This appeared because certain new functionality was introduced behind an
old language extension flag, according to [this proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0448-type-variable-scoping.rst#4type-arguments-in-constructor-patterns). It says we don't want to introduce new functionality behind established extensions,
so that's why we require TypeAbstractions now, 
where previously ScopedTypeVariables and TypeApplications were enough.

This extension enables you to bind type variables in pattern matches.
I don't know why this happened like this, but it happened in 2023:

```diff
+-- Invisible type binders in type declarations, such as
+--
+--   type family Sing @k
+--
+-- require the TypeAbstractions extension.
+#if __GLASGOW_HASKELL__ >= 909
+{-# LANGUAGE TypeAbstractions #-}
+#endif
+
```

## (4) Star is type
This change was announced via a warning. 
It tells users to write `Type` instead of `*` for kinds representing types.
A kind is essentially the type of a type,
and as a concept is used for type-level programming type safety.

```diff
-  type Basis v :: *
+  type Basis v :: Type
```

## Conclusion

Often these breakages are annoying and frustrating.
But if we look deeper, we find that each of them has
a little story
and good reasons for being introduced.
If you find this all as interesting as I do,
please consider joining some of the stability
working group meetings!

[^upstream]: Upstreaming is the process of sending a patch to the “maintainers” of an open-source project. The maintainers will then make the patch ‘official’ by merging it. In principle, the process is simple, but in practice, the burden of proof (especially for larger projects) is on the person who submitted the patch. They have to convince the maintainers that the patch is useful, which takes time in the form of communication

[^meaning]: The precise meaning of features enabled by language extensions. I guess parser changes also count.
