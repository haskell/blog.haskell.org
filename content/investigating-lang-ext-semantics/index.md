+++
title = "Analyzing language extension semantics"
date = 2025-11-26
[taxonomies]
authors = ["Jappie Klooster"]
categories = ["Haskell Foundation"]
tags = ["Community", "Stability"]
+++

We analyzed the head.hackage patches to understand 
why code breaks on new GHC releases.
Surprisingly, most breakage wasn’t caused by 
Template Haskell—it came from deeper semantic changes in language extensions.
This post walks through the main categories of breakage, 
why they happened, and what they tell us about long-term stability.
If you care about a smoother upgrade path for Haskell users, 
we invite you to participate in the Stability Working Group.

The [Haskell Foundation Stability Working Group](https://blog.haskell.org/stability-working-group/) 
is interested in understanding
why breakage occurs.
This is an extension of our initial [investigation](https://jappie.me/analyzing-haskell-stability.html).
We've recently done further analysis on [head.hackage](https://ghc.gitlab.haskell.org/head.hackage/),
and learned surprisingly enough that the root cause
of a lot of breakage isn't Template Haskell,
but seems to be from language extension semantics[^meaning].
We're doing this investigation to better understand where the Haskell
Foundation stability working group should focus it's efforts.
About a year ago I started on analyzing the causes
for all `head.hackage` patches, but I got bogged down
by the sheer amount of work.
Trevis suggested focusing on the
most important question,
why do language extension semantics cause so much breakage?
So instead of being bogged down by analyzing all
the patches, I just looked at the 12 patches from language extension
semantics.

[^meaning]: The precise meaning of features enabled by language extensions. I guess parser changes also count.

This gave us the following table:

| Name                                | Cause                                        | Had warnings? |
|-------------------------------------+----------------------------------------------+---------------|
| Cabal-2.4.1.0.patch                 | simplified subsumption                       | no            |
| Cabal-3.0.2.0.patch                 | simplified subsumption                       | no            |
| Cabal-3.2.1.0.patch                 | simplified subsumption                       | no            |
| data-r-tree-0.6.0.patch             | parser change (see 1)                        | no            |
| drinkery-0.4.patch                  | simplified subsumption                       | no            |
| ghc-lib-parser-9.8.1.20231121.patch | rename forall identifiers (2)                | yes           |
| hgeometry-ipe-0.13.patch            | Instances moved due to splice enforcement    | no            |
| singletons-3.0.2.patch              | add TypeAbstractions as a language extension | yes           |
| singletons-base-3.1.1.patch         | add TypeAbstractions as a language extension | yes           |
| vector-space-0.16.patch             | * is type (4)                                | yes           |

`th-compat-0.1.4.patch` was miscounted so I left that out.
Simplified subsumption appears a lot but 3 are for Cabal,
so it's only 2 real occurrences.
We expect that to appear a lot however,
because it was one of *the* motivating changes for a [stability working group](https://blog.haskell.org/stability-working-group/).

## Simplified subsumption
For the blissfully ignorant reader simplified subsumption causes you
to do this under certain existential conditions:
```haskell
--- a/Distribution/Simple/Utils.hs
+++ b/Distribution/Simple/Utils.hs
@@ -1338,7 +1338,7 @@ withTempFileEx opts tmpDir template action =
     (\(name, handle) -> do hClose handle
                            unless (optKeepTempFiles opts) $
                              handleDoesNotExist () . removeFile $ name)
-    (withLexicalCallStack (uncurry action))
+    (withLexicalCallStack (\x -> uncurry action x))
 
```
You've to insert a lambda, which apparently signifies some performance impact.
This went wild with [Yesod stacks](https://www.yesodweb.com/book), 
whose code generation helpfully created 
the database alias in the template:
```haskell
type DB a = forall (m :: Type -> Type).
    (MonadUnliftIO m) => ReaderT SqlBackend m a
```

So anything that now uses a query has to insert those lambdas,
as you can imagine this would be in quite a few places for non-trivial commercial code bases.
Which caused many issues for commercial users.
You can just delete those aliases to solve the problem.
Alternatively you can just enable the language extension: [DeepSubsumption](https://downloads.haskell.org/~ghc/9.12.2/docs/users_guide/exts/rank_polymorphism.html#extension-DeepSubsumption).
Which restores the original behavior.

## Moving of instances due to Template Haskell
This change forces you to put the instances above the splice where
it's being used in the same module.
A dear colleague decided to generate instances in Template Haskell.
That was quite the puzzle!
I asked the GHC devs why they did this,
and it turns out this was a soundness issue in the typechecker.
Here, soundness means the type system can't be tricked into allowing invalid programs.
So the community is better off, despite this causing a fair bit of work.

```haskell
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

```haskell
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

```haskell
 hintExplicitForall :: Located Token -> P ()
 hintExplicitForall tok = do
-    forall   <- getBit ExplicitForallBit
+    forAll   <- getBit ExplicitForallBit
     rulePrag <- getBit InRulePragBit
-    unless (forall || rulePrag) $ addError $ mkPlainErrorMsgEnvelope (getLoc tok) $
+    unless (forAll || rulePrag) $ addError $ mkPlainErrorMsgEnvelope (getLoc tok) $
       (PsErrExplicitForall (isUnicode tok))
```

## TypeAbstractions

From what I understand from the [manual](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_abstractions.html#type-abstractions)
Is that part of the syntax for type abstractions landed in GHC 9.2,
however 9.8 and onwards requires you to enable this language extension.
This appears to because certain new functionality was introduced behind an
old language extension flag, according to [this proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0448-type-variable-scoping.rst#4type-arguments-in-constructor-patterns)

This extension enables you to bind type variables in pattern matches.
I don't know why this happened like this, but it happened in 2023:

```haskell
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

## Star is type
This change was announced via a warning. 
It tells users to write `Type` instead of `*` for kinds representing types.
A kind is essentially the type of a type,
and as a concept is used for type-level programming type safety.

```haskell
-  type Basis v :: *
+  type Basis v :: Type
```


## Conclusion

Often we experience these breakages as annoying and frustrating.
However, if we look deeper, we find that each of them has
a little story
and good reasons for being introduced.
If you find this all as interesting as I do,
please consider joining some of the stability
working group meetings!

