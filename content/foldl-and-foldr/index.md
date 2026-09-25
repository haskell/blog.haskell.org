+++
title = "Differences between `foldl` and `foldr`"
description = "`foldl` and `foldr` can be confusing, so let's work out what's up"
date = 2026-09-25
[taxonomies]
authors = ["Alexis King"]
categories = ["Deep Dives"]
tags = []
+++

_Editor's note: This article is a reproduction of a seminal explanation of the differences between foldr and foldl, both strict and lazy versions. As it has been used consistently to teach newcomers since its first appearance on [hasura/graphql-engine!2933](https://github.com/hasura/graphql-engine/pull/2933#discussion_r328821960) on the 26th September 2019, we believe that it ought to be preserved in the blog. Our many thanks to Alexis King for giving her permission to do so_

To start, you have to understand that `foldl` and `foldr` are *not* folds “from the left” and “from the right.” Both `foldl` and `foldr` traverse the structure in the same order, which in the case of lists means left to right. The difference is the fold’s *associativity*.

### `foldl` vs `foldr` illustrated

The best way to think about this is with an illustration. When you write

<pre><code>foldl (⨂) <i>v</i> [<i>e</i><sub>0</sub>, <i>e</i><sub>1</sub>, <i>e</i><sub>2</sub>, ..., <i>e</i><sub><i>n</i>−1</sub>, <i>e</i><sub><i>n</i></sub>]</code></pre>

you’re performing the following computation:

<pre><code>( ... (((<i>v</i> ⨂ <i>e</i><sub>0</sub>) ⨂ <i>e</i><sub>1</sub>) ⨂ <i>e</i><sub>2</sub>) ⨂ ... ⨂ <i>e</i><sub><i>n</i>−1</sub>) ⨂ <i>e</i><sub><i>n</i></sub></code></pre>

In contrast, when you write

<pre><code>foldr (⨂) <i>v</i> [<i>e</i><sub>0</sub>, <i>e</i><sub>1</sub>, <i>e</i><sub>2</sub>, ..., <i>e</i><sub><i>n</i>−1</sub>, <i>e</i><sub><i>n</i></sub>]</code></pre>

you’re performing this computation:

<pre><code><i>e</i><sub>0</sub> ⨂ (<i>e</i><sub>1</sub> ⨂ (<i>e</i><sub>2</sub> ⨂ ... ⨂ (<i>e</i><sub><i>n</i>−1</sub> ⨂ (<i>e</i><sub><i>n</i></sub> ⨂ <i>v</i>)) ... ))</code></pre>

See the difference? In both expressions, the elements of the list appear in the expression in the same order—from left to right—but the *grouping* changes. With `foldl`, the applications of `(⨂)` are left-associated, while with `foldr`, they’re right-associated.

### `foldl` vs. `foldr`, strictly

The question is: how does this difference actually impact the behavior of a program? Well, let’s start by first thinking about what the difference would be in a strict language. In a strict language, evaluation order always proceeds from the “inside out,” starting with the most deeply nested expression.

Let’s think about that in the context of `foldl` first. Let’s say we wrote this expression:

```haskell
foldl (+) 0 [1, 2, 3, 4]
```

By the above illustration, we know that expression is equivalent to this one:

```haskell
(((0 + 1) + 2) + 3) + 4
```

Reducing from the inside out, we get the following reduction sequence:

```haskell
  foldl (+) 0 [1, 2, 3, 4]
= (((0 + 1) + 2) + 3) + 4
= (( 1      + 2) + 3) + 4
= (  3           + 3) + 4
=    6                + 4
=   10
```

In contrast, if we had used `foldr`, we’d get the same result (since `(+)` is an associative, commutative operation), but with a slightly different reduction sequence:

```haskell
  foldr (+) 0 [1, 2, 3, 4]
= 1 + (2 + (3 + (4 + 0)))
= 1 + (2 + (3 +      4 ))
= 1 + (2 +           7  )
= 1 +                9
= 10
```

What’s the practical difference between these two things? Well, note the following detail: with `foldl`, to start reducing, we only need the *first* element of the list, but with `foldr`, we have to start from the *end* of the list and reduce “backwards.”<sup>1</sup> Practically, this means `foldl` can be tail-recursive, reducing as it traverses the list in constant space, while `foldr` cannot be: to reduce a list of length *`n`* with `foldr`, you need to create *`n`* stack frames before any reduction can start.

<sub><sup>1</sup> This is why `foldr` is sometimes described as “folding from the right”, even though it traverses the list from left to right. As we’ll see, however, this doesn’t actually hold in lazy languages!</sub>

### `foldl`, lazily

But what about in *lazy* languages, like Haskell? In a lazy language, evaluation order doesn’t proceed from the “inside out” like it does in strict languages, but rather from the “outside in,” evaluating expressions only as their results are *demanded*.

In a strict language, `foldl (+) 0 [1, 2, 3, 4]` doesn’t *actually* get turned into the expression `(((0 + 1) + 2) + 3) + 4`; as I mentioned earlier, it’s implemented as a tail-recursive loop. But in Haskell, it basically *does* get expanded into that expression before any reduction starts—each application of `(+)` is lazily suspended in a thunk.

If we explicitly denote thunks with ⟨⟩ brackets, the thunk we’ll end up with looks like this:

```haskell
⟨⟨⟨⟨0 + 1⟩ + 2⟩ + 3⟩ + 4⟩
```

These thunks will only get forced when the outermost thunk is evaluated, which will cause `(+)` to be applied to the arguments `⟨⟨⟨0 + 1⟩ + 2⟩ + 3⟩` and `4`. Since `(+)` is strict in both its arguments, it will force the next thunk, which will in turn apply `(+)` to `⟨⟨0 + 1⟩ + 2⟩` and `3`, and so on until the whole thunk tree is reduced.

The result ends up being the same, but from a practical point of view, this is really bad, because instead of reducing the list in constant space, like we did in the strict language, we’re now creating a thunk linear in the size of the input list! It’s even worse that that, though, because in a strict language, the input list takes up space linear to its own size, so our overall space consumption for producing/consuming the list would simply be a constant factor increase, but in a lazy language, the list itself is more like a *stream*, which may actually use constant space if the whole stream is not fully realized in memory. By using the lazy `foldl`, we’ve possibly gone from a constant-space algorithm to a linear-space algorithm, which is bad!

What we want is to recover the behavior of the strict language’s `foldl`, efficiently updating an accumulator as we traverse the list, not building thunks. Therefore, we need a stricter version of `foldl`, which is exactly what `foldl'` is. `foldl'` places a demand on the `⟨0 + 1⟩` thunk *before* moving onto the next element of the list, so instead of building a larger `⟨⟨0 + 1⟩ + 2⟩` thunk, it simply builds a `⟨1 + 2⟩` thunk. `foldl'` continues traversing the list in constant space, never building a thunk larger than a single application of `(+)`.

### `foldr`, lazily

But what about `foldr`? Remember that in a strict language, `foldr` already needed to consume space linear in the size of the input list, since it fundamentally needed the last element in the list before it could start reducing. Indeed, if we consider a lazy `foldr` with a strict operation like `(+)`, this is still true—but in an interestingly *different* way from `foldl`.

With `foldl`, we ended up building thunks incrementally as we traversed the list, leading to a very large, nested thunk. But with `foldr`, that doesn’t actually happen. Why? Well, consider the expansion again for just a moment:

```haskell
  foldr (+) 0 [1, 2, 3, 4]
= 1 + (2 + (3 + (4 + 0)))
```

To consider how we end up with this expansion, let’s write the expansion out in an explicitly inductive way:

```haskell
  foldr (+) 0 [1, 2, 3, 4]
= 1 + foldr (+) 0 [2, 3, 4]
= 1 + (2 + foldr (+) 0 [3, 4])
= 1 + (2 + (3 + foldr (+) 0 [4]))
= 1 + (2 + (3 + (4 + foldr (+) 0 [])))
= 1 + (2 + (3 + (4 + 0)))
```

This makes the recursive calls to `foldr` more explicit. In a strict language, as soon as we call `foldr`, we need to demand the result, so we *have* to traverse the whole list. But here’s where things get interesting—in a lazy language, we can actually just return the following result, with a suspended thunk:

```haskell
  foldr (+) 0 [1, 2, 3, 4]
= 1 + ⟨foldr (+) 0 [2, 3, 4]⟩
```

This might seem totally irrelevant, since once that result is forced, the `⟨foldr (+) 0 [2, 3, 4]⟩` will be forced by `(+)`, and we’ll get the same reduction sequence we had before. But note that this is only true because `(+)` is a strict operation. What if we instead used a *lazy* operation, like `(:)`? In that case, we’d get the following expansion:

```haskell
  foldr (:) [] [1, 2, 3, 4]
= 1 : ⟨foldr (:) [] [2, 3, 4]⟩
```

Guess what? That result is already in [weak-head normal form (WHNF)](https://stackoverflow.com/a/6889335/465378)! So evaluation just stops there until the rest of the result is explicitly demanded by something else. Now, in this case, this is a silly operation, since `foldr (:) []` is just a complicated identity function on lists, but we could imagine a slightly more complicated function, such as one that doubles each element in a list:

```haskell
let f x xs = (x * 2) : xs
in foldr f [] [1, 2, 3, 4]
```

This will expand into the following:

```haskell
  foldr f [] [1, 2, 3, 4]
= ⟨1 * 2⟩ : ⟨foldr f [] [2, 3, 4]⟩
```

…and again, it will just stop there, since it’s already in WHNF. How is this useful? Well, what if we didn’t actually consume the entire result list, like this?

```haskell
sum (take 2 (foldr f [] [1, 2, 3, 4]))
```

Because `take 2` will only return the first two elements of the list, then when `sum` forces  the list and its values to add them together, it will never even evaluate the thunk `⟨foldr f [] [3, 4]⟩`, and the list will only be partially-traversed.

What are the implications of this? Well, it means that `foldr` can possibly save on work if the reducing function is lazy in its second argument, and the result list is not entirely consumed. In fact, **`foldr` can operate on infinite lists** this way, while `foldl` cannot. It also means that `foldr` may be subject to more [list fusion](https://teh.id.au/posts/2017/06/30/notes-on-fusion/index.html) than `foldl`, though that’s another discussion entirely.

### `foldl` vs. `foldr`, lazily

Okay, so, to briefly recap, here’s what I’ve said so far:

  1. In a lazy language, `foldl` on lists is bad because it’s too lazy, and it builds up big thunks. Use `foldl'` instead to force the thunks incrementally and consume the list in constant space.

  2. In a lazy language, `foldr` on lists is good because it’s lazy, so if the reducing function is lazy in its second argument, it can save on work.

These two things might seem a little contradictory. Why is `foldl` bad because it’s too lazy while `foldr` is good because it’s lazy?

To understand the difference, let’s expand `foldl` inductively like we did with `foldr`:

```haskell
  foldl (+) 0 [1, 2, 3, 4]
= foldl (+) (0 + 1) [2, 3, 4]
= foldl (+) ((0 + 1) + 2) [3, 4]
= foldl (+) (((0 + 1) + 2) + 3) [4]
= foldl (+) ((((0 + 1) + 2) + 3) + 4) []
= ((((0 + 1) + 2) + 3) + 4)
```

See the difference? With `foldr`, the recursive call was pushed into a “leaf” of the resulting expression tree, but with `foldl`, the recursive call is always the root. This is, by the way, why `foldl` is tail recursive—this is exactly what tail recursion *is!*—but it means it can’t possibly be lazy, since it will never be in WHNF until the entire list has been traversed.

This gives us a general rule of thumb for using `foldl` and `foldr` on lists:

  1. When the accumulation function is strict, use `foldl'` to consume the list in constant space, since the whole list is going to have to be traversed, anyway.

  2. When the accumulation function is lazy in its second argument, use `foldr` to do work incrementally to improve streaming and work-saving.

  3. Never use `foldl` or `foldr'`; they’re always worse on lists.

In your case, the accumulation function you’re applying is `Map.delete`, which *is* strict, so you should use `foldl'`.

That said, this is often a micro-optimization, so if the list is not large, it usually doesn’t really matter. It’s just a good habit to get into, and it’s worth understanding, since it’s a great example of laziness in practice.

### Addendum: `foldl` and `foldr` on other data structures

As a final note, you might wonder: if `foldl` and `foldr'` are so useless, why do they even exist? Why not just have `foldl'` and `foldr`?

The answer is that everything I just said only applies to lists. This behavior happens because, fundamentally, `(:)` is a right-associative operation, so the “remainder” of the list is on the right. But if we had snoc lists, like this:

```haskell
data SnocList a = Nil | Snoc (SnocList a) a
```

…then our lists would be *left-associative*, and we’d want to use `foldr'` in situations where we use `foldl'` on ordinary lists and `foldl` where we use `foldr` on ordinary lists. A little confusing, isn’t it?

Ordinary cons lists and snoc lists are basically the two extremes of `foldl` vs `foldr`, but in practice, other data structures are a lot fuzzier. For example, if you have a tree, like

```haskell
data Tree a = Leaf | Branch (Tree a) a (Tree a)
```

…then some elements are on the left and others are on the right, and neither `foldl` nor `foldr` are clearly better. In that case, if you really, really care about performance, `foldMap` and `foldMap'` are usually your best bet, since they don’t specify any particular associativity of calls to `(<>)`. However, we don’t have `foldMap'` until `base-4.13.0.0`, which won’t be available until we switch to GHC 8.8.1. (But in truth, it probably doesn’t matter, anyway.)
