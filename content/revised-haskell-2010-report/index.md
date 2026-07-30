+++
title = "A Revised Haskell 2010 Language Report"
date = 2026-08-01
[taxonomies]
authors = ["David Binder"]
categories = ["Ecosystem"]
tags = ["haskell-2010"]
+++

The Haskell 2010 language report was released almost exactly 16 years ago, and although various attempts have been made, no further report has been released.
This does not mean, however, that the language hasn't changed during that time.
On the contrary, the Haskell that we write today is quite different to the one we wrote 16 years ago, and the language has become more beautiful, consistent and ergonomic during that time.
The official specification, unfortunately, couldn't keep up with the speed of those changes.

Many members of our community express some pessimism when it comes to the possibility of a new report. Sure, they would also love to have a new and updated report, but they think that it is either too hard or not worth the effort. I will argue the contrary: Having an up-to-date report is essential for the health of our community, and it is actually possible for us to get there in a reasonable amount of time with a reasonable amount of work.
My Christmas wish for the Haskell community for this year is that we gift ourselves a revised Haskell 2010 language report. Here is my plan how we can get there.

# Why We Need a New Report

The Haskell Language Report is an essential part of the documentation of the language. Sure, it is not the first thing that a beginner should consult when they learn Haskell, but it is an important resource for those who want to advance to being an intermediate or experienced Haskeller. If you want to learn about the precise syntax of floating point literals, the rules and desugaring for do-notation, or the meaning of `default` declarations, the report is the authorative resource where you can find answers.
It is therefore unfortunate if the answers that you find in the report are wrong, or do not correspond to what compilers implement or the base library provides.
In that case, you have to retrace the proposals and discussions that lead to the current state:
The Functor-Applicative-Monad, MonadFail, Foldable-Traversable and Monad-of-no-return proposals undoubtedly made the language better, but they also turn programmers looking for answers in the report into involuntary Haskell historians.

If the Haskell 2010 language report were all that is available to you to know how the language works, then any non-trivial Haskell 2010 program that you would write is almost guaranteed to not be compilable with modern compilers like GHC or MicroHS. We can fix that situation, so let's do it!


# How to get there