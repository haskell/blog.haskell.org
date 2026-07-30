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
The specification, unfortunately, couldn't keep up with the speed of those changes.

Many members of our community express some pessimism when it comes to the possibility of a new report. Sure, they would also love to have a new and updated report, but they think that it is either too hard or not worth the effort. I will argue the contrary: Having an up-to-date report is essential for the health of our community, and it is actually possible for us to get there in a reasonable amount of time with a reasonable amount of work.
My Christmas wish for the Haskell community for this year is that we gift ourselves a revised Haskell 2010 language report. Here is my plan how we can get there.

## Why We Need a New Report

The Haskell Language Report is an essential part of the documentation of the language. It is not the first thing that a beginner should consult when they learn Haskell, but it is an important resource for those who want to advance to being an intermediate or experienced Haskeller. If you want to learn about the precise syntax of floating point literals, the rules and desugaring for do-notation, or the meaning of `default` declarations, the report is the authorative resource where you can find answers.
It is therefore unfortunate if the answers that you find in the report are wrong, or do not correspond to what compilers implement or the base library provides.
In that case, you have to retrace the proposals and discussions that lead to the current state:
The Functor-Applicative-Monad, MonadFail, Foldable-Traversable and Monad-of-no-return proposals undoubtedly made the language better, but they also turn programmers looking for answers in the report into involuntary Haskell historians.

If the language report were the only Haskell resource available to you, then any non-trivial Haskell 2010 program that you would write is almost guaranteed to not be compilable with modern compilers like GHC or MicroHS. We can fix that, and we should fix that.


## How to Get the Cost-Benefit Tradeoff Right

Writing a new report is a lot of work, and we want to avoid running into the problems that stalled previous attempts of writing one.
It is therefore important that we set ourselves a goal that is both achievable and provides a tangible benefit for the Haskell community.
The most obvious such goal is to start with a **Revised Haskell 2010 Language Report**:

- A revised report is eminently achievable: We have a very clear "todo list" in the documented discrepancies in the user guide ([GHC User Guide: Bugs and Infelicities](https://downloads.haskell.org/ghc/latest/docs/users_guide/bugs.html)). Most of these listed discrepancies are already well-documented in the accepted proposals that lead to them.
- None of the changes that we have to implement is likely to be contentious: They reflect the Haskell that we are already writing today. This also means that we can use a very lightweight decision process in the compilation of the revised report.
- There is a very clear benefit for the community: The most important piece of our reference documentation is no longer outdated. We can refer people to the Haskell report without having to warn them to stay clear of certain passages and parts of the report that no longer hold true.
- Since we don't define a new Haskell version with a new feature set, but only a revised version of a previous report, we don't generate any churn for the ecosystem or compiler writers. Taking inspiration from literature, we can even [retcon](https://en.wikipedia.org/wiki/Retroactive_continuity) the report and pretend that the revised report is what we meant by `Haskell2010` all along. The GHC user guide could then be simplified and remove most of its section 16 which documents differences between its implementation and the official report.

So here is the a target that we can set for ourselves: Every program that is valid according to the revised Haskell 2010 language report will be accepted by MicroHS and GHC using the `Haskell2010` language edition.

## Let's Talk Technicalities

With the motivation and goal out of the way, let us discuss some technical details when it comes to working on the report, building it, and generating the final PDF and HTML documents.

The Haskell 2010 language report is available both as a PDF and HTML document, and there are no obvious good reasons to change this.
Working on a document of the complexity and length of the Haskell report can be made joyful if the authoring technology is right, and arduous if we are struggling with long compile times (Compiling longish PDFs with LaTeX can take minutes using the wrong tools and packages!), inscrutable error messages or a difficult scripting and programming model.

The previous version of the Haskell report was written using a combination of command line tools (`sed`, `awk`, `perl`, `make`), a custom lexer/parser for preprocessing a custom `.verb` file format, `pdflatex` to compile the PDF, and `tex4ht` to generate HTML.
In 2026 we can use technology that was not available in 2010 and simplify the process of authoring the report significantly.
Of the available technologies, the [Typst](https://typst.app/) typesetting system currently satisfies the constraints the best:
- It is open-source technology available under the Apache 2.0 license. (An associated cloud service similar to Overleaf is proprietary, but completely optional.)
- Typst is implemented in Rust and has compile times that are roughly an order of magnitude faster than tex: Compiler runs are almost instantaneous, and this allows to see directly what you have changed using `typst watch`.
- Both PDF and HTML output is supported natively, with no need to use separate tools.
- The error messages are fantastic and directly point to the correct location in the code. 
- Simpler technologies like plain Markdown also allow to target multiple output formats, but they lack the advanced features to generate high-quality technical documentation, especially when it comes to the typesetting of mathematics and complex figures.

Using Typst means we have to convert the report from the custom `.verb` format to `.typ` files; fortunately this part of the work is already completed, and the existing report can be compiled using a simple `typst compile` invocation. (The translation process did not involve LLMs, and was done manually using some `sed` and `awk` magic to facilitate the process.)
Re-typesetting the BNF grammars also allowed to make them hyperlinked: It is now possible to navigate the grammar by clicking on non-terminal symbols and to jump to their definition.

## Documenting the Standard Libraries

The existing Haskell 2010 report consists of two parts: The first part documents the syntax and semantics of the language, and the second part documents the standard libraries.
Documenting the standard libraries is an essential part of the report, because many aspects of the language can only be defined in terms of functions, types and type classes available in the libraries. For example, numeric literals are polymorphic and have to be defined in terms of the Numeric hierarchy, do-notation has to be defined in terms of the Monad (and MonadFail) type classes, and the foreign function interface (FFI) makes extensive use of functions and types defined in the `Foreign.*` namespace.

Maintaining hundreds of pages of standard library documentation in purely textual form (either as latex or typst files), and ensuring that they remain valid Haskell and consistent is very hard. And indeed, some unnoticed typos managed to sneek into the existing report.
If we want to avoid that and and if we want to ensure that the library specification remains maintainable then we have to organize it in some form which is amenable to machine checking.
The obvious solution is to document the API of the standard libraries using Haddock. The repository for the revised report contains a cabal package which exposes the precise API specified in the Haskell 2010 language report.
(This package only specifies the public API: The functions and methods themselves do not have an implementation. A possible stretch goal would be to use tools such as `api-diff` and `print-api` [Link](https://discourse.haskell.org/t/maintain-a-golden-test-of-your-packages-api-with-diff-package-api-and-print-api/9997) to ensure that the API exposed by packages like `base` is a strict superset of the API defined in the report.)

## What Process Shall we Use

The Haskell Prime process worked using a combination of mailing list discussions and a Git repository which discussed and collected RFCs. Notably, the process did not work directly on the source code used for building the report.
We can reduce a lot of the overhead by switching to a collaboration process that is modelled more closely on how we develop code: Changes to the report should primarily be discussed in the form of pull requests to the textual sources of the report itself. The repository to organize the writing of the report is now public: TODO Please start contributing by discussing the existing issues, by contributing new issues, and by helping fix issues with the technical infrastructure. At the moment we will not merge any actual changes to the report, since these changes will need to find the consensus of the language committee that we will hopefully be able to establish.

## Call to Action

In order for a new report to enjoy broad support from the Haskell community we have to organize it under the umbrella of our existing institutions.
The Haskell Foundation and its working groups is best suited to host this project, but it will rely on volunteers who can contribute a part of their time to help organize the writing of the new report, and who are willing to help establish a new language committee tasked with revising the language report. While it will involve some work, it is also not going to be an unending endevour with unclear outcomes. The actual amount of work that remains is quite limited and I am confident we can finish a revision of the report within this year! So if you are interested in helping, then please join the discussion on the Haskell discourse!


