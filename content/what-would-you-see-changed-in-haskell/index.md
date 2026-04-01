+++
title = "What Would You See Changed in Haskell?"
description = "We received 553 answers to the question “If you could change one thing about Haskell, what would it be?”. This is the breakdown."
date = 2026-04-02
[taxonomies]
authors = ["Hécate"]
categories = ["Ecosystem"]
tags = ["Ecosystem Survey"]
+++


This report condenses the free form answers to Question 77 of the [State of Haskell Survey 2025](https://discourse.haskell.org/t/state-of-haskell-2025-results/13755).
Due to the phrasing of the question, we have received 553 answers, with varying degrees of precision, actionability, and relevance.
A team of two people (Sawa & Hécate) proceeded to quantitatively and qualitatively analyse the survey results,
extracting trends and topics about what the community would like to see changed.

## Interpreting The Answers

A qualitative analysis of survey answers entails the hard task of noticing trends and moods, as well as determining the relevance of answers.
We needed to understand if an answer is an example of the [XY Problem](https://en.wikipedia.org/wiki/XY_problem),
and if so establish what the deeper problem is. Some answers were simply too vague to be classified either as a trending mood, or an actionable suggestion. 
Finally, one important thing to do when performing such an analysis is to have empathy for the people who spent their time responding to the survey and not outright dismiss their answers.

## Major Topics

The following topics are the ones that have collected the most answers from the survey participants. They are presented in no particular order.

### Switching APIs to using `Text` instead of `String`

A laser-focused suggestion that came up is the replacement of `String` by `Text` as the canonical type for textual data. In particular, one answer explains the reasoning:

> The `text` library must live from `base`,  because none of the APIs of `base` can use `Text`, and when people look
> at base for examples of well-written Haskell code, they end up using `String`, which is not the right choice.

This has been a common source of frustration for beginners and advanced users alike. 
The topic was especially discussed in 2022 on the Discourse: [*Bringing Data.Text into `base`: What is the next step?*](https://discourse.haskell.org/t/bringing-data-text-into-base-what-is-the-next-step/5016/)

### Onboarding of Beginners

Concerns regarding the quality of resources for beginners have been expressed in large numbers. Up-to-date content, and a centralised entry point in the language are some of the most actionable suggestions that we received. 
The Haskell Wiki is especially well-referenced by search engines, but has seen a steady decrease in contributions. People are encouraged to apply for an account and contribute to the articles.

### Documentation

This one will not be a surprise to anyone, but thankfully some of the insights that were received pointed to concrete improvements that could be made. Library authors are encouraged to provide more *working* examples, as well as documentation that goes beyond the scope of a simple API reference.

Community gatherings lend themselves especially well to documentation contributions, as people needing the documentation can meet with people who are best positioned to write it.

For more information about what types of documentation exist and when to use particular ones, you can read [Documentation Best Practices in 2024](https://blog.haskell.org/documentation-best-practices-in-2024/).

### Complexity of the Haskell Language

We have noticed a trend amongst participants that the Haskell Language is perceived as too complex, riddled with historical cruft, and that there is a lack of consistency in how the language and its libraries have evolved over the decade.

Avoiding partial functions in the Prelude and more generally sane / safe defaults are a recurring suggestion. People want to feel proud of the `base` library, where the strengths of Haskell can shine, and not be just a hodgepodge of convenient functions.

There have been requests to stop adding language extensions to GHC or even re-design the existing ones with a holistic approach. This feedback is in line with answers asking for a new Haskell Language Report, which could take advantage of the extensions that GHC has introduced over the years

### The Record System

A certain proportion of survey participants have expressed their dissatisfaction at the state of records in Haskell. Answers remained vague, which gives us a mood indicator more than an action plan, but there are notable requests for `OverloadedRecordDot` and `NoFieldSelectors` to be enabled by default.

### Tooling

#### Compilation times

“Make GHC 10x faster” is one of the prevalent moods amongst participants. Coming from end-users, these suggestions do not come with actionable advice or specific pain points in the compilation pipeline. Specific situations like underpowered laptops have been mentioned.

#### Ecosystem consolidation, Developer Experience

As the Haskell language and its ecosystem are more than thirty years old, it’s normal that we see a certain degree of fragmentation in the tooling, as new people and ideas enter the community. However, they tend to also confuse newcomers, who do not wish to navigate the intricacies of the technical and political disagreements that produced the existing tooling ecosystem. Survey participants very helpfully directed our attention to projects like [`uv`](https://docs.astral.sh/uv/)  for an example of a fully-integrated toolchain (tooling management and project management).

Compilation error messages are also called confusing. This might be explained by a mismatch between the intended audience of those messages and the actual audience. Error messages may feel more intended for implementers than for end-users. Recent versions of GHC have started to direct users to the [Haskell error index](https://errors.haskell.org/), so we can expect some positive development in the coming years. However, we should keep improving error messages for end-users who are not in the know about implementation details.

Lastly, the [Haskell Language Server](https://haskell-language-server.readthedocs.io/en/latest/what-is-hls.html#what-is-the-haskell-language-server) is often mentioned when participants complain about development experience, mostly due to the fact that they have to directly interact with it. Memory usage and reliability are thus felt by most people.

### Program Performance

#### Pervasive Laziness

Anxiety about laziness is a recurring theme, indicating lasting difficulties developers have in reasoning about lazy evaluation. Resource blow-up due to misunderstood runtime behaviours make people anxious when it comes to deploying or distributing Haskell software. Actionable advice like eliminating lazy I/O was given, which is indeed known for tripping people up, when they may expect it to happen only when they use a streaming library.

Some suggestions are more targeted, like having data structures strict by default, and only explicitly lazy when the developer wishes so, by means of the [StrictData extension](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/strict.html#strict-by-default-data-types).

## Minor Topics

These topics have been mentioned in lesser but still substantial amounts. This does not mean that they are worthless, just that other concerns seemed more pressing to most participants.

### Dependent Haskell

When people are not thinking about what is bad, they start thinking about what could be better. The subject of Dependent Types has come up enough times to attract our attention. Dependent Haskell is viewed as a unified solution that can replace otherwise loosely related language extensions. You can see the progress of the implementation on the [Dependent Haskell Roadmap](https://ghc.serokell.io/dh) page.

### Community

The desire for community is ever present in human beings, and Haskellers are no exception. They want to see their community thrive, meet more people, and see higher regard for non-technical contributions.

### Effect Systems

The past few years have seen an increase in the research and development of algebraic effect systems, meant to replace the shortcomings of the existing Transformers/MTL approaches. We are beginning to see an interest for `base` to provide such a thing out of the box. Participants wishing for more interoperability also expressed the desire to change most functions operating in the `IO` monad to instead make use of `MonadIO`.

Participants expressed interest in having a coherent story when it comes to the architecture of Haskell programs, the choice of libraries, and the fragmentation of the ecosystem.

### Debugging

Debugging is still a sore point for participants, but the [Haskell Debugger](https://well-typed.github.io/haskell-debugger/) is mentioned and seems to address most of the pain points. “`HasCallStack` everywhere" is also mentioned, which we consider to be a symptom of the perception of the existing debugging tools and backtrace mechanisms.

## Academia, Industry, Hobbyists

An underlying theme of many of the previously mentioned answers is the question of “To whom does Haskell cater?”. Historically an academic project to study non-strict evaluation and pure functional programming, Haskell has grown to be adopted by software engineers in industry, leading to different ways of learning and socialising. A certain proportion of industrial programmers feel disenfranchised by the academic focus of the ecosystem, especially those that do not have an academic background or formal education.

One answer in particular elaborates on the subject:

> There needs to be a recognition that there is documentation produced for/by the type theory / heavy mathematics enthusiasts which doesn't translate well as developer documentation. When new developers are looking for best practices they are often pointed into the direction of blogs written by clever, well-meaning people who write in a verbose style, often from the vantage point of mathematical abstractions. Any documentation that is software engineering oriented is always welcomed, but surprisingly rare.

## Special Mentions

Some answers had to be categorised in a way that had to reflect how incredible they were. Here are our favourites:

#### "A whole other language"

Some survey participants just want another language, going beyond taking inspiration from others, with suggestions such as swapping the meanings of `::` and `:`, removing laziness, or a total language and tooling cleanup, scorched-earth style. [Lean](https://lean-lang.org/) and [Idris](https://www.idris-lang.org/) might be more up your alley.

#### "I don’t know how, but make it better"

The vague rants and self-admitted impossible ideas hold a special place in our heart. “Replace base”, “tooling like TypeScript”, and “fix the weird syntax” were pretty good. It's hard to imagine these changes being possible without the level of support given to large corporate projects, such as VS Code, TypeScript or Go.

Special mention for “weird syntax”, which we can **absolutely** **do** something about, we just forgot to press the big red button that says “fix syntax”. Thanks for the reminder!

## To conclude

This year’s survey has brought us a very valuable insight into the preoccupations of a certain proportion of Haskellers. Of course, the results are to be handled with care, as surveys are a self-selecting tool in the first place. We cannot possibly know what exactly the community thinks, only what the subset that answers the survey thinks. 
We had a variety of answers, ranging from one word (“base”) to several paragraphs that could be the start of a [GHC Proposal](https://github.com/ghc-proposals/ghc-proposals/?tab=readme-ov-file#ghc-proposals) or a [Core Libraries Proposal](https://github.com/haskell/core-libraries-committee?tab=readme-ov-file#core-libraries-committee). Anyone can open one, and we encourage you to do so!

Tooling is a common thread to many topics, from beginner onboarding to consolidation of the developer experience. There is a tension between a fully integrated development ecosystem and having a language with multiple implementations that are free to explore different paths. 

Thank you again for participating in this year’s survey. Each new edition gives us more data points to measure the community trends, and they help the various groups with crafting the best experience for Haskell developers.
