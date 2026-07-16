+++
title = "Enterprise Haskell at H-E-B"
description = "How Haskell earned its place in H-E-B's supply chain."
date = 2026-07-16
[taxonomies]
authors = ["Joshua Miller"]
categories = ["Haskellers from the trenches"]
tags = ["Production", "H-E-B"]
+++

*This article is the second installment of our series "[Haskellers from the trenches]",
where we invite experienced engineers to talk about their subjects of expertise, best practices, and production tales with Haskell*

## The mainframe problem

When I joined [H-E-B](https://www.heb.com) back in 2018, I was entering a world far removed from any
of my previous roles in technology. H-E-B is a _retail_ company first, and the
largest privately held company in Texas. Our customers care about full shelves,
not the systems behind them, and for decades those systems delivered:
rock-solid COBOL mainframes doing their job reliably, day in and day out.
Reliability like that is no small thing. But customer expectations have
shifted. Curbside pickup, home delivery, real-time inventory, in-store search,
and many other tech-heavy features have really only emerged in the last 10
years, and meeting that demand means scaling supply chain operations across
435+ stores and the distribution centers that feed them. That kind of growth
asks more of our core systems than they were originally designed to give,
especially as the grocery business turns more digital and more competitive by
the year.

It's no secret that a lot of the world still runs on mainframes. There's
usually little incentive to spend the capital on a massive lift-and-shift into
the cloud when current systems maintain the status quo well enough. Copying
code from one machine to another is hardly the real challenge here. Usually
these kinds of projects fail to deliver because they lack the bespoke
institutional knowledge encoded in decades of legacy business logic. How do you
take a monolithic application operating on hundreds of tables and split it out
into microservices, owned and operated by their own autonomous engineering
teams? It's both a technological and political undertaking of enormous
proportions, and an expensive one. Whatever product comes out on the other end,
you can bet that the business owners don't want to repeat this process ever
again.

## Enter the lambda

Haskell was originally pitched by consultants as the lingua franca for a new
initiative to replace the largest mainframe application in the company. I came
onto this new project in its infancy, ready and giddy to use Haskell on a
real-world project with real-world impact. What started as an experiment grew
into multiple production systems now running critical supply chain
applications. Eight years on, those systems are still in production, with
nearly a million lines of Haskell maintained by several teams across different
domains. So there have been real successes, but also real struggles. I hope that
my experience on these projects can translate into some valuable lessons for
those who wonder about or may already be running Haskell programs in
production.

## Why Haskell?

Pitching a programming language as a business solution is an uphill battle.
Haskell is not known for being an enterprise language, and while it thrives in
programming-language research, it lost the foothold it once had in the intro
course, despite Dijkstra's famous defense of it over Java in his [letter to the
UT Austin Budget
Council](https://www.cs.utexas.edu/~EWD/transcriptions/OtherDocs/Haskell.html).
But "enterprise language" says more about market forces than language design.
The same momentum that filled the intro course with Java fills the hiring pool
and the vendor SDK lists, and nearly all of that momentum belongs to imperative
languages, deemed easier to hire for, and these days, easier to prompt for. The
enterprise cares not for laziness, pure functions, immutable data, or anything
that weighs lambda calculus against object-oriented orthodoxy. The enterprise
just wants another COBOL: a system that runs for 40 more years without
null-pointer exceptions and with a lower maintenance burden. I firmly believe
that Haskell fits into this niche. But it's not trivial to translate the
benefits of a souped-up polymorphic lambda calculus and software-transactional
memory into a business case.

So what's my pitch?

I can tell our CTO that a Haskell application will suffer fewer classes of
runtime failure by default, ruling out the whole category of bugs that
imperative and dynamically typed programs encounter on a daily basis. There
will still be bugs, but the ones that remain tend to be the interesting ones in
our business logic, rather than null-pointer exceptions and type confusion.

In the face of multi-year strangler fig migrations of legacy systems, I can
tell Haskell developers that _it's ok to make mistakes_. They can iterate
while the business requirements are still taking shape, because GHC gives
them the tools for painless refactoring. That freedom is what lets a
migration run like an agile project instead of a waterfall. The business
measures results incrementally, rather than paying up front for an incorrect,
crystallized product that will be abandoned in a year.

Integrating with legacy systems is bread-and-butter work for Haskell programs.
By chance do you need to parse thousands of lines of fixed-width IBM copybooks?
Parsers in Haskell are best in class. A legacy system that can't easily emit
XML or JSON doesn't have to, because its native format becomes the contract.
Need to hook into old services that only offer C libraries? Haskell has one of
the best foreign-function interfaces in the game. We've written Haskell
libraries around simdjson (C++), Oracle's ODPI-C, and IBM's MQ, to name a few.

The dearth of Haskell SDKs in the wild looks like a showstopper at first. In
practice, you take on the maintenance burden forever, and in exchange you own
your contracts outright, avoid vendor lock-in, and never wait on an upstream
maintainer to ship a fix. For critical dependencies, we've found that trade
well worth making. Today the velocity gap is narrower than it looks, since
porting your favorite Go library to Haskell is the kind of mechanical work a
coding agent does well. Unfortunately, rote translations like that may not
take full advantage of the type system without clever prompting.

There's a subtler advantage that makes Haskell a compelling core enterprise
language: its strength as a host for embedded DSLs. A central team can encode a
domain model or business logic once in a Haskell eDSL and generate safe, typed
code in whatever language other teams already use, letting them hook into your
system without ever writing Haskell themselves.
[Hydra](https://github.com/CategoricalData/hydra), an open-source project that
grew out of graph-data work at Uber, is a striking example. It's a
Haskell-bootstrapped DSL framework that generates semantically-equivalent
implementations across Java, Python, Scala, and more, with a shared test suite
guaranteeing parity. You keep Haskell's correctness guarantees at the source
while meeting every other team where they already are.

We've explored this embedded DSL idea in various ways at H-E-B. In one case,
we prototyped a Haskell DSL for sensitive financial calculations that could be
shared across our Haskell, TypeScript, and Java teams. In another, we built a
Haskell DSL for bidirectional JSON schemas, similar in spirit to the
[autodocodec](https://hackage.haskell.org/package/autodocodec) library, but
using a tagless final encoding that allows users to extend the core semantics
with their own type classes. It lets our Haskell teams standardize on schema
versioning strategies, documentation/OpenAPI generation, and regression/golden
testing. For anything else, developers can write ad-hoc interpreters, e.g.
converting a JSON schema to a PostgreSQL table definition, swapping in
`simdjson` for `aeson`, or layering on additional validation.

I could go on about what Haskell offers programmers in the way of type safety,
purity, and elegant concurrency, but these days most modern languages offer
many of the same features (some of them inspired by Haskell itself). The
difference lies in the kind of engineering culture the language promotes. Ian
Duncan's [post](https://blog.haskell.org/a-couple-million-lines-of-haskell/)
covers this territory well, and I risk parroting it here. Suffice to say, his
general advice holds. Haskell attracts idealists who care about correctness and
think carefully about the abstractions they build with. Hiring for a Haskell
role has never been the problem that leaders think it will be. If anything,
candidates came to us for the Haskell itself, even when they had no particular
interest in supply chain.

## Why not Haskell?

Let's get real for a little bit. Haskell has been around for a long time, it
has warts, and it really pays to stay current with the latest GHC releases.
Without an expert on your team, all it takes is one wart to fester and infect
the entire project. In the beginning, we were fortunate to have consultants
with extensive Haskell experience. We were prescribed guardrails and some basic
rules to ensure we didn't get overwhelmed by language extensions and custom
operator line noise.

Back in those days we didn't yet have the GHC2021 or GHC2024 language editions,
so a sane baseline of extensions was something each team had to curate by hand.
That baseline was really a complexity budget - a cap on how much language a new
hire has to absorb before they can contribute. And enforcing it turns out to be
a losing game, because your dependencies spend the budget for you. Once you
pull in something like `servant` or `haxl`, you've just blown through the whole
budget on what the business sees as plumbing. Serving web requests with
`servant` means encoding your API as a type-level DSL, and extending it with
your own combinators drags in type families and the inscrutable errors that
come with them. Batching concurrent operations with `haxl` means modeling every
data source as a GADT indexed by its result type. These are elegant designs,
but they are not free.

As beginners become adepts and adepts become veterans, the perception of a
complexity budget begins to fade. More advanced features become tempting to
use, and their adoption weakens our promise to the business of easy
maintainability. New hires need to understand GADTs, type families, generics,
tagless final encodings, arrows, free structures, higher-kinded data type
patterns, and other shenanigans on top of an already dense business domain. It
wasn't that we started out wanting to use all of these advanced patterns and
features of the language, nor could we really. Their viability emerged over
time as engineers became more familiar with the ecosystem. And that gradual
creep can really sneak up on you.

"Boring" Haskell is easier to preach than to enforce at an organizational
level. We engineers get, well, bored, and we are often tempted to leverage GHC
features for problems that may not warrant them. The key point here is an
intentional echo from Ian's post, which is that encapsulating advanced Haskell
behind sane interfaces reduces the blast radius and scopes your complexity
budget to smaller libraries maintained by a handful of experts. The risk of
exploding complexity at the language level is hardly unique to Haskell, but few
languages offer so much rope. The business may measure complexity in lines of
code or tokens spent, but a better gauge for a Haskell project is how much of
it will break with the next GHC upgrade.

## Trials by fire

A sobering truth is that not all teams are created equal. Haskell can empower
one team while disempowering another. If this were not true, we would have more
Haskell teams than we do today. I've had to watch Haskell fail to stick on two
separate projects, and for seemingly unrelated reasons. In one case, a nasty
space leak brought the entire program to a halt. This was a team of seasoned
developers with no Haskell experience at all, using a project template copied
from a sibling team. They were simply hoping to follow the conventions they
inherited, but without a strong Haskell developer to guide them along, the
space leak was enough to kill the entire endeavor. In the other case, the
engineers who advocated for Haskell and built the applications left to work on
other projects - priorities shifted, nothing to do with the language.

In both cases a fallback option was always available. Engineers
gravitated back to what they wrote before Haskell, unconvinced that it was
making them more productive. This is the consequence of viewing Haskell as just
another means to an end, as just another programming language. The engineers
learned the language well enough. They just never found a passion for it.
Ultimately, as saccharine as it may sound, this is the differentiator for a
successful Haskell project in the enterprise. Nobody will be motivated to
diagnose a space leak if there's an escape hatch to a Spring Boot template,
and now that LLMs can mechanically translate whole codebases, that escape hatch
is wider than ever. No codebase is too clever to rewrite, so the only real
defense is to build a team that would rather keep the Haskell.

Even on teams that embrace Haskell, there were some harrowing moments
that made us reflect on our choices. We ran into double-free errors stemming
from [custom reference counting
logic](https://github.com/hdbc/hdbc-postgresql/pull/52), and we disabled
parallel garbage collection on one project because it was the only thing that
fixed a runaway space leak. In the worst case, [infinite
loops](https://github.com/iand675/datadog/pull/33) are one of the scariest bugs
simply because [finding them is
hard](https://discourse.haskell.org/t/debugging-program-with-infinite-loop-somewhere/13111).

The GHC runtime itself has never been the root cause of a production outage
for us. In steady state, its throughput and memory footprint have been more
than competitive for our workloads, and the war stories above were exceptions
rather than the norm. What matters here is that engineers were able to fix these
problems by tracing through segfault core dumps, garbage collection metrics,
and heap profiles. A crucial lesson learned is that test coverage cannot be
sacrificed in the name of a strong type system. In fact, I wager that Haskell
applications are prime candidates for chaos engineering. Because whole
categories of trivial errors are compiled away, injecting infrastructure faults
surfaces the genuine logic and resource failures of your system instead of the
noise that would dominate a dynamically typed program.

## Times have changed

When we started building our systems, we were on GHC 8.2. We had to write
most of our own API clients and SDKs for popular platforms. The Haskell language
server did not exist. GHCup was brand new. Profiling the runtime was a dark
art. If you'd asked me back then whether Haskell would increase velocity, I'd
have had some caveats. The business wants everything off the shelf, it doesn't
want to pay to reinvent wheels, and we had to spend resources on building what
we needed. Developers coming from their Java IDEs recoiled at the state of
Haskell tooling. H-E-B even went so far as to sponsor development of Debug
Adapter Protocol support for Haskell (for VS Code users).

But now, after years of hard work on GHC and across the ecosystem, building
production-grade Haskell applications has gone from merely viable to genuinely
competitive.

For tooling we have HLS, `ghcid`, `ghciwatch`, `static-ls`,
[tricorder](https://github.com/atelier-hub/tricorder), and first-class code
editor plugins for Haskell. Some of us at H-E-B use GHC plugins to facilitate
richer GHCi-driven development, while some of us use VS Code and HLS, and we
cherish the freedom of developers to use their preferred tools.

For observability we now have `hs-opentelemetry`, and we use
[opentelemetry-auto](https://github.com/aaronallen8455/opentelemetry-auto), a
GHC plugin that autoinstruments Haskell functions with OTel spans. If the
business says they'd prefer Java or Python for their instrumentation story, we
can point at this and say Haskell has the same story.

The library shelf has filled out as well. Our services run on
workhorses like `warp`, `aeson`, `attoparsec`, `stm`, `conduit`, `servant`, and
[orville-postgresql](https://hackage.haskell.org/package/orville-postgresql).
Where we find gaps, we write our own. For example,
[arbiter](https://github.com/velveteer/arbiter), a transactional PostgreSQL job
queue I maintain, was born from our work at H-E-B and runs in production
today.

All of this means developers and operations can both stop shuddering when
someone says "this application uses Haskell". It's only getting better as GHC
improves its own runtime diagnostics, compiler performance, monitoring features
(e.g. eventlog streaming), and [exception
annotations](https://well-typed.com/blog/2026/05/lay-annotation-land/). For the
record, I haven't feared a space leak since info table profiling landed in GHC
9.2.

## Haskell culture

Internal libraries matter a great deal in our organization, and we encourage
developers to work on both internal and open-source projects. We've been
running a Haskell meetup every other week for years, hoping to attract interest
across the company, sometimes succeeding. The meetups serve as an open forum for
engineers to share their own Haskell explorations and teach others.
Cross-pollination of ideas and patterns matters, because silos are everywhere
in the enterprise. This is also our answer to the bus-factor risk.
The more engineers who can read and own a codebase, the less any single
departure can sink it.

How do we empower developers while simultaneously advocating for boring
Haskell? Certainly we set some loose ground rules, but tech leads make the
final decisions on how to spend their complexity budgets. If a team must store
PII encrypted at rest, it will likely pay off to add type-level guarantees that
make leaking sensitive data a compile error rather than a runtime incident,
the kind of assurance security reviews actually care about. If a team is tiring
of boilerplate and wants to consolidate four API contracts using a
trees-that-grow pattern, it may not be worth obscuring the public-facing
schemas with type families.

For us, boring Haskell doesn't preclude type-level programming or advanced
GHC features. It's more that we establish principled conventions and weigh the
costs of straying from them. A big one is "no spooky action at a distance",
e.g. no `TemplateHaskell` code generation, no `Generic`-derived serialization
logic, etc. The convention in this case is to write boilerplate and keep
everything explicit for the programmer. It's not as draconian as it sounds,
since teams who want quasiquoters and deriving via `Generically` are not
forbidden outright. We want healthy skepticism when an engineer implements the
type-level state machine for a web form, or reaches for a GADT when a regular
ADT would do. An engineer can argue that their complexity is essential, but it
takes the whole team, and a culture where pushback is welcome, to spot when
it's accidental.

That said, there's plenty of advanced Haskell across our projects in both
libraries and production applications. I think it goes to show that you don't
have to stay bored forever. You just have to limit the scope of your playtime.
We use Haskell because we want to work _and_ play, and somewhere in the middle
is where engineering happens.

## The road ahead

I am incredibly optimistic about Haskell and GHC's future in industry. Through
many years of using GHC we've found and reported a handful of bugs, none of
which were game-breaking for us. Production incidents are rare, and the
few we do see trace back to business logic far more often than to type errors
or the runtime.

There's also a new argument forming in Haskell's favor. Coding agents now write
a growing share of our code, which shifts the bottleneck from writing code to
verifying it, and a compiler like GHC is well suited to that job. The same
type system that makes refactoring painless for humans gives an agent a tight
feedback loop that rejects hallucinated code before it runs.

We need more stories of Haskell's success, and I am grateful that the Haskell
Foundation is making the effort to bring them to light.

[Haskellers from the trenches]: /categories/haskellers-from-the-trenches/
