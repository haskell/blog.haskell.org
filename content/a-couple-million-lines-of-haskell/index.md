+++
title = "A Couple Million Lines of Haskell: Production Engineering at Mercury"
description = "What it takes to run 2 million lines of Haskell in production at a fintech company serving 300,000 businesses."
date = 2026-03-30
[taxonomies]
authors = ["Ian Duncan"]
categories = ["Production"]
tags = ["Haskell in production", "Mercury"]
+++

I first heard about Haskell when I was sixteen, sitting in a high school computer science class where we were writing Java and learning, among other things, that `NullPointerException` was apparently a lifestyle choice if you decided to go into software development. While looking at the `/r/programming` subreddit after school, I stumbled across a reference to a language where null pointer exceptions simply could not happen, where the type system could prevent an entire category of bugs that I had been fighting with every week. Haskell. I was immediately, hopelessly enamored with the idea.

I have been writing Haskell for nearly two decades now, and I still think the value proposition I fell in love with at sixteen was basically right. What took me longer to learn is what that promise looks like after a codebase gets large, the company grows faster than its documentation, and the system is allowed to touch money. Haskell earns its keep there in numerous, sturdy ways. It lets you pack operational knowledge into APIs, put dangerous machinery behind tight boundaries, and make the safe path the easy one. At a growing company, those aren't just matters of taste; they are how you keep a system understandable after the people who first understood it have moved on.

Fast forward to today: I work at Mercury, a fintech company that provides banking services.\* We serve over 300,000 businesses. We processed $248 billion in transaction volume in 2025 on $650 million in annualized revenue, and are, at the time of writing, in the process of obtaining a national bank charter in the USA from the OCC. We have around 1,500 employees. Our engineering organization largely hires generalists, and most of them have never written a line of Haskell before joining.

My time working at Mercury has changed how I think about the language more than any sermon about purity ever did. Elegance is pleasant, but keeping your business alive is compulsory.

Our codebase is roughly 2 million lines of Haskell, once you strip out comments and such.

This is the part where you are supposed to recoil in horror.

A couple million lines of Haskell, maintained by people who learned the language on the job, at a company that moves huge amounts of money? The conventional wisdom says this should be a disaster, but surprisingly, it isn't. The system we've built has worked well for years, through hypergrowth, through the SVB crisis that sent $2 billion in new deposits our way in five days,[^svb] through regulatory examinations, through all the ordinary and extraordinary things that happen to a financial system at scale.

This article is about why it works. Not in the "Haskell is beautiful" sense, though it is. Not in the "the compiler will save us from ourselves" sense, though I frequently feel gratitude in that direction. I mean in the much less romantic and much more useful sense that we run this language in production, at scale, with a rapidly changing team, and have learned some hard lessons about what it takes to keep the whole enterprise afloat. The beauty of Haskell is charming enough, but there is a whole swath of operational and organizational reality beyond it, and if you ignore that reality for too long, your company will likely fire the whole Haskell team[^zalora] and start writing PHP or something instead.

## How We Think About Reliability

Before diving into practical advice, a note on philosophy.

There is a traditional way of thinking about system reliability that focuses on preventing failures. You enumerate the things that can go wrong. You add checks. You write tests for each bad case. You hunt for bugs. This is, of course, necessary work, and we do it. But it is not sufficient, and if you orient entirely around it you develop a specific blind spot: you get very good at cataloguing the ways things break and very bad at understanding why they ordinarily work.[^safety-one]

We try to think about it differently. A system operates reliably because it can **absorb variation**: it degrades gracefully, its operators can understand and adjust it, and the architecture makes the right thing easy and the wrong thing difficult.[^safety-two] Reliability is not just the absence of failure. It is the presence of adaptive capacity. It is a system's ability to keep functioning while reality continues its longstanding and regrettable habit of refusing to hold still.

When you have hundreds of engineers working in a multi-million-line codebase, many of whom are six months into their Haskell careers, "adaptive capacity" stops being a nifty phrase from a resilience engineering paper and starts being a daily concern. Patrick McKenzie has observed that in a company growing at 2x per year, half of your coworkers will always have less than a year of experience. A year later, half of your coworkers will still have less than a year of experience. For very successful companies, this never stops being true.[^patio11] You become organizationally ancient very quickly, whether you like it or not, and the things you know become institutional dark matter: load-bearing, but invisible to most of the people around you.

So the questions we ask are operational. Can the new hire on your team read this module and understand what it does? If the database is slow, does this service degrade or does it fall over and take its neighbors with it? If someone misuses an interface, does the compiler tell them, or do we find out when the on-call gets paged? If you don't have answers to those questions, you have a future incident quietly unfolding.

This is why I increasingly think of the type system as an **operational aid** more than a correctness proof. Its value is not merely that it rules out certain classes of errors, though it does. Its value is that it encodes institutional knowledge in a form that survives the departure of the person who wrote it. In a fast-growing company, people leave, people transfer teams, people go on vacation or parental leave, people join, and the churn means that things people knew walk out the door with them unless you have written them down somewhere. Ideally, you have written them down in a form that the compiler can read, because the compiler is much more disciplined than the average wiki page.

This extends beyond code. As a member of our stability engineering team, we constantly investigate the prospective production behavior of features and products. We do not do this to slow down product development, but in partnership with the team shipping the feature, to make sure we are prepared to deal with the fallout when it breaks and, if possible, to make that fallout boring rather than exciting. We ask things like: what is the blast radius if this fails? Which operations must be idempotent, and how? What does the rollback look like? What happens to in-flight work? Which systems will absorb the failure, and which ones will amplify it? The point is to have the conversation early enough that it shapes the design rather than merely auditing the launch after all the important decisions have already become expensive to revisit.[^prr]

Our philosophy, stated plainly: we are not the quality police. We are the people who would like to help you avoid being woken up at 4 AM to deal with the fallout of a broken feature. Rather than a deeply ideological stance, we simply desire to help people.

So, in light of that, how do we make Haskell work in production?

## Purity Is a Boundary, Not a Property

My hot take: the first and most consequential misunderstanding about Haskell is that purity is not something the language _is_, so much as that it is something your interfaces enforce.

Under the hood, Haskell is not a magical machine that performs side effects despite being pure. Behind every "pure" function in `bytestring`, `text`, and `vector` lies a cheerful little hellscape of mutable allocation, buffer writes, unsafe coercions, and other behavior that would alarm you if you discovered it in a junior engineer's side project. Behind the `ST` monad lies in-place mutation and side effects, observable within the computation. What makes it acceptable is that the side effects are encapsulated such that the boundary cannot be violated.

```haskell
runST :: (forall s. ST s a) -> a
```

The rank-2 type (that is, the type `s` is scoped within the parenthesis and can't escape) of `runST` ensures that the mutable references created inside the computation cannot escape due to being tagged with the type `s`. Internally, all sorts of imperative nonsense may occur. Externally, the function is pure. The world outside the boundary gets none of the mutation, only the result.

This is, I think, a wonderful design principle in the larger scheme of things when writ large: you can permit arbitrarily dangerous operations within a scope, provided the scope's exit is typed narrowly enough that the danger cannot leak. That principle applies everywhere in production. Your database layer uses connection pooling, retry logic, and mutable state internally. Your cache uses concurrent mutable maps. Your HTTP client probably has circuit breakers, pooled connections, and a small municipal government's worth of bookkeeping. None of this is a problem if the interface is tight enough to prevent misuse and the boundary holds.

In production, the goal is often not to avoid mutation entirely, because that is not a serious proposition for most real systems. The goal is to contain mutation, make the containment legible, and verify that it stays contained. Often the right question is not "is this pure?" but "where is the impurity, and how much of the codebase is allowed to know about it?"

For a new engineer who learned Haskell three months ago, "purity is a boundary you try to maintain" is much more useful than "Haskell is pure." One tells them what to do when they sit down to design a module. The other mostly sits there looking profound.

This boundary-oriented view of purity sets up a more general pattern that recurs throughout production engineering in Haskell: dangerous things are tolerable when they are fenced in, carefully exposed, and hard to misuse. That is true of mutation. It is true of retries, transactions, state machines, distributed workflows, and type-level machinery. Much of what follows is really just this same idea, wearing different hats.

## Make the Right Thing Easy

There is a pattern in large codebases where correctness depends on performing operations in a particular order, or including a particular step that has no visible connection to the main work.

"Remember to flush the audit log after every transaction."

"Always check the feature flag before calling this endpoint."

"Make sure to enqueue the notification *inside* the database transaction, not after it."

These are the incantations of operational lore. They live in wiki pages, onboarding documents, half-forgotten design reviews, and the memories of senior engineers who are now three teams away and booked solid until Thursday. In a company that is hiring aggressively, the half-life of tribal knowledge is alarmingly short. When an engineer leaves, the incantations fade. When a deadline approaches, they are the first thing skipped. When a new engineer joins, they often have no way to know the incantation exists at all. Nothing says "robust system design" quite like a critical invariant living in a Slack thread from nine months ago[^slack].

Haskell gives you tools to encode these incantations in types so they cannot be forgotten. This is, for my money, the single most valuable thing the language offers a production engineering organization.

Consider a simplified version of a real pattern: you need to ensure that certain side effects (sending a notification, publishing an event) happen *transactionally* with a database write. Not before, not after, and not in a separate transaction. Together, or not at all.

The naïve approach is to tell people to use the right function:

```haskell
-- Please use this one, not the other one
writeWithEvents :: Transaction -> [Event] -> IO ()

-- Don't use this directly (but we can't stop you)
writeTransaction :: Transaction -> IO ()
publishEvents :: [Event] -> IO ()
```

This is rookie-level engineering. It works until it doesn't, and "until it doesn't" tends to arrive on a Friday afternoon when the person who wrote the wiki page is on vacation and everybody else is discovering, in real time, that the wiki page was load-bearing.

A better approach restructures the types so that the only way to *commit* work is through a path that includes event publication:

```haskell
data Transact a -- opaque; cannot be run directly
record :: Transaction -> Transact ()
emit :: Event -> Transact ()

-- The *only* way to execute a Transact: commit and publish atomically
commit :: Transact a -> IO a
```

Now the incantation is the only door in the room. You cannot forget it because there is nothing else to do. The type system has not proven anything especially deep about your events. It has done something more practical: it has made the correct operational procedure the path of least resistance.

That distinction matters. In production, there are plenty of places where we do not need a theorem. We need a design that makes it difficult for an ordinary busy engineer to accidentally do the wrong thing while trying to do a dozen other perfectly reasonable things. The compiler is not merely checking logic here; it is preserving institutional memory and turning it into a hard-edged interface.

When a new engineer joins and asks "how do I write a transaction?", the type system answers them. When a senior engineer leaves, the answer remains. The institutional knowledge survived not because someone documented it beautifully, though documentation is pleasant when available, but because someone encoded it in a form the compiler enforces. Again, the compiler is a better custodian of operational lore than the average wiki and less prone to people forgetting to update it as the reality of the system changes.

## Durable Execution

The pattern above — structuring types so that the correct operational procedure is the only procedure — works well within a single transaction. Financial systems, unfortunately, have never felt obligated to remain inside a single transaction.

They are full of processes that span multiple steps, multiple services, and multiple failure modes. Send a payment, wait for a partner to acknowledge it, update the ledger, notify the customer, handle cancellation, handle timeout, handle the case where the partner said yes but your worker died before recording the answer, handle the case where the partner said nothing because the network briefly entered a higher plane of existence and declined to tell you about it. If any step fails, you need to know where you were, what has already happened, and what still needs to happen. You need state. You need retries. You need timeouts. You need idempotence. You need all of these things to keep working across process crashes and deployments. Very quickly, what began as "just some business logic" amasses a remarkable amount of one-off repeats of common operational concerns.

Previously at Mercury, we coordinated these processes with database-backed state machines driven by cron jobs and background workers, with retry logic and timeout handling scattered across the codebase. It worked. It also required the sort of vigilance usually associated with defusing unexploded ordnance. It was fragile, difficult to reason about, and the source of a disproportionate share of our operational incidents.

[Temporal](https://temporal.io) is our durable execution framework, and adopting it was one of the better infrastructure decisions we have made. You write your workflow as ordinary sequential code, and the platform records every step in an event history. If a worker crashes mid-workflow, another worker replays the deterministic prefix to reconstruct the state, then continues from where it left off. Retries, timeouts, cancellation, and error handling are provided by the platform rather than each team reimplementing them poorly.

I think of Temporal as Frankenstein's monster, in the flattering sense: assembled from excellent parts, animated by improbable effort, and smarter than many of the people alarmed by it. It takes durable history, replay, and determinism (things some platforms get natively,) and bolts them onto runtimes that were never born knowing how to do any of this. Most of us are not going to rewrite our companies in Erlang. Temporal is a prosthetic for the rest of us. It gives ordinary languages a shot at some of the same operational virtues by slightly mad but highly effective means.

The alignment with Haskell dovetails nicely with the virtues often attributed to Haskell: A Temporal workflow is, in an important sense, a pure function over its event history. Temporal Workflows have a determinism requirement — that a replayed workflow must produce the same sequence of commands as the original — which is exactly the same constraint Haskell imposes on pure code: same inputs, same outputs. Side effects are isolated into *activities*, which are the workflow's equivalent of `IO`. The workflow orchestrates; the activities execute. If you have spent any time thinking about pure core / impure shell, this is that model with the platform enforcing the separation rather than relying on sheer discipline.

We built and open-sourced [`hs-temporal-sdk`](https://github.com/MercuryTechnologies/hs-temporal-sdk), our Haskell SDK for Temporal, which wraps the official Core SDK (Rust, via FFI) and provides a Haskell-native API for defining workflows, activities, and workers.

I gave a [talk about our adoption patterns at Temporal's Replay conference](https://www.youtube.com/watch?v=FdGMaLcEP2M), and the short version is: Temporal has let us replace fragile chains of cron jobs and database-backed state machines with durable workflows where the platform handles the coordination. The operational improvement has been substantial. It is difficult to overstate how pleasant it is to delete a hand-rolled distributed state machine and replace it with something whose failure semantics were not improvised during sprint planning.

This, again, is adaptive capacity in a different costume. A system that can survive worker crashes, process restarts, and long-lived coordination without losing its place is a system whose operators have more leverage and fewer mysteries to unwind during an incident.

## Design for Your Domain, Not Your Transport

As your production system grows, a common mistake I've observed is letting the invoking system leak into the domain model.

We have code that throws HTTP status code exceptions which return those results directly to the user on the frontend. This made sense when the code was written, because it ran in an HTTP request handler. Then, as happens in any growing codebase, pieces of that code got extracted and reused. Now it also runs in cron jobs. It runs in queued background workers. It runs in Temporal workflows. And it still throws `StatusCodeException 409 "Conflict"` when something goes wrong, which is an absolutely unhinged thing for a cron job to do. A cron job does not have a caller waiting for a 409. Nobody is reading that status code. The error has propagated through the system simply because the original abstraction was coupled to its transport layer.

The fix is conceptually simple: model your domain errors as domain types. A payment that fails because of insufficient funds should be an `InsufficientFunds`, not a 402. A duplicate request should be a `DuplicateRequest`, not a 409. These are things your business logic can match on, retry against, log meaningfully, and handle differently depending on context.

Then you write thin translation layers at each boundary:

```haskell
data PaymentError
  = InsufficientFunds
  | DuplicateRequest RequestId
  | PartnerTimeout Partner

toHttpError :: PaymentError -> HttpResponse
toHttpError InsufficientFunds       = err402 "Insufficient funds"
toHttpError (DuplicateRequest _)    = err409 "Duplicate request"
toHttpError (PartnerTimeout _)      = err502 "Partner unavailable"

toWorkerStrategy :: PaymentError -> WorkerAction
toWorkerStrategy InsufficientFunds    = Fail "Insufficient funds"
toWorkerStrategy (DuplicateRequest _) = Skip
toWorkerStrategy (PartnerTimeout _)   = RetryWithBackoff
```

Nothing novel about it. But it is remarkable how often it gets skipped in practice, because the first version of any code is written for *one* context, and by the time you realize it will be called from three others, the status code exceptions are load-bearing because someone decided to catch those exceptions as part of business logic and nobody wants to refactor them.

The earlier you make this separation, the less it costs. The later you make it, the stranger the resulting behavior becomes. Eventually you wind up with cron jobs hurling 409s at Sentry and background workers interpreting HTTP-specific exceptions as business semantics, which is how abstractions let you know they have escaped containment.

This is the same principle as purity, expressed in domain language rather than operational language. Your transport concerns belong at the edges. Your domain model should survive being invoked from a web handler, a CLI, a cron job, a background worker, or a workflow engine without having to drag an HTTP status code behind it like a tin can tied to a wedding car.

## The Type Encoding Tradeoff

Here is the part where I tell you not to do too much of the thing I just told you to do.

Encoding invariants into types is powerful. It is also expensive. Not at runtime, but in cognitive overhead, in the rigidity it introduces, and in the difficulty of changing things later when the requirements shift. And the requirements *will* shift. If you work at a company where they do not, I would like to know your secret, and also your stock ticker.

Every invariant you push into the type system is a constraint on every future engineer who touches that code. If violating the constraint would cause data loss, financial errors, regulatory trouble, or a poor soul's pager to go off, then the cost is justified. If the constraint is "we currently happen to do things this way," or "I read this article about dependent types and I *simply must* apply that to my authorization logic," you have likely just made your codebase harder to change for no operational benefit. The next person to encounter it will either spend a week refactoring the types or, more likely, find a way around them that is worse than what you were trying to prevent.

There is a spectrum, and production codebases must live on it honestly.

At one end: you encode everything. Your types are a faithful model of your domain. Illegal states are unrepresentable. Refactoring takes weeks because changing a business rule means threading a type change through fifty modules. New engineers stare at the type signatures and wonder what they have done to deserve this, then quietly begin discussing their career options with a therapist. You have built a cathedral. Cathedrals are beautiful. They are also expensive, cold, and not especially famous for how quickly one renovates the plumbing.

At the other end: you encode nothing. Your types are `String` and `IO ()` and, in the worst case, `Dynamic`. The code is easy to change because there are no contracts to violate. The system works because the people who built it are still around and remember what the strings mean. When they leave, it stops working, and nobody knows why. You have built a tent. Tents are flexible, portable, and, under certain weather conditions, a very direct way to learn about the sky. This is, of course, one of the reasons many Haskell developers flee to Haskell in the first place — to avoid the suffering that comes from this approach.

The sweet spot is somewhere in the middle. A few heuristics I think are useful:

**Encode invariants that protect against silent corruption.** If a violation would produce wrong data without any immediate error (a transaction committed without its events, a payment processed without an audit log, a state transition that looks plausible but is semantically impossible), put it in the types. The feedback loop for silent failures is too long to rely on human diligence.

**Use runtime checks for invariants that fail loudly.** If a violation would produce an immediate, obvious error (a 500 response, a failed assertion, a type mismatch at a JSON boundary), a runtime check with a good error message may be enough. You will catch it before production, or very quickly after.

**Resist the urge to model your entire domain in types.** Your domain is messy. It has edge cases, grandfather clauses, rules that contradict each other, and special behavior for three specific customers that dates back to 2018 and that nobody fully understands. The type system wants crispness. Your business does not provide it. Nor will it ever.

**Remember that types are for the team, not just for the compiler.** The compiler is one tool among many. Tests, documentation, code review, examples, playbooks: these all combine to provide defense in depth. The goal is not to win an argument with the type checker. The goal is to build a system that a team of humans, including humans who learned Haskell this year, can operate, extend, and maintain.

That said, intense type-level machinery is sometimes exactly what you need. We have internal libraries where the types are genuinely hairy: GADTs, type families, phantom types tracking state transitions. These tend to be mechanisms where getting it wrong means money goes to the wrong place or a regulatory invariant is violated. The complexity is absolutely essential here.

The key thing, if you want to do this sustainably, is that we *encapsulate* the complexity. The module that implements the type-level state machine typically has a small number of authors who understand it deeply and, ideally, a thorough test suite. The module that *uses* it has a surface API that looks like five normal functions with normal types. A product engineer on another team can call those functions without knowing or caring that underneath there is a small type-level theorem prover ensuring they cannot commit a transaction in the wrong state. The proof obligations are discharged inside the boundary, not leaked across it.

This is the same containment principle as purity, applied one level up. The complexity itself is fine, because it buys you something valuable. What causes problems is complexity that leaks across module boundaries into code maintained by people who did not sign up for it. We catch this in code review more than anywhere else. Someone opens a PR that touches a module they do not own, and the diff is full of type annotations they had to cargo-cult from a neighboring file just to make the compiler stop screaming. That is usually a sign the abstraction has become a form of friendly fire.

## Designing for Introspection

If reliability is about adaptive capacity, introspection is one of the ways you buy it. Operators cannot understand what they cannot see. Teams cannot adapt systems whose internals are opaque. Observability is not a garnish you sprinkle on at the end. It is part of the design surface of the software.

This matters a great deal in Haskell, because Haskell does not have monkey patching. You cannot, at runtime, reach into a library and replace its HTTP client with one that records timings, or swap its database calls for ones that emit OpenTelemetry spans. This is not unique to Haskell, of course. Rust has the same fundamental constraint: no monkey patching, no runtime method swizzling, no language-level "let me just interpose here real quick" escape hatch. The orphan rule even prevents you from adding trait implementations for types you do not own. The difference is that the Rust ecosystem has largely converged on the `tower` middleware pattern as the answer, while Haskell's ecosystem is still fragmented across several approaches. The constraint is the same; the question is whether your ecosystem gives you a conventional escape hatch or leaves every team to improvise one.

So, if a library exposes its functionality as a set of concrete top-level functions, your options for instrumenting it are limited to wrapping those functions in a new module and hoping nobody forgets to import yours instead of the original. Hope, it should be said, is not one of the stronger architectural patterns.

A point generalizeable for any language: **_You cannot operate what you cannot see._** If your traces have large opaque gaps because a library does something expensive and provides no hooks for you to measure it, you will find out what is slow when a customer complains, not when your dashboards tell you. If the dashboards tell you because the customer has already complained, then you are already paying interest on that design decision.

The solution I reach for most often is **records of functions**. Instead of exposing a module full of concrete functions, you expose a record whose fields are the functions. The caller can then wrap, instrument, mock, or replace any individual function without touching the rest. I wrote about this at length in [Embracing Flexibility in Haskell Libraries](https://www.iankduncan.com/articles/2024-01-26-records-of-effects), but the short version is:

```haskell
-- A concrete module gives you no leverage:
sendRequest :: Request -> IO Response

-- A record of functions gives you all of it:
data HttpClient = HttpClient
  { sendRequest :: Request -> IO Response
  , getManager  :: IO Manager
  }
```

With the record, you can wrap `sendRequest` with timing instrumentation and return a new `HttpClient`. You can inject faults for testing. You can swap the implementation for a mock. You can add retries, tracing, request rewriting, tenant-specific behavior, or whatever other cross-cutting concern production has discovered for you this quarter. All at runtime, without touching the library's source code. WAI got this right with `type Middleware = Application -> Application`: composable transformation of behavior is massively useful across a wide variety of systems, and systems are rarely kind enough to present all of their cross-cutting needs up front.

There is a nice algebraic property hiding in this pattern that deserves more attention. I promise this is not me sneaking category theory into the room in a fake moustache; it is genuinely practical. Middleware and interceptor types almost always admit `Semigroup` and `Monoid` instances. WAI's `Middleware` is `Application -> Application`, an endomorphism, and endomorphisms form a monoid under composition with `id` as the identity. A record of interceptor hooks, where each field is itself an endomorphism (or a continuation-passing function with a similar shape), gets a fieldwise `Semigroup` instance for free: `a <> b` composes each field independently, and `mempty` is the record where every field is the identity (pass the call through unchanged).

This turns composition from an engineering problem into almost a non-problem. You do not write bespoke plumbing to combine your tracing interceptor with your timeout interceptor with your task queue rewriting interceptor. You `mconcat` them:

```haskell
appTemporalInterceptors =
  mconcat
    [ retargetingInterceptor
    , otelInterceptor
    , sentryInterceptor
    , sqlApplicationNameInterceptor
    , loggingContextInterceptor
    , statementTimeoutInterceptor
    , teamNameInterceptor
    , clientExceptionInterceptor
    , workflowTypeNameInterceptor
    ]
```

Each interceptor is defined in isolation, in its own module, by someone who only needs to think about one concern. Individual interceptors are built from `mempty` with one or more fields overridden — everything else passes through. Composition is just `(<>)`. There is no hidden wiring. There is no coordination tax beyond agreeing on the shape of the hooks. Ordering is explicit in the list. New cross-cutting concerns are added by appending one more element; existing interceptors are never touched.

This is the `Monoid` pattern at its most practical. The abstraction does not exist for elegance, though elegance is a nice side effect. It exists because it makes the operational task of "combine N independent cross-cutting concerns" trivially correct by construction. If your middleware type supports `mempty` and `(<>)`, you can hand it to fifteen engineers and tell them each to write one piece, and the pieces will compose naturally.

Effect systems (`effectful`, `polysemy`, `fused-effects`, `cleff`, etc.) offer another route. You define an effect type describing the available operations, then provide interpreters that can be swapped at the call site. One for production, one for testing, one that wraps the production interpreter with tracing. The reinterpretation capabilities are particularly nice: intercept an effect, record a metric or inject a delay, and re-send it to the real handler. I wrote a guide to [using this for fault injection and observability](https://github.com/fused-effects/fused-effects/blob/main/docs/reinterpreting_effects.md) in the `fused-effects` documentation, and the core ideas transfer to any effect system. The tradeoff is that effect systems add machinery (type-level effect lists, handler stacks, sometimes gnarly type errors) where records of functions are simple enough that a new hire can understand the pattern in an afternoon. Both work. The important thing is that you pick one and use it consistently enough that your operability story is not itself an archaeological dig.

The `persistent` library is the canonical positive example, for me. Its `SqlBackend` type is a record of functions: `connPrepare`, `connInsertSql`, `connBegin`, `connCommit`, `connRollback`, and so on. When I implemented OpenTelemetry instrumentation for `persistent`, I could add tracing spans around every database operation by wrapping the relevant fields. No fork required. Almost no source changes. A few lines of code and we had full visibility into our database layer. That is what a good operational escape hatch looks like.

Libraries that do not do this are the ones that cost us the most operational pain. At Mercury, we very rarely use web API client bindings published on Hackage. This is not because they are necessarily poorly written (some are quite good). The problem is that we cannot trust code we cannot instrument. If a third-party binding makes HTTP calls through concrete functions, we have no way to add tracing, no way to inject timeouts tuned to our SLOs, no way to simulate partner outages in testing, and no way to explain the 400ms gap in a trace except by squinting at it and developing theories. So we write our own. More work upfront, but the clients we write are observable by construction, because we built them that way from the start.

There is also a milder ecosystem tax that people do not always mention. Some of the libraries you depend on are not abandoned, exactly; abandoned is too clean a word. They are still in service, structurally important to the ecosystem, maintained enough that the things continue to run, and not obviously owned by anyone whose full-time job is to improve them.

This has a few causes. Some of the people who built large parts of the modern Haskell ecosystem have, quite reasonably, gone on to other things. A handful of prolific authors of widely used libraries and frameworks seem to have shifted much their attention towards Rust, for example. Their libraries still work, still get support to a degree, and still have users who depend on them. But the stewardship model becomes less clear. Nobody is obviously in charge in the way a fast-moving production team would mean "in charge," and that creates understandable hesitation around making breaking changes, even when experience has taught us better ways to design these systems.

Diffuse stewardship has technical consequences. Old interfaces tend to persist. Libraries are often stable in the sense that they still function, but not especially alive in the sense of incorporating newer ideas about observability, boundary design, or operability. They may also lag behind advances you would like to take for granted. `http-client`, for example, still only supports HTTP/1.1 directly, which is perfectly serviceable right up until it isn't. You can work around this. But it means some parts of the ecosystem feel less like actively evolving products and more like public infrastructure: useful, durable, and mildly resistant to renovation.

This is not a complaint about volunteer maintainers. It is simply one of the ambient risks of building serious systems on a smaller ecosystem. The code may not be abandoned. It may just be standing there, somewhat stoically, waiting for someone with enough context and enough appetite for downstream breakage to renovate the station while the trains are still running.

If you are writing a Haskell library, leave escape hatches. Provide records of functions, or effect types, or callbacks, or *something* that lets the consumer of your code inject behavior without modifying it. Haskell's type system is wonderful for enforcing constraints. But it can also, if you are not careful, seal a system so tightly that the people who have to operate it cannot see inside. The perfect abstraction, if it is operationally opaque, simply cannot be used in production.

### A note to package authors

Please consider taking [`hs-opentelemetry-api`](https://hackage.haskell.org/package/hs-opentelemetry-api) as a dependency and adding instrumentation to your package. The API package is designed to be a good citizen: we are conservative about breaking changes, and it is completely inert when the OpenTelemetry SDK is not initialized by the application. Minimal performance overhead, and no unexpected exceptions or logging in your users' applications. In the interest of honesty, the dependency footprint is not yet as minimal as we want it to be, and we are actively working on that. But even a handful of spans around your key `IO` operations would make a real difference to anyone running your library in production.

And please, we beg you, do not log directly from library code. Do not import a logging framework and write to `stdout` or `stderr`. Provide a logging callback, or accept a logger as a parameter, or expose your log messages as a data type the caller can route wherever they want. The moment a library decides where logs go, it has made a decision about the operational environment that belongs to the application. At Mercury, we route logs through structured pipelines that feed into our observability stack. When a library writes directly to `stderr`, those messages bypass everything, and we have to write bespoke plumbing to account for them being dumped into what is otherwise a stream of JSON lines, which is a ridiculous way to spend anyone's finite time on Earth. A callback costs you almost nothing. Let the application decide how to be operated.

One more thing for library authors: consider exposing `.Internal` modules. This is somewhat controversial advice. Others have argued, reasonably, that `.Internal` modules create implicit API surface that users depend on, making it harder to refactor your library's internals without breaking downstream code. The concern is legitimate, but it rests on a degree of confidence that you have gotten your public API exactly right, that you have anticipated every use case your users will encounter. In my experience, that confidence is rarely justified.

A well-documented `.Internal` module with an explicit stability warning ("this module's API may change without notice between minor versions") is vastly preferable to the alternative, which is that your users fork your package, vendor it into their codebase, and now maintain a divergent copy that will never receive your upstream fixes. The Haskell ecosystem's best libraries already do this (`containers`, `text`, `unordered-containers`). It costs almost nothing to provide, and it turns "I need to fork this library" into "I need to import one unstable module," which is often a better situation for everyone involved.

One thing to watch out for here is that exposing internal modules may mean that you don't hear feedback from consumers, who have quietly popped the hood to get at what they need instead. Sometimes that is fine. Sometimes it means your public API's missing pieces remain missing because nobody bothers to tell you.

## What We Don't Put in the Types

Not all production Haskell is beautiful.

`unsafePerformIO` is used in libraries you depend on daily. The `bytestring` and `text` libraries use it internally to allocate mutable buffers, write into them, and freeze the result. The types say nothing about what happened during construction. The containment is maintained by convention, by careful reasoning, and by code review. You will encounter this in any production Haskell codebase. You will *write* this, when the type-safe alternative imposes an unacceptable performance or complexity cost. When you do, be honest about it. Document the invariants that the types are not checking. Be appropriately uncomfortable. Periodically revisit whether the type-safe alternative has become practical. Production Haskell is not the absence of compromise. It is the disciplined containment of compromise.

Then there is the testing situation. An alarming number of Haskell libraries on Hackage have few or no tests. The implicit (and sometimes disastrously explicit) argument is that if it compiles, it works. This is sometimes true for small pure code with tight types. It is certainly a common early Haskell experience: you make illegal states unrepresentable, the compiler catches a shocking amount, and you briefly begin to suspect you may have transcended ordinary software failure.

This feeling should not be trusted.

It is almost never true for IO-heavy code, for code that interacts with external systems, or for code where the interesting bugs live in the *semantics* rather than the *structure*. Types can tell you that a function returns an `Either ParseError Transaction`. They cannot tell you whether it parses the `amount` field as cents or dollars. They cannot tell you whether your partner API interprets an omitted field differently from a null field. They cannot tell you whether your retry logic doubles charges under exactly one timing window on leap day. In a production context, you are building on top of these libraries, and you are inheriting their untested assumptions. Worth knowing about, and worth compensating for with integration tests at your own layer.

Other ugly bits of Haskell crop up too: orphan instances, partial functions that you swear are total in context, `error` calls that you promise are unreachable, awkward FFI wrappers, hand-rolled exceptions hierarchy shennanigans, places where the ideal abstraction lost a knife fight with the real one. These accumulate. Operating Haskell in production is the act of tending a living system, and living systems accrete compromises. What you can do is enforce discipline in code review, by way of documentation and examples and tests, so that you know where every compromise is, why it was made, and what would break if it were removed. The goal is not moral purity. The goal is to avoid discovering, during an incident, that half the system's assumptions existed only as oral tradition.

## Why bother with Haskell at all?

There is a question that comes up whenever someone considers Haskell for a production system: **is it worth it?**

Not on day one.

On day one, you are slower. The current Haskell ecosystem simply does not have the power of Next.js or Rails to dump a batteries-included hot-reloading development environment onto your desk. Projects like IHP may eventually get there, but inevitably, the library you need does not exist, or exists but is maintained by one person in their spare time. The error messages occasionally read like the screed of a madman who knows exactly what is wrong and deeply resents having to explain it to you.

The hiring concern, though, is overstated. Our CTO Max Tagher has said publicly that backend Haskell engineer is the *easiest* role to hire for in all of Mercury.[^max] There is more demand for Haskell jobs than the market provides, which inverts the usual recruiting dynamic. Interest in Haskell acts as a decent proxy for baseline developer quality (Paul Graham's [Python Paradox](http://www.paulgraham.com/pypar.html), applied one language further up the abstraction ladder), and it attracts people who are genuinely excited about the work rather than indifferent to it. We hire people with deep Haskell experience and people with no Haskell experience at all, and we have a training program that gets the latter group productive in six to eight weeks. The hiring pool problem is real if you need a hundred Haskell experts tomorrow. It is much less real if you are willing to hire good generalists and invest in teaching them.

The hiring risk that nobody warns you about is not the size of the pool. It is the *disposition* of the pool.

Haskell attracts idealists. This is mostly a strength: you get people who care about correctness, who think carefully about abstractions, who read papers for fun, and who are willing to interrogate assumptions other ecosystems accepted years ago out of pure exhaustion. But idealism, left unchecked, becomes a production liability. The engineer who wants to rewrite the database layer using a novel type-level encoding of relational algebra is not helping you ship features. The engineer who refuses to merge code that uses `String` instead of `Text` in a throwaway script is not helping you hit a deadline. The engineer who treats every design discussion as an opportunity to advocate for a total rewrite in the style of a paper they read last week is, however brilliant, making the team slower.

You have to actively cultivate a culture of pragmatism. Haskell gives you powerful tools. Using all of them all the time is not pragmatism; it is self-indulgence. The type system is a power tool, not a religion, and production systems are not the place to treat every feature as an excuse to invent a novel mechanism to solve a problem that already has a perfectly good solution.

The return comes later, but it does come. You see it when a refactor that would take weeks in a dynamically typed codebase takes hours, because the compiler threads the change through every call site and tells you exactly what you missed. There is a particular pleasure to mechanical refactoring in Haskell that is hard to convey until you have experienced it: you change a type, and the compiler hands you an exhaustive list of everywhere that needs to adapt. You work through it methodically — no searching, no guessing, no wondering whether you missed a call site in a module you forgot existed — and when it compiles, you are done. It is one of the few programming experiences that feels genuinely *complete* rather than *probably complete*. You see it when a new hire reads a module's type signatures and understands its contracts without asking anyone, which in a company that has grown from fifty engineers to several hundred is not a convenience but a survival tactic. You see it when a production incident does *not* happen because an impossible state was actually impossible, not just considered unlikely by someone in a hurry.

We have found that the investment pays off on the order of months, not years. This is particularly true in financial services, where the cost of a data integrity bug is measured not in user complaints but in regulatory findings and in other people's money. The type system does not eliminate these risks. But it gives you the tools to make them harder to introduce by accident, and in a fast-growing codebase, "by accident" is where most of the risk lives.

We have been running Haskell in production for years now, at a scale and in a domain where getting things wrong carries tremendous consequences, both for our customers and for us. It has not been painless. Some of the pain was the language's fault, some was the ecosystem's, and a good deal of it was our own. But the system works, and it keeps working as the team grows and changes around it, and that is not something I can say about every technology choice I have made in my career.

When I was sixteen, what captivated me about Haskell was the promise that some bugs could be made impossible. I still love that premise, but what I understand now is that the deeper production value is broader and, in a way, more interesting. Haskell lets you preserve hard-won operational knowledge after the people who discovered it have gone home, changed teams, or left the company. It lets you draw hard boundaries around dangerous machinery. It lets you make the safe path the easy path. In a system that has to keep working while the organization around it changes, that surpasses academic elegance and provides solid engineering leverage.

If you are considering Haskell for production, I hope this has given you a realistic picture of what that looks like: not a silver bullet, not a moral crusade, but a genuinely powerful set of tools even when wielded by a team that with a broad range of Haskell expertise.

---

*Ian Duncan is an engineer on the Stability team at [Mercury](https://mercury.com), where he works on reliability infrastructure, including Mercury's Temporal SDK, OpenTelemetry instrumentation, and making sure production doesn't keel over and die when he isn't looking. He has been writing in assorted functional programming languages professionally since 2014. He lives in The Hague with his family and has strong opinions about coffee, power metal, and system architecture. You can find him on [Bluesky](https://bsky.app/profile/iankduncan.com) and at [iankduncan.com](https://iankduncan.com).*

---

[^svb]: In March 2023, Silicon Valley Bank collapsed in what turned out to be the second-largest bank failure in U.S. history. Mercury gained over 8,700 new customers and $2 billion in deposits in five days, because nothing stress-tests your infrastructure quite like the entire startup ecosystem simultaneously deciding to move their money to you. See ["SVB's collapse drove 26K customers to Mercury in 4 months"](https://techcrunch.com/2023/07/07/mercury-says-it-gained-nearly-26k-new-customers-in-the-four-months-after-svbs-collapse/) (TechCrunch, 2023). 95% of them stayed.

[^zalora]: It's a thing that I've seen happen to at least three or four companies at this point!

[^safety-one]: The distinction between studying failure and studying success in complex systems is the central thesis of Erik Hollnagel's [*Safety-I and Safety-II*](https://www.routledge.com/Safety-I-and-Safety-II/Hollnagel/p/book/9781472423085) (Ashgate, 2014). The traditional approach ("Safety-I") focuses on preventing things from going wrong. The alternative ("Safety-II") focuses on understanding why things ordinarily go *right* and ensuring the conditions for success are maintained.

[^safety-two]: "Adaptive capacity" as the foundation of resilient systems comes from the resilience engineering literature, particularly David Woods' work. See [*Resilience Engineering: Concepts and Precepts*](https://www.routledge.com/Resilience-Engineering-Concepts-and-Precepts/Woods-Hollnagel-Leveson-Cook/p/book/9780754649045) (Ashgate, 2006) and Woods' ["Four Concepts for Resilience and the Implications for the Future of Resilience Engineering"](https://doi.org/10.1016/j.ress.2018.01.004) (*Reliability Engineering & System Safety*, 2015).

[^patio11]: Patrick McKenzie (patio11), ["What Working At Stripe Has Been Like"](https://www.kalzumeus.com/2020/10/09/four-years-at-stripe/) (2020). At a company growing fast enough, the majority of your colleagues will *always* be new, which means anything you know that isn't written down in code or docs is effectively secret knowledge.

[^prr]: Our process is informed by Google's production readiness review model, described in Betsy Beyer et al.'s [*Site Reliability Engineering*](https://sre.google/sre-book/table-of-contents/) (O'Reilly, 2016), Chapters 32-33. Our adaptation emphasizes collaborative review over gatekeeping.

[^slack]: Incidentally, Slack at Mercury now has a message lifespan of like a couple years or something before it gets deleted. So, you eventually reach a point at large orgs where deep lore begins to evaporate entirely unless it is written down somewhere more permanent.

[^max]: Max Tagher, ["Haskell in Production: Mercury"](https://serokell.io/blog/haskell-in-production-mercury) (Serokell, 2022) and ["Haskell in Mercury"](https://serokell.io/blog/haskell-mercury-functionalfutures) (Functional Futures, 2024).


---

*\*Mercury is a fintech company, not an FDIC-insured bank. Banking services provided through Choice Financial Group and Column N.A., Members FDIC. (At the time of writing.)*
