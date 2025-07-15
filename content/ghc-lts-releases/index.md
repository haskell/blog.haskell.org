+++
title = "GHC LTS Releases"
date = 2025-07-07
[taxonomies]
authors = ["Andreas Klebinger"]
categories = ["GHC"]
tags = ["Release"]
+++

# GHC will start maintaining an LTS release/branch in the near future

A release being designated LTS (Long Term Support) in this case means we plan
to support it over a longer timeframe than usual.

Concretely the plan is to provide updates for a LTS releases for *at least* two
years. Most likely we will support LTS releases for even longer than that,
aiming for a support window of three years currently.

During this time we will be providing minor releases fixing bugs as with any
other release. The main difference being that we will do so for a longer period
of time.

There are no plans to backport any new features to LTS releases after their
initial release.

In terms of frequency of LTS releases we plan to have an overlap between LTS
support windows of different LTS series of six months.

A potential timeline might then look like this:

```
2025 Aug - LTS 9.14 released
2028 Spring - LTS 9.22 released
2028 Summer - LTS 9.14.X - last 9.14 point release
2031 Spring - LTS 9.X released
2031 Summer - Last 9.22 point release
…
```

# Non-LTS releases

GHC will continue to release new major non-lts releases on a ~6 Month cadence.
We expect to cut back on the lifetime of these releases slightly, dedicating
the resources freed up this way to enable a longer support window for the LTS
releases.

# Why LTS releases?

In practice some releases always saw more adoption than others by users. The
GHC Team has not been blind to this fact and has at times informally extended
the life of a certain release based on this as well.

This resulted in a sort of informal "post-hoc LTS" status of releases. At times
with support windows not much shorter than our proposed minimum of two years.

This worked reasonable well for people who were confident to stay on a fairly
old release, only upgrading to a newer "post-hoc LTS" once the dust settled. It
also worked out for those who picked one of those "post-hoc LTS" releases by
happenstance before it was clear the release would end up as "post-hoc LTS".

However users who adopted major releases which did not end up as "post-hoc LTS"
often had to choose between upgrading earlier than expected, or risk running
into a show stopping bug after the support window of the release had already
ended. Similarly much of this was based on informal community sentiment and
rarely written down explicitly. Making this information hard to access for
members not deeply involved in the day to day of the haskell community.

By designating a major release as LTS ahead of time we hope that users can make
a informed decision about which GHC version they pick. Making it clear what the
tradeoffs will be. With a clear choice between a longer support window or the
newest features.

# Why not make post-hoc LTS releases official instead?

This is a question that has come up a lot in discussion. The major downsides of
this are a lack of predictability, and that a lot of time might be lost between
the initial release and any such decision. If we declare a release as LTS 9
months after its .1 release we essentially shaved off months from the LTS
support window.

On the flip side if we announce it ahead of time everyone knows that a given
release will be the new LTS. So the hope is that this encourages more and
quicker support for the release by the community. Hopefully compressing the
timeline of bug fixing, testing and eventual widespread adoption.

Overall I'm hopeful that LTS releases being explicit will remove a lot of
ambiguity around GHC versions. And while the guaranteed LTS support window
might not be as long as one might hope having LTS releases with longer
guaranteed support window should still be helpful to people working on long
running haskell projects.

# Next steps

The first LTS release will be GHC 9.14, which will be released this summer!

