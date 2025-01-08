+++
title = "Haskell Foundation Stability Working Group"
date = 2025-01-07
[taxonomies]
authors = ["Trevis Elser"]
categories = ["Haskell Foundation"]
tags = ["Community", "Stability"]
+++

What is stability? What has the Haskell Foundation Stability Working Group been working on?

<!-- more -->

## Who am I?

The health of the Haskell ecosystem is very important to me. Most of my professional career to this point has involved using Haskell. As such it is in my own best interest to help the community to be healthy and grow. That way I can continue to work in the language and ecosystem! Even prior to my professional career, I used Haskell as an undergraduate. Despite it not being a part of the curriculum and being met with skepticism from several advisors that I could ever find work in Haskell.

Working as first a member and now chair of the Haskell Foundation Stability Working Group has been enlightening. Over the last few years two things have become very apparent. First, that there is a real appetite for movement from practitioners on some notion of stability. Second, getting things done in this area takes much more time and effort than it would seem.

With the appetite from practitioners and effort in mind, let me describe my views on stability, the operating goals of the Stability Working Group, who the group is, what we have been working on, and how you can help.

## What is stability? What is it not?

Stability is often used with different definitions. As a result discussions can be confusing, unintentionally misrepresentative, or worse. The concern here is about _ecosystem stability_. So let us discuss _ecosystem_ and _stability_ independently. Then we can move on to putting the words back together.

By ecosystem we mean the broadest most inclusive set possible. This includes compiler and tooling implementors, industrial users across the spectrums of experience and enthusiasm, language designers, library authors and maintainers, researchers, students, anyone else working with the language, and anyone who works with the aforementioned. The needs of those groups are quite diverse and can often be in tension. So one of the first things to keep in mind for stability is how to balance those needs. It is simply required to be empathetic to all of those groups.

Recognizing this diversity means realizing where there are gaps in representation in the ecosystem today. Further we must remain cognizant that many of those groups simply will not increase their engagement with the rest of the ecosystem. How then can we possibly be aware of issues or concerns if those impacted are not engaged? There must be a bridge to them. Anyone involved in being that bridge then must navigate conversations to represent needs that might be, irrelevant to them or even in tension with their own.

Now with the ecosystem in mind, what exactly is stability?

For many they will read that word and think immutability. We are talking about the Haskell ecosystem after all. However, stability here does not mean a complete lack of change. To forego change entirely would mean to not be able to incorporate new ideas or react to the rest of the world changing. If this was the case Haskell would never have grown to include a huge number of things, including the IO monad! Interaction with he rest of the world will continue to require changes, for example the first version of The Unicode Standard was published after Haskell 1.0!

So we accept that not only will change happen, it must. This leads to how I would describe stability: the ability to react to change in an informed and graceful way. With change as a given, we must be able to react to it. But the other parts are also extremely important. Change can be extremely disruptive. In the ecosystem, change might mean:
 - industrial users need to spend time away from their employer's goals
 - library and tooling maintainers have to adapt and support multiple paths, taking away from feature development
 - academic materials might need to be rewritten
 - any other number of issues that are difficult to predict

But we just said that change is going to happen! As such, _ecosystem stability_ is managing the *impact* of change as felt by all members described above . In order to reduce the negative impacts of change to the ecosystem there must be a graceful handling path, not a lack of change entirely. For example, deprecating something before removal. Another important factor here is the timing. Being predictable in change management means that the various groups can schedule work rather than being interrupted. This is why it is critical to be able to handle change as gracefully as possible.

## The Stability Working Group

Who is the SWG? Membership and meeting attendance is fluid, but we have representation from a cross section of the ecosystem. That includes people involved in or with background in language design, compiler, tooling and library maintenance, industrial usage, academia and consulting.

Given that, we promote tools, procedures, and general culture to effectively manage change. In order to manage this we, as a community must be able to identify when change happens, plan for upcoming changes, and minimize the breadth and depth of the impact of any given change. Finally we aim to bring representation for the sections of the ecosystem that are generally less present.

## What We Have Been Working On

The SWG has worked on several projects, some of which continue to this day.

We worked on a [review](https://jappie.me/analyzing-haskell-stability.html) of breakages tracked by the GHC team. This was illuminating to highlight and enumerate a high level set of changes. There is still room to do more extensive analysis here as well.

Another project we have been working on is defining a framework for classifying GHC language extensions. This has been a long process. But ultimately is the type of communication enhancing improvement that is needed for predictability.

More concretely we have defined a set of compiler features to help with stability. These were ideas that already existed in some form. Examples that we were able to bring to attention and have since landed in GHC include warning categories and deprecation of exports. Those both allow for more communication from compiler and library authors to users.

Finally, we find ourselves with a recurrent item. That is to be generally facilitators of communication. This means bringing groups together, highlighting issues, and generally fostering discussion.


## How you can help

We have been working on many things, but we can still use help! Hopefully I have intrigued, inspired, or just otherwise cajoled you into a desire to contribute. There are several ways to help. The lowest effort is to contribute to Haskell Foundation. Another is to reach out! As I said above we have several projects that could use someone to champion. The SWG often hosts guests at meetings to discuss particular issues or concerns. You do not have to commit ongoing time in order to contribute!

With your help we can continue to strengthen and grow the ecosystem!

### Contact

You can reach me via email `trevis <at> flipstone <dot> com`, or less reliably on matrix and discourse. The SWG as a whole can be reached at [GitHub](https://github.com/haskellfoundation/stability) and via email `stability <at> haskell <dot> foundation`.

### Thank you

Finally, if you made it this far, then thank you for reading. Additionally I want to extend great thanks to the Haskell Foundation, the other members of the SWG and everyone who has made Haskell what it is today.
