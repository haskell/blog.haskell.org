# Haskell Blog

## Local installation

The blog is made with [Zola] and based on the [Abridge theme] that is included
as a Git [submodule][git-submodule].

You will need:
  * The `zola` binary v0.19.1 or higher

Run `zola serve` in order to serve the website and automatically render it when files change.

```
$ git clone git@github.com:haskell/blog.haskell.org.git --recurse-submodules
$ cd blog.haskell.org
$ zola serve
```

## Contribute content

### Blog post structure

#### Name

Blog posts are located in the `content` directory, as markdown files. The files themselves contain
their title in a "slug" format (alphanumeric characters, words separated by a hyphen, all lowercase):

```
documentation-best-practices.md
```

#### Front-matter

The file itself must contain a front-matter in TOML format that has the following properties:

```
+++
title = "Title of the post"
date = YYYY-MM-DD
[taxonomies]
authors = ["Author's Name"] # The author's name. Will be indexed.
categories = ["Haddock"] # A minima should be the name of the team ("Haddock", "HLS", "Cabal"). Will be indexed.
tags = ["Practices"] # Something more precise like "Release", "Practice", "JavaScript", can be aded. Will be indexed.
+++
```

#### Summary

The eye-catcher that you wish to show on the front-page of the blog is the first paragraph. 
You can manually delimit where it ends by inserting `<!-- more -->` on a newline between this paragraph and the rest of the content.

Otherwise, in the absence of this comment, the first 150 characters will be used by Zola

#### Images and other files

If you want to add images to your blog post, create a folder named after the blog post, and write the content of the post to an `index.md`
file within it. Put your associated files in the same directory.

For instance:

```
documentation-best-practices
├── flow-of-documentation.png
└── index.md
```


[Zola]: https://www.getzola.org/
[Abridge theme]: https://abridge.pages.dev/overview-abridge/
[git-submodule]: https://git-scm.com/docs/git-submodule