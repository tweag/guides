# Gamazeps' opinionated guide to Haskell

This guide is addressed to regular programmers interested in learning Haskell.

It presuposes no previous knowledge of Haskell, lambda calculus, monads or
endofunctors in co-De Bruijn space (this one doesn't exist, but with functional programmers
you're never too careful).

The point here is to get you up and running so that you become capable of reading
Haskell programs.

Note: This is a very biased guide based on my experience of learning Haskell with
a background in C, Rust and python; your exeperience may differ from mine.

This is also meant to be a living document updated as I become better at Haskell,
thus new ressources to learn about more advanced subject will come as I encounter them.

## Know your tools

### Stack and Cabal

First things first you will want to know stack and cabal, they are the most important
parts of the Haskell dev environment and you can easily loose a lot of time because
of them.

I found ["What I wish I knew when learning Haskell"](http://dev.stephendiehl.com/hask/#cabal)
to be quite helpful on this area.

You should now be able to have a `ghc` when needed and to create projects.

### GHCi

GHCi is a Haskell interpreter, this is very useful for testing when you are a beginner.

The books below all have decent explanantions on it.

### Hackage

This is also on "What I wish I knew when learning Haskell", this is a repository
of most of the Haskell library and hosts their documentation

### Hoogle

[hoogle](https://www.Haskell.org/hoogle/) is a nice tool for searching functions by name or type signature. 
For example if you want to find a function to convert a String into a Bytestring
you acn simply search "String -> Bytestring" on it.

## What not to do

Here are a list of things that I advice you *not* to do:

- Do not try to understand what Monads are, you will understand them as you code in Haskell
and this is fine, trying to learn then alone will damage your brain, give you a false understanding of them,
waste your time and disgust you with Haskell
- Do not try to understand lenses, they are like monads, but worse.
- If you read an article about something in Haskell and it starts to mention cabalistic
names (such as profunctors), run ! I'm not saying that this is useless, but if you
are a beginner you will just be confused and waste your time (and feel like Haskellers
are a bunch of pretentious pricks).

## Good ressources

The folowing parts are supposed to be self contained references, I would advise you
to read a chapter of each and chose the one that best fits how you like to learn.

Gentle introduction to Haskell is pretty short and to the point so I would advise
to read this one anyway (at least that is my experience).

### The Haskell book

This is considered as the de facto first tutorial, and recommended on the official website.

- pros :
    - very easy to understand
    - assumes no previous programming experience
    - delves into most of the common typeclasses
    - gives a broad vision of Haskell, with testing and toolings chapters
- cons :
    - extremely long (about 1200 pages)
    - not very dense
    - assumes no previous knowledge, thus if you have programmed before you may find it boring (I did).
    - not a lot of practical applications (i.e. focuses more on Haskell itself than *building software* in Haskell)
    - does not go into intermediate topics (monad transformers, of which tweag people are quite fond)

### Gentle Introduction to Haskell

This is a pretty old guide, some parts are deprecated, but let's face it you're a
beginner, it's not a big issue.

- pros :
    - short
    - very dense
- cons :
    - you may have to read the same parts multiple times to get them
    - a bit outdated
    - just an introduction

### Real world Haskell

I have not read this one fully, so this may not be a fair review of it.

- pros :
    - applied, this will actually have you build things in Haskell
    - medium length (~300 pages)
    - paper version
    - written by one of the big names in Haskell (Bryan O'Sullivan),
      who teaches Haskell at Stanford and Facebook.
- cons :
    - a bit outdated

### Learn you a Haskell

I only read half of this one too, so this may not be a fair portrayal either.

- pros :
    - easy to read
    - short (<300 pages)
- cons :
    - does not go into intermediate Haskell

## Good references

The following links are not self contained and should be considered to improve your knowledge on
specific areas once you are done with one of the tutorials

### What I wish I knew when learning Haskell

[link](http://dev.stephendiehl.com/hask/)

The "Basic" part, I would highly advise you to read, this will cover most of the common
mistakes newcomers will encounter, and tools you wished you knew existed.

### Typeclassopedia

[link](https://wiki.Haskell.org/Typeclassopedia)

A bestiary of the most common typeclasses, this is a nice addition for the previous books
and a good starting place when you encounter a new typeclass.
