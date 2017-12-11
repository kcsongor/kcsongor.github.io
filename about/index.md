---
layout: page
title: About me
tags: [about]
modified: 2017-9-24
comments: true
image:
  feature:
---

<section id="table-of-contents" class="toc">
  <header>
    <h3>Overview</h3>
  </header>
<div id="drawer" markdown="1">
*  Auto generated table of contents
{:toc}
</div>
</section><!-- /#table-of-contents -->

I'm a final year undergraduate at Imperial College London, studying computer
science. I have a strong interest in functional programming, Haskell in
particular. My main focus is using advanced type system features to derive
programs from statically available information. In my spare time, I enjoy
playing with interactive theorem provers, most notably Coq.

### Talks/publications
- **Deriving lenses using Generics** (conference paper)
    (Csongor Kiss, Matthew Pickering, Toby Shaw) - IFL 2017, Bristol, UK
- **VoxelCAD, a collaborative voxel-based CAD tool (demo)** [link](http://functional-art.org/2016/voxelcad.html)
    (Csongor Kiss, Toby Shaw) - FARM 2016, Nara, Japan
- **Generating elm data types using GHC Generics** (lightning talk) - Haskell eXchange 2016, London, UK

### Teaching
I've given 3 guest lectures in Advanced Functional Programming using Haskell in
the Department of Computing at Imperial College London.
- **GADTs** - November, 2017
- **Monadic IO** - November, 2017
- **Lenses** - December, 2017

### Projects
Some of my more interesting projects

#### generic-lens (2017)
([github](https://github.com/kcsongor/generic-lens)) ([blog post](/generic-lens/))

A Haskell library for deriving lenses and prisms using GHC8's `Generic` machinery.

#### PureScript safe printf (2017)
([github](https://github.com/kcsongor/purescript-safe-printf)) ([blog post](/purescript-safe-printf/))

A type-safe `printf` interface for PureScript showcasing a type-level parser
that uses my `ConsSymbol` class solved by the compiler.

#### Type-level register machine (2016)
([github](https://github.com/kcsongor/register-machine-type))

A type-level register machine, as a proof of concept for showing that Haskell's
type system is indeed Turing complete.

#### Type-level regex (2016)
([github](https://github.com/kcsongor/regex-type))

Type-level implementation of a regular expression to nondeterministic finite
automaton compiler, and matcher.

#### Haskell compiler (2015)
I wrote a compiler for a subset of Haskell (notably, no type classes), with
some extensions (mixfix syntax by dynamically generating context-free grammars,
like in Agda).

#### Doom on Raspberry Pi (2015)
([github](https://github.com/kcsongor/arm-doom))

Doom-like game engine running on a 1st generation Raspberry Pi, written in 100%
pure ARM assembly, with no operating system.
Written with a group of 4, in about 3 weeks.
{% include doom.html %}

#### Others
My other projects can be found on my [github](https://github.com/kcsongor)

### Work experience
- Habito -- 2017, London, UK
- GSA Capital -- 2016, London, UK
- Netcraft -- 2015, Bath, UK
