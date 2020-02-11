---
layout: post
title: Announcing generic-optics (& generic-lens 2.0.0.0)
excerpt: generic-lens gets a new interface.
modified: 2020-02-11
tags: [haskell, generic-lens, generics, lens, optics]
comments: true
---

{% include _toc.html %}

I'm happy to announce a new library,
[generic-optics](https://hackage.haskell.org/package/generic-optics),
accompanied by version 2.0.0.0 of [generic-lens](https://hackage.haskell.org/package/generic-lens).

## Background

A few months ago, the folks at Well-Typed [announced the `optics`
library](https://www.well-typed.com/blog/2019/09/announcing-the-optics-library/),
which aims to improve on the user experience compared to the `lens` library.
Oleg Grenrus has written an excellent [migration guide](http://oleg.fi/gists/posts/2020-01-25-case-study-migration-from-lens-to-optics.html)
from `lens` to `optics`, so please have a look there for some more background.

`generic-optics` is essentially a port of `generic-lens` that is
compatible with `optics`, and is designed to be a drop-in replacement
for `generic-lens`. This means that if you're already using `generic-lens`
with `lens` and decide to migrate to `optics`, you should be able to replace the
`generic-lens` dependency with `generic-optics` and expect things to just work.

## Examples

To explain why I'm so excited about `optics`, I'm going to
compare a real-life workflow between `generic-lens` and `generic-optics`.

First, language pragmas and imports:

{% highlight haskell %}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Generics.Product
import GHC.Generics
{% endhighlight %}

Note that the module `Data.Generics.Product` is shared between
`generic-lens` and `generic-optics`.

When using `generic-lens` with the `lens` library, we would import

{% highlight haskell %}
import Control.Lens
{% endhighlight %}

When using `generic-optics` with `optics`, the import becomes

{% highlight haskell %}
import Optics.Core
{% endhighlight %}

Now we define a simple record:

{% highlight haskell %}
data MyRecord = MyRecord { a :: Int, b :: Int, c :: (Bool, Int) }
  deriving (Generic, Show)

myRecord1 :: MyRecord
myRecord1 = MyRecord 0 1 (False, 2)
{% endhighlight %}

With either library, we can view the `a` field using
the `field` lens:

{% highlight text %}
lens|optics> myRecord1 ^. field @"a"
0
{% endhighlight %}

If we ask what the type of `field @"a"` is in GHCi, we already see
the advantage of `optics`'s opaque representation.

Compare

{% highlight text %}
lens> :t field @"a"
field @"a"
  :: (HasField "a" s t a b, Functor f) => (a -> f b) -> s -> f t
{% endhighlight %}

with

{% highlight text %}
optics> :t field @"a"
field @"a" :: HasField "a" s t a b => Lens s t a b
{% endhighlight %}

Now let us use the `typed` lens, which performs a type-directed lookup in
a product type, as long as there is a unique field with that type:

{% highlight text %}
lens|optics> myRecord1 ^. typed @(Bool, Int)
(False,2)
{% endhighlight %}

When the type of the field is not unique (such as if we tried to retrieve a field
of type `Int`), both `generic-optics` and `generic-lens` provides a helpful type error:

{% highlight text %}
lens|optics> myRecord1 ^. typed @Int

<interactive> error:
    • The type MyRecord contains multiple values of type Int.
      The choice of value is thus ambiguous. The offending constructors are:
      • MyRecord
{% endhighlight %}

For situation likes this, both libraries provide a traversal called
`types` that focuses on all values of the given type.

Let's see what happens if we replace `typed` with `types` in the above
example when using `lens`:

{% highlight text %}
lens> myRecord1 ^. types @Int

<interactive>:43:14-23: error:
    • No instance for (Monoid Int) arising from a use of ‘types’
{% endhighlight %}

This error is rather puzzling. Unless we know what's going on under
the hood, it's not obvious where the `Monoid` constraint is coming from.

Compare this with `generic-optics`:

{% highlight text %}
optics> myRecord1 ^. types @Int

<interactive>:32:1-23: error:
    • A_Traversal cannot be used as A_Getter
{% endhighlight %}

Right! `types @Int` is a traversal, but `^.` takes a getter!
Arguably this is a more helpful message. Consulting the documentation
of `optics`, we find the combinator we're looking for: `^..`, which returns
all the values focused on by a traversal:

{% highlight text %}
lens|optics> myRecord1 ^.. types @Int
[0,1,2]
{% endhighlight %}

This now of course works in both libraries.

To summarise, using the two libraries should be nearly identical as
long as everything goes well and we're not hitting type errors.
Where `generic-optics` (but really, `optics` itself) shines is when things
do not go all that well, in which case the resulting error messages are a lot more
comprehensible.

## Differences

The above was just to give a little taste of using
`generic-optics`. The interface of `generic-optics` is
intended to be largely identical to that of `generic-lens`.

### Labels

At the time of writing, the main difference is the support for overloaded
labels in `generic-lens`, which allows writing

{% highlight text %}
lens> import Data.Generics.Labels ()
lens> myRecord1 ^. #a
0
{% endhighlight %}

I intend to add support for this for `generic-optics` too, but it
isn't implemented yet.

## Changes in generic-lens

To support this new interface, `generic-lens` itself has undergone a major
reorganisation. I thought this was a good opportunity to clean some things
up and change the interface at places, which ultimately resulted in a new major
version bump.

Most notably, GHC versions below 8.4 are no longer
supported. `generic-lens` (and `generic-optics` too) promises good
performance by making sure that the generic overhead is eliminated at
compile time.  Doing so requires really careful coding practices, and
GHC's optimiser changes between every version, which meant that
certain tricks that worked for 8.2 didn't work for 8.6 and vice
versa. The result was horrible CPP macros to enable certain hacks on
certain versions of GHC. In the end, I decided it wasn't worth the
effort to maintain these hacks for older versions of the compiler.

I intend to write a blog post in the near future describing some of these
hacks, as they are quite interesting and potentially educational.

For a more comprehensive list of changes, refer to the
[changelog](https://github.com/kcsongor/generic-lens/blob/master/generic-lens/ChangeLog.md).


## Conclusion

Thanks for reading this blog post, and I'm hope you're as excited
about `generic-optics` as I am! Since this release required a major refactoring
and moving things around, it is possible that some documentation is out of date, or
certain functions are not exported from where you would expect. If you find anything
that looks off, please either open a pull request or let me know on the issue tracker!

Finally, if you find `generic-lens` or `generic-optics` useful,
consider [buying me a coffee](https://github.com/sponsors/kcsongor)!
