---
layout: post
title: "Opaque constraint synonyms"
excerpt: "Hiding internal details of constraint synonyms."
tags: [haskell]
comments: true
---

{% include _toc.html %}

The list of type class constraints in a function signature can
sometimes get out of hand. In these situations, we can introduce a
type synonym (thanks to `ConstraintKinds`) to avoid repetition.

Say we want to group together the `Show` and `Read` constraints:

{% highlight haskell %}
type Serialise a = (Show a, Read a)
{% endhighlight %}

Now `Serialise a` can be used anywhere where we require both constraints:

{% highlight haskell %}
roundtrip :: Serialise a => a -> a
roundtrip = read . show
{% endhighlight %}

This is great, because it means we no longer have to spell out `(Show a, Read a)`
whenever we need both, and we also improved readability, because
`Serialise` conveys some additional domain-specific meaning.

There's a problem with this, however. If we ask GHCi about the type of
`roundtrip`:

{% highlight text %}
>>> :t roundtrip
roundtrip :: (Show a, Read a) => a -> a
{% endhighlight %}

it will eagerly expand the type synonym, removing all traces of
`Serialise`. Of course this is a well known problem of type
synonyms, so we generally avoid them in favour of `newtype`s.

But there's no analogous construction for constraints. Or is there?

## Constraints newtypes (kind of)

To begin, we're going to drop the type synonym in favour of the
"constraint synonym" technique, which is essentially the following:

{% highlight haskell %}
class (Show a, Read a) => Serialise a
instance (Show a, Read a) => Serialise a
{% endhighlight %}

In other words, we introduce a new type class with the required
superclass constraints, and a single catchall instance.

So far, the status quo hasn't improved though. GHC is quite renitent:
{% highlight text %}
>>> :t roundtrip
roundtrip :: (Show a, Read a) => a -> a
{% endhighlight %}

This happens because the compiler sees that there's only one matching
instance, so it's safe to pick that one, and it will do so. This point
is the important one: that there's only one instance. So, if we could somehow
trick GHC into thinking that there are other options, then maybe it wouldn't
be so eager to expand our constraints.

So, we create an empty data type, only to be used internally:
{% highlight haskell %}
data Opaque
{% endhighlight %}

Next, we satisfy the superclass constraints

{% highlight haskell %}
instance Read Opaque where
  readsPrec _ _ = []

instance Show Opaque where
  showsPrec _ = \case {}
{% endhighlight %}

Note that these two instances only exist so that the constraint is
satisfied, but since the type is internal, the actual functions are
never going to be invoked.

Finally, the key ingredient: an overlapping instance for `Serialise Opaque`.

{% highlight haskell %}
instance {-# OVERLAPPING #-} Serialise Opaque
{% endhighlight %}

Now, every time GHC sees a `Serialise a` constraint, it will no longer
be able to pick the catchall instance, in case `a` gets instantiated
to `Opaque` later. Of course, this won't happen, because we don't
export `Opaque`, but it's good enough for GHC.

{% highlight text %}
>>> :t roundtrip
roundtrip :: Serialise a => a -> a
{% endhighlight %}

## A real world example

You might say that the `(Show a, Read a)` example is perhaps overly
simplistic. I came up with this technique to solve a very real problem
in the [generic-lens](http://hackage.haskell.org/package/generic-lens-1.2.0.0) library.
This problem shows up at many places in the library, but to pick one, consider the `AsType`
class:

{% highlight haskell %}
class AsType a s where
  _Typed :: Prism' s a
{% endhighlight %}

The exact meaning of the class is irrelevant here (but see the
[documentation](http://hackage.haskell.org/package/generic-lens-1.2.0.0/docs/Data-Generics-Sum-Typed.html#v:_Typed) if you're interested). What
matters is that there's a catchall instance defined for all types
(using `GHC.Generics`), which in turn requires a large number of other constraints and predicates
to hold. Since this catchall instance is the only one defined by the library, asking for the
type of `_Typed` in GHCi eagerly expands the constraints to those of the instance.

{% highlight haskell %}
>>> :t _Typed
_Typed
  :: (ErrorUnlessOne
        a s (CollectPartialType (TupleToList a) (Rep s)),
      Defined (Rep s) (TypeError ...) (() :: Constraint), Generic s,
      ListTuple a (TupleToList a), GAsType (Rep s) (TupleToList a),
      Data.Profunctor.Choice.Choice p, Applicative f) =>
     p a (f a) -> p s (f s)
{% endhighlight %}

Not great. All the internal implementation details leak out. By
employing the opaque constraint trick above, we can define overlapping
instances for the `AsType` class, which results in the following type signature:

{% highlight haskell %}
>>> :t _Typed
_Typed :: AsType a s => Prism' s a
{% endhighlight %}

which is much nicer!

## Acknowledgements

I wrote most of this post a while time ago, but never published it.
Thanks to [Rob Rix](https://twitter.com/rob_rix) for bringing up this
topic and thus reminding me to publish it. It's good to see library authors
care about the user experience of their library down to this level of detail,
and I hope this technique will be useful for many others!
