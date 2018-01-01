---
layout: post
title: Deriving Bifunctor with Generics
excerpt: A techinque for approximating Generic2 with only the Generic type class.
modified: 2018-01-01
tags: [haskell, generics, deriving]
comments: true
---

<section id="table-of-contents" class="toc">
  <header>
    <h3>Contents</h3>
  </header>
<div id="drawer" markdown="1">
*  Auto generated table of contents
{:toc}
</div>
</section><!-- /#table-of-contents -->

Recently, I've been experimenting with deriving various type class instances
generically, and seeing how far we can go before having to resort to
TemplateHaskell.  This post is a showcase of one such experiment: deriving
[Bifunctor](https://hackage.haskell.org/package/bifunctors), a type class that ranges
over types of kind `* -> * -> *`, something `GHC.Generics` is known not to be
well suited for. The accompanying source code can be found in [this gist](https://gist.github.com/kcsongor/a8cb718f676c6ca1d999bfc56def9b7b).

## The problem

The [GHC.Generics](https://hackage.haskell.org/package/base-4.10.1.0/docs/GHC-Generics.html)
module defines two representations: `Generic` and `Generic1`. The former is used to describe
types of kind `*`, while the latter is used for `* -> *`.
For example, the `Generic1` representation is used in the [generic-deriving](http://hackage.haskell.org/package/generic-deriving-1.12/docs/Generics-Deriving-Functor.html) package's Functor derivation.

{% highlight haskell %}
class GFunctor (f :: * -> *) where
  gmap :: (a -> b) -> f a -> f b
{% endhighlight %}

Then instances are defined for the generic building blocks. Whenever we have a
`GFunctor (Rep1 f)`, we can turn that into a `Functor f`.

With this, it's possible to derive many useful instances of classes that range
over `*` or `* -> *`. However, there's no `Generic2`, so if we try to adapt `generic-deriving`'s
Functor approach to Bifunctors, we'll run into problems.

{% highlight haskell %}
class Bifunctor (p :: * -> * -> *) where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
{% endhighlight %}

The type parameter `p` takes two arguments, but the generic `Rep` and `Rep1`
representations are strictly `* -> *` (in the case of `Rep`, the type parameter
is phantom -- it's only there so that much of the structure of `Rep` and `Rep1`
can be shared, and `Rep1` requires `* -> *`). This means that even if we
defined a `GBifunctor`, we would need to require a `GBifunctor (Rep2 p)` which
we could then turn into a `Bifunctor p`. Alas, `Rep2` doesn't exist.

Indeed, the deriving mechanism in the bifunctors package uses TH.

## The solution

The solution is inspired by how lenses implement polymorphic updates. The idea
is that a `Lens s t a b`  focuses on the `a` inside some structure `s`, and if
we swap that `a` with a `b`, we get a `t`.

Since we're talking about Bifunctors now, we need two more type variables:

{% highlight haskell %}
class GBifunctor s t a b c d where
  gbimap :: (a -> b) -> (c -> d) -> s x -> t x
{% endhighlight %}

`s` and `t` will be the generic representations, which means they are of kind
`* -> *`. However, we're going to be using `Generic` instead of `Generic1`, so
the type parameter `x` is not used.

Unlike the `GFunctor` class, which looked exactly like `Functor`, this one is a
lot different from `Bifunctor`. Also important to note that `gbimap`'s type
signature is more polymorphic than that of `bimap`, so we need to ensure that
our instances are properly parametric.

In an earlier version of this class, I had functional dependencies on the
class that expressed this interrelation between the type variables, but I had to
lose them so that more interesting instances could be defined (more on this
later).
{: .notice }

### The boring instances

The first instance simply looks through the metadata node.
{% highlight haskell %}
instance GBifunctor s t a b c d
  => GBifunctor (M1 k m s) (M1 k m t) a b c d where

  gbimap f g = M1 . gbimap f g . unM1
{% endhighlight %}

A sum `l :+: r` can be turned into `l' :+: r'` if we can turn `l` into `l'` and
`r` into `r'`.
{% highlight haskell %}
instance
  ( GBifunctor l l' a b c d
  , GBifunctor r r' a b c d
  ) => GBifunctor (l :+: r) (l' :+: r') a b c d where

  gbimap f g (L1 l) = L1 (gbimap f g l)
  gbimap f g (R1 r) = R1 (gbimap f g r)
{% endhighlight %}

And similarly, for products.
{% highlight haskell %}
instance
  ( GBifunctor l l' a b c d
  , GBifunctor r r' a b c d
  ) => GBifunctor (l :*: r) (l' :*: r') a b c d where

  gbimap f g (l :*: r) = gbimap f g l :*: gbimap f g r
{% endhighlight %}

The last boring instance is for unit types, these are trivially Bifunctors.
{% highlight haskell %}
instance GBifunctor U1 U1 a b c d where
  gbimap _ _ = id
{% endhighlight %}

### Incoherent instances

With all of the gluing out of the way, we can now get to the meat of the
problem: the actual fields in the constructors. When considering a field, we
have 3 cases:

The field is of type `a`, and we apply the first function to turn it into a `b`.

{% highlight haskell %}
instance {-# INCOHERENT #-} GBifunctor (Rec0 a) (Rec0 b) a b c d where
  gbimap f _ (K1 a) = K1 (f a)
{% endhighlight %}

Similarly, if it's a `c`, we turn it into a `d` using the second function.

{% highlight haskell %}
instance {-# INCOHERENT #-} GBifunctor (Rec0 c) (Rec0 d) a b c d where
  gbimap _ g (K1 a) = K1 (g a)
{% endhighlight %}

Finally, the field is neither `a`, nor `c`, so we just leave it alone.
{% highlight haskell %}
instance {-# INCOHERENT #-} GBifunctor (Rec0 x) (Rec0 x) a b c d where
  gbimap _ _ = id
{% endhighlight %}

Note that these instances need to be defined with `{-# INCOHERENT #-}` pragmas.
This is required because neither of `(Rec0 a) (Rec0 b) a b c d` and `(Rec0 c) (Rec0 d) a b c d` is
more specific than the other.

However, in our case, this is not a problem, because we're going to invoke
instance resolution with polymorphic arguments, so there will be exactly one
instance that matches.

### Default signatures

We can now revise our original class definition, and add a default signature
(`DefaultSignatures`). This will make `Bifunctor` derivable with `DeriveAnyClass`.

{% highlight haskell %}
class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

  default bimap
    :: ( Generic (p a c)
       , Generic (p b d)
       , GBifunctor (Rep (p a c)) (Rep (p b d)) a b c d
       ) => (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = to . gbimap f g . from
{% endhighlight %}

Note the line `GBifunctor (Rep (p a c)) (Rep (p b d)) a b c d`. Here's where we
establish the relationship between the types. This now allows us to derive a
`Bifunctor` instance for `Either`:

{% highlight haskell %}
deriving instance Bifunctor Either
{% endhighlight %}

For example, when looking at the `Left` constructor, the compiler will try to
find an instance for `GBifunctor (Rec0 a) (Rec0 b) a b c d`. There is exactly
one instance that matches this, so our incoherent instance will not bite us.
This is important: if instead we wanted an instance for a concrete type, say,
`Either Int Int`, all of our incoherent instances would match, and an arbitrary
one would be picked. However, we avoid this problem by ensuring that the
instance is derived for the aformentioned polymorphic form.

With this, we have a correct implementation of `bimap` for `Either`:

{% highlight txt %}
>>> bimap show (+ 10) (Left 10)
Left "10"
>>> bimap show (+ 10) (Right 10)
Right 20
{% endhighlight %}

Even better, compiled with `-O1`, all of the overhead from using generics is
optimised away:

{% highlight txt %}
$fBifunctorEither_$cbimap
  = \ @ a_a3EL @ b_a3EM @ c_a3EN @ d_a3EO f_X1EN g_X1EP eta_B1 ->
      case eta_B1 of {
        Left g1_a3X5 -> Left (f_X1EN g1_a3X5);
        Right g1_a3X8 -> Right (g_X1EP g1_a3X8)
      }
{% endhighlight %}

### A few more instances

The above deriving mechanism is naive: it only looks at fields whose types is
exactly `a` or `b`. But we can do better: what if the field is a `Maybe a`?
Surely we can turn that into a `Maybe b`. Or if it's an `Either a b`, we can turn that into
an `Either c d`, since it has a `Bifunctor` instance.

The following three instances do exactly that.

{% highlight haskell %}
instance {-# INCOHERENT #-} Bifunctor f
  => GBifunctor (Rec0 (f a c)) (Rec0 (f b d)) a b c d where
  gbimap f g (K1 a) = K1 (bimap f g a)

instance {-# INCOHERENT #-} Functor f
  => GBifunctor (Rec0 (f c)) (Rec0 (f d)) a b c d where
  gbimap _ g (K1 a) = K1 (fmap g a)

instance {-# INCOHERENT #-} Functor f
  => GBifunctor (Rec0 (f a)) (Rec0 (f b)) a b c d where
  gbimap f _ (K1 a) = K1 (fmap f a)

instance {-# INCOHERENT #-} Bifunctor f
  => GBifunctor (Rec0 (f a a)) (Rec0 (f b b)) a b c d where
  gbimap f _ (K1 a) = K1 (bimap f f a)

instance {-# INCOHERENT #-} Bifunctor f
  => GBifunctor (Rec0 (f c c)) (Rec0 (f d d)) a b c d where
  gbimap _ g (K1 b) = K1 (bimap g g b)
{% endhighlight %}


Now we can derive even more interesting `Bifunctor` instances.

{% highlight haskell %}
data T a b = T1 (Maybe a) a (Either a b) | T2 (Maybe b)
  deriving (Generic, Bifunctor)
{% endhighlight %}

## Conclusion

We have seen a technique for approximating a hypothetical `Generic2`
representation with only using `Generic`. Of course there was nothing specific
about the number 2, we can easily generalise this to any fixed number of
parameters.

I'm planning on writing a post about a further generalisation of
this idea, which allows us to talk about types that have an arbitrary number type
parameters (unlike here, where it's a fixed number), which I used in the
[generic-lens](https://hackage.haskell.org/package/generic-lens) library, to
allow for type changing lenses over any type parameter (thanks to the more
elaborate extra machinery, there is no need for incoherent instance
resolution).

It would be interesting to see how far this can be pushed before hitting a
roadblock that would truly require a bespoke `GenericN` representation.

## Acknowledgements

Thanks to [@adituv](https://github.com/adituv) for pointing out that two instances were missing.
