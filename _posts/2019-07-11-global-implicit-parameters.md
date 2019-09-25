---
layout: post
title: "Global Implicit Parameters"
excerpt: "Exploiting GHC internals to define top-level implicit parameters."
modified: 2019-07-11
tags: [haskell, ghc, implicit parameters]
comments: true
---

{% include _toc.html %}

Implicit parameters (enabled with the `{-# LANGUAGE ImplicitParams #-}` pragma) provide a way to dynamically bind variables in Haskell.

For example, the following function can be called in any context where `?x` is bound:

{% highlight haskell %}
foo :: (?x :: Int) => Int
foo = ?x

bar :: Int
bar = let ?x = 10 in foo
{% endhighlight %}

Unlike type classes, implicit parameters are bound locally. But what
if we want to bind one in the global scope? This would allow a global
"default" value, which could then be shadowed locally.

Unfortunately, the following is syntactically invalid:

{% highlight haskell %}
?x = 21
{% endhighlight %}

We turn to the [GHC User Manual](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#implicit-parameter-bindings),
only to be further discouraged:

> A group of implicit-parameter bindings may occur anywhere a normal group of Haskell bindings can occur, except at top level.

Of course, we won't let mere syntactic restrictions to get in our way.

## Under the hood

Since global binding of implicit parameters is officially not possible,
we need to turn to unofficial methods.
To begin, we pass the `-ddump-tc-trace` flag
to GHC and recompile the module containing `foo` and `bar`.
This makes GHC dump information about what it's doing during typechecking
the module. There is quite a lot of output, but one line looks interesting:

{% highlight text %}
canEvNC:cls ghc-prim-0.5.3:GHC.Classes.IP ["x", Int]
{% endhighlight %}

Good software engineering practice dictates code reuse, and we all
know that GHC is a well-engineered piece of software. Therefore, it is
not surprising to find that implicit parameters are implemented by
piggybacking off of type class resolution with some additional rules
to disregard issues like global coherence.

As the above line suggests, implicit parameter resolution is desugared into
the resolution of the `GHC.Classes.IP` type class from `ghc-prim`.

Even though this module is [not documented](http://hackage.haskell.org/package/ghc-prim-0.5.3), we
can import it and ask GHCi for more information:

{% highlight haskell %}
class IP (s :: Symbol) a | s -> a where
  ip :: a
  {-# MINIMAL ip #-}
{% endhighlight %}

It looks like GHC generates instances of the `IP` class on the fly
whenever it sees a binder for an implicit parameter. The name
of the parameter is represented as a type-level symbol. The functional
dependency allows the variable's type to be resolved just from its name.

Let's try to write an instance for this class by hand:

{% highlight haskell %}
-- ?x = 21
instance IP "x" Int where
  ip = 21
{% endhighlight %}

GHC happily accepts this definition. Indeed, we can now write

{% highlight haskell %}
baz :: Int
baz = ?x
{% endhighlight %}

which evaluates to `21`, by picking up the `?x` variable from the
top-level scope. As expected, `let ?x = 10 in foo` still evaluates to `10`, as it
_shadows_ the top-level binding.

## Barewords

Perhaps this is a good place to stop. But we can go further:
above, we defined only the `?x` variable. It turns out
that we can define an instance for _all_ symbols at once:

{% highlight haskell %}
instance KnownSymbol s => IP s String where
  ip = symbolVal (Proxy :: Proxy s)
{% endhighlight %}

This instance brings all possible implicit variables into scope, and
assigns their name their value by reflecting the symbol into a string.

{% highlight haskell %}
bye :: String
bye = ?thanks ++ " " ++ ?for ++ " " ++ ?reading
{% endhighlight %}

Which _almost_ feels like writing Perl!
