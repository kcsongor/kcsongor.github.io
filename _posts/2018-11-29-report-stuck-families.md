---
layout: post
title: "Detecting the undetectable: custom type errors for stuck type families"
excerpt: "Stuck type families are notorious for producing puzzling type errors. In this post I present a technique for detecting and reporting them."
modified: 2018-11-30
tags: [haskell, type-level, type families, errors]
comments: true
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

Custom type errors are a great way to improve the usability of Haskell
libraries that utilise some of the more recent language extensions.
Yet anyone who has written or used one of these libraries will know
that despite the authors' best efforts, there are still many occasions
where a wall of text jumps out, leaving us puzzled as to what went wrong.

This post is about one particular class of such errors that have been
troubling users of many modern Haskell libraries: stuck type
families.

The following type error perfectly illustrates the problem. It is an
actual error [reported](https://github.com/kcsongor/generic-lens/issues/73)
on the issue tracker of the
[generic-lens](http://hackage.haskell.org/package/generic-lens)
library.

{% highlight text %}
• No instance for (Data.Generics.Product.Types.HasTypes'
                     (Data.Generics.Product.Types.Snd
                        (Data.Generics.Product.Types.InterestingOr
                           Description
                           (Data.Generics.Product.Types.InterestingOr
                              Description
                              (Data.Generics.Product.Types.Interesting'
                                 Description
                                 (Rep Text)
                                 Name
                                 '[Text, Sirname, None, Description])
                              (M1
                                 S
                                 ('MetaSel
                                    ('Just "name")
                                    'NoSourceUnpackedness
                                    'NoSourceStrictness
                                    'DecidedLazy)
                                 (Rec0 Name))
                              Name)
                           (M1
                              C
                              ('MetaCons "M" 'PrefixI 'False)
                              (S1
                                 ('MetaSel
                                    'Nothing
                                    'NoSourceUnpackedness
                                    'NoSourceStrictness
                                    'DecidedLazy)
                                 (Rec0 Multiple)))
                           Name))
                     Description
                     Name)
    arising from a use of ‘types’
{% endhighlight %}

Can you spot the problem? Even if you know what to look for,
it takes a good few seconds to locate the culprit. The goal of this
post is to turn the above into the following:

{% highlight text %}
• No instance for Generic Text
   arising from a traversal over Description.
{% endhighlight %}

How could we possibly identify a lack of `Generic` instance from the
above? Let us have a closer look at that large type error. It is a
nested chain function of calls, such as `Snd` and `Interesting`, which
are type families leaking out of the library's implementation. The
reason we see these type families (as opposed to the result they
evaluate to), is because the computation is _stuck_. The culprit is
the `Rep Text` part somewhere in the middle.

It turns out that `Rep` is an associated type family of the `Generic`
class:

{% highlight haskell %}
class Generic a where
  type Rep a :: Type -> Type
  ...
{% endhighlight %}

Thus, the reason `Rep Text` is not defined is that `Text` has no
`Generic` instance. Clearly, it's unreasonable to expect users to keep
such implementation details in mind and hunt for unreduced occurrences
of `Rep` in their type errors to find out what the issue is!

Yet, reporting this is not so easy. To explain why, we need to
understand the behaviour of type families.

As things stand today, the associated family `Rep` is not actually
connected to the `Generic` class as far as the type checker is
concerned.  This is why unreduced occurrences will not result in error
messages mentioning anything about `Generic` in the first place.
[Constrained type families](https://arxiv.org/abs/1706.09715) offer a solution to this problem, but
they are not (yet) implemented in GHC.
{: .notice}

## Type family evaluation semantics

The reduction of type families is driven by the constraint solver. To
the best of my knowledge, there is no formal specification for their
semantics, so I'm not going to attempt to give a comprehensive account
here either. Instead, let us just make some key observations about
how type families reduce.

A type involving a type family is said to be _stuck_ if none of the
type family's equations can be selected for the provided
arguments. Since `Text`s have no `Generic` instance, there is
consequently no `Rep Text` instance defined either. Thus, `Rep Text`
is stuck.

How does "stuckness" propagate up a chain of function calls? Consider
the following type family:

{% highlight haskell %}
type family Foo a where
  Foo a = a
{%endhighlight %}

No matter what we pass in as the argument, the single equation will
always match. This means that even if we pass in a stuck type, such as
`Rep Text`, the equation can reduce to the right hand side (and get
stuck afterwards):

{% highlight text %}
>>> :kind! Foo (Rep Text)
= Rep Text
{% endhighlight %}

In other words, we can think of `Foo` as a type family that's "lazy"
in its argument. Now consider the `Bar` type family:

{% highlight haskell %}
type family Bar a where
  Bar Maybe = Maybe
  Bar a = a
{%endhighlight %}

Here, we first check if the argument is `Maybe`, in which case `Maybe`
is returned, otherwise we pick the second equation. Perhaps
surprisingly, `Bar` behaves the same as `Foo`:

{% highlight text %}
>>> :kind! Bar (Rep Text)
= Rep Text
{% endhighlight %}

The two equations of `Bar` _agree_ with each other, because the first
one is a substitution instance of the second. GHC recognises this, and
decides that it is safe to drop the first equation in favour of the
second one.

We can of course write disagreeing equations:

{% highlight haskell %}
data T1 x
data T2 x

type family FooBar a where
  FooBar T1 = T2
  FooBar a = a
{% endhighlight %}

This time, notice that the first equation is not a substitution
instance of the second: it returns something other than the argument.

GHC won't optimise this case away anymore, and now instance matching
will have to consider both equations. A given equation matches, if the
argument unifies with the pattern, and is apart from all of the preceding
patterns (i.e. doesn't match any of them).
The important thing here is that a stuck type is *not* apart from any
other type, but neither does it match any other type. This means that

{% highlight text %}
>>> :kind! FooBar (Rep Text)
= FooBar (Rep Text)
{% endhighlight %}

`FooBar` gets stuck just when its argument does. We can think of
`FooBar` as a type family that is "strict" in its argument.

If we pass in a non-stuck value, evaluation proceeds as normal:

{% highlight text %}
>>> :kind! FooBar Maybe
= Maybe
{% endhighlight %}

Since `Maybe` is apart from `T1` (they are different ground types), and
it unifies with the catch-all pattern `a`.

So, if a type family that inspects its argument is given a stuck type,
then the resulting type will be stuck itself. Notice that we can't
proceed any further: there is no way to detect if the argument was
stuck or not. This is why the type error above is so impenetrable.
If we ignore our argument like `Foo` does, then it just slips by, but
if we try to do something with it like `FooBar` does, we get stuck.

Of course, I wouldn't have written down all of these low-level details
about type family reduction if they didn't lead to a solution!

## Custom type errors

The mechanism of custom type errors is quite simple. The constraint
solver proceeds normally, reducing all type family equations and
solving all type class instances. If at the end, there are any
constraints of the form `TypeError ...`, then the payload of the error
gets printed, otherwise any unsolved constraints are reported.

As an example

{% highlight haskell %}
foo :: TypeError ('Text "Ouch") => ()
foo = 10
{% endhighlight %}

yields

{% highlight text %}
• Ouch
{% endhighlight %}

even though `10` clearly doesn't have type `()`.

We want to produce a custom type error when the `Rep` type family gets
stuck, and we'd like to continue normally otherwise. As discussed
above, there is no way to branch on whether a type family is stuck or
not.

However, we now have all the necessary pieces: all we need to do is
to make sure that when `Rep` gets stuck, we leave a `TypeError` in the
residual constraints. To do this, we're going to wrap the call to `Rep` in
another type family, which will get stuck just when `Rep` is stuck. When
`Rep` reduces, our wrapper reduces too. The additional piece is that
the wrapper will also hold a type error as its argument, which will
reside in the unsolved constraint in the stuck case, but disappear otherwise.

{% highlight haskell %}
type family Break (c :: Constraint) (rep :: Type -> Type) :: Constraint where
  Break _ T1 = ((), ())
  Break _ _  = ()
{% endhighlight %}

`Break` is the wrapper family. It takes a constraint, which will be
our type error. Then it forces its argument by testing against
`T1`. Note that in both equations, the type family reduces to the
trivial constraint `()`, but in the first case, we use `((), ())` (a
tuple of two trivial constraints) to ensure that the equations
don't optimise away, like they did with `Bar`.

Finally, we introduce a type family to construct a custom error message:
{% highlight haskell %}
type family NoGeneric t where
  NoGeneric x = TypeError ('Text "No instance for " ':<>: 'ShowType (Generic x))
{% endhighlight %}

Now, consider what happens when we call `Break` with the stuck
argument `Rep Text`:

{% highlight text %}
>>> :kind! Break (NoGeneric Int) (Rep Text)
= Break (TypeError ...) (Rep Text)
{% endhighlight %}

the type gets stuck, with a `TypeError` inside! However, when
called with a type where `Rep` is defined, such as `Bool`, the type reduces
to the unit constraint, no mention of the type error.

{% highlight text %}
>>> :kind! Break (NoGeneric Bool) (Rep Bool)
= () :: Constraint
{% endhighlight %}

And with this, we can report errors for any stuck type family.

{% highlight haskell %}
bar :: Break (NoGeneric Text) (Rep Text) => ()
bar = ()
{% endhighlight %}

yields

{% highlight text %}
• No instance for Generic Text
• In the expression: bar
{% endhighlight %}


# Conclusion

Using this technique, we can place custom type errors right where our
stuck type families are, and provide more contextual information about
what went wrong. We can even generalise the above to the following type family:

{% highlight haskell %}
type family Any :: k

type family Assert (err :: Constraint) (break :: Type -> Type) (a :: k) :: k where
  Assert _ T1 _ = Any
  Assert _ _ k = k
{% endhighlight %}

which we can use at any point in a computation, not just in
constraints. `Assert` takes a type error, a potentially stuck
computation, and a value. If the computation is stuck, then the custom
error is presented, otherwise the value is passed through without any
errors. Here, strictness is forced by the same `T1` trick, but this
time, to ensure that the right hand sides are also different, we
return the `Any` type family in the first case.