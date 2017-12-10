---
layout: post
title: Announcing generic-lens 0.5.0.0
excerpt: Deriving lenses generically
modified: 2017-10-10
tags: [haskell, generic-lens, generics, lens]
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

The [generic-lens](https://hackage.haskell.org/package/generic-lens) library
provides utilities for deriving various optics for your datatypes,
using `GHC.Generics`. In this post I'll go over some of the features and
provide examples of using them.

## Overview
Lenses have proven to be an exteremely powerful tool in the Haskell ecosystem.
`generic-lens` uses `GHC.Generics` to derive lenses and prisms on the fly, only
when they are needed. These optics are highly polymorphic, and can be used with
all types that are of the right shape. Extra care has been taken to keep type
errors readable.

### Examples
To get started, we will need the following extensions:

{% highlight haskell %}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{% endhighlight %}

And the following imports

{% highlight haskell %}
import Control.Lens
import Data.Generics.Product
import GHC.Generics
{% endhighlight %}

Consider the following datatype:

{% highlight haskell %}
data Human a
  = Human
    { name    :: String
    , age     :: Int
    , address :: String
    , other   :: a
    } deriving (Generic, Show)
{% endhighlight %}

#### field

We can access the `name` field:

{% highlight txt %}
>>> Human "John" 18 "London" True ^. field @"name"
"John"
{% endhighlight %}

We can update fields too, even changing types where possible (when the type of
the field is a type parameter of the datatype):

{% highlight txt %}
>>> Human "John" 18 "London" True & field @"other" %~ show
Human {name = "John", age = 18, address = "London", other = "True"}
{% endhighlight %}

In case of sum types, it only makes sense to have a lens on the fields that
appear in every constructor. Trying to use `field` to get a lens for a partial
field is a type error.

Note that the `field` lens works with `DuplicateRecordFields`, which means that
record fields can actually be shared, and we can get a reusuble lens for all
cases without code duplication.
{: .notice}

#### typed

We can directly reference a field by its type, as long as the type is unique in
the structure.

{% highlight txt %}
>>> Human "John" 18 "London" True ^. typed @Bool
True
{% endhighlight %}

{% highlight txt %}
>>> Human "John" 18 "London" True ^. typed @String

<interactive>:34:34: error:
    • The type Human Bool contains multiple values of type [Char].
      The choice of value is thus ambiguous. The offending constructors are:
      • Human

    • In the second argument of ‘(^.)’, namely ‘typed @String’
      In the expression: Human "John" 18 "London" True ^. typed @String
      In an equation for ‘it’:
          it = Human "John" 18 "London" True ^. typed @String
{% endhighlight %}

#### position

When the above two fail, and we have a product type, we can specify the field
of interest by its position.

{% highlight haskell %}
data MyTuple a b = MyTuple a b deriving (Generic, Show)
{% endhighlight %}

{% highlight txt %}
>>> MyTuple 10 20 & position @1 .~ "hello"
MyTuple "hello" 20
{% endhighlight %}

#### super (row polymorphism)

Given two records, where the set of fields of one is the subset of that of the
other, we can talk about a structural subtype relationship. The `super` lens
allows us to treat the subtype as the supertype - without forgetting the
original structure.

{% highlight haskell %}
data Small
  = Small
    { small :: Int
    } deriving (Generic, Show)

data Large
  = Large
    { small :: Int
    , large :: String
    } deriving (Generic, Show)

smallFun :: Small -> Small
smallFun (Small n) = Small (n + 10)
{% endhighlight %}

(Here, we need the `{-# LANGUAGE DuplicateRecordFields #-}` extension in
addition to the previous ones.)

{% highlight txt %}
>>> Large 10 "foo" & super %~ smallFun
Large {small = 20, large = "foo"}
{% endhighlight %}

Or we can simply upcast:

{% highlight txt %}
>>> Large 10 "foo" ^. super :: Small
Small {small = 10}
{% endhighlight %}

{% highlight txt %}
>>> Small 10 ^. super :: Large

<interactive>:53:13: error:
    • The type 'Small' is not a subtype of 'Large'.
      The following fields are missing from 'Small':
      • large
{% endhighlight %}

#### _Ctor

We can also obtain prisms that focus on individual constructors:

{% highlight txt %}
>>> Human "John" 18 "London" True ^? _Ctor @"Human"
Just ("John",18,"London",True)
{% endhighlight %}

{% highlight txt %}
>>> Human "John" 18 "London" True ^? _Ctor @"Human" . position @3
Just "London"
{% endhighlight %}

### mtl
So far, we haven't provided any type signatures. Indeed, everything can be
inferred by the compiler. However, because these combinators are highly
polymorphic, it might be interesting to use them in a polymorphic context.

{% highlight haskell %}
f :: (MonadReader env m, HasField' "username" env String) => m String
f = view (field @"username")
{% endhighlight %}

This function is now polymorphic not just in the monad stack it will eventually
run in, but also in the type of the environment.

The type of `field` is
{% highlight haskell %}
field :: HasField field s t a b => Lens s t a b
{% endhighlight %}

`HasField'` (similarly to `Lens'`) is a type synonym for `HasField field s s a a`.

For a more comprehensive overview and more examples, please have a look at the
library on [hackage](https://hackage.haskell.org/package/generic-lens), or on
[github](https://github.com/kcsongor/generic-lens).

## Performance
An important question when evaluating such high-level abstractions is whether
the abstraction comes at the cost of performance. Fortunately, GHC optimises
away all of the overhead of the generic transformations, leaving us with code
that is equivalent to what we would've written manually.

This can be verified by comparing the generated core of both the manually
written lens and the generated one. However, it happened multiple times during
development that a small change (such as eta-reduction) broke the optimisation.
Joachim Breitner's excellent
[inspection-testing](https://github.com/nomeata/inspection-testing) tool, which
is now integrated into the automated test suite, is making sure that the
optimisation happens by automatically doing this comparison. This tool has been
invaluable in ensuring the performance guarantees, without having to manually
inspect the generated core after every single commit. The tests can be found
[here](https://github.com/kcsongor/generic-lens/blob/master/test/Spec.hs).

It's important to mention that as of this release, only the lenses are
optimised away completely, the prisms still have some leftover overhead. This
is planned to be fixed in a future release.
{: .notice}

## Quick note (migration)

In case you were already using the library, there are some breaking changes in `0.5.0.0`.
Namely, all the `Has*` classes have been extended from 3 type parameters to 5.
Auxiliary constraint synonyms are provided, and migration should be relatively simple:

{% highlight haskell %}
f :: HasField field a record => ...
{% endhighlight %}
becomes
{% highlight haskell %}
f :: HasField' field record a => ...
{% endhighlight %}
Notice the `'` at the end of the class name, and the swapping of the last two arguments.
{: .notice}

## Acknowledgements
Thanks to Matthew Pickering for useful comments on a draft of this post.
