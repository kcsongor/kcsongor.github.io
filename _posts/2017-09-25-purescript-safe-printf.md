---
layout: post
title: Well-typed printfs cannot go wrong
excerpt: "Writing a simple parser of type-level symbols in PureScript 0.12"
modified: 2017-9-25
tags: [purescript, parsing, type-level]
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

One of the classic examples that keeps coming up when talking about dependently
typed programming languages is the "safe" `printf` function -- one that ensures
that the number and type of arguments match the requirement in the format
specification.

In languages like Idris, this is just a function that takes a format string,
and returns the type of arguments required for constructing the formatted
output string.

{% highlight haskell %}
  format "A number: %d, and a string: %s" : Int -> String -> String
{% endhighlight %}

Other languages, like rust, solve this by various means of metaprogramming:
writing a program (macro) that runs at compile-time, generating the program to
be executed at runtime.

What these two approaches have in common is that they both operate on strings
that are statically available to the compiler. The aim of this post is to show
another way of achieving the same result, with tools that are available in
PureScript - a strongly-typed functional language, with no dependent types.

## The problem

We want to write a program that takes a format string, some number of
arguments, and returns the result of inserting the arguments at their specified
places in the format string, and does all this in a type-safe way.

{% highlight haskell %}
> :t format @"Wurble %d %d %s"
Int -> Int -> String -> String
{% endhighlight %}

{% highlight haskell %}
> format @"Wurble %d %d %s" 10 20 "foo"
"Wurble 10 20 foo"
{% endhighlight %}

{% highlight haskell %}
> format @"Wurble %d %d %s" 10 20 30
Error found:

  Could not match type

    String

  with type

    Int

while trying to match type Function String
  with type Function Int
{% endhighlight %}

Crucially, we need to compute a type from some input, but because PureScript
has no dependent types, values and functions in the traditional sense are not
available for evaluation at compile-time. However, there is a way to interact
with the compiler: via the type-checker.

The solution therefore is to encode this computation in the types, and have the
type-checker evaluate it for us as part of type-checking. Luckily, PureScript
allows string literals in types (these are types whose kind is `Symbol`).

Thus, constructing our `printf` function comprises two steps:

  * parse the input `Symbol` into a list of format tokens
  * generate the function from the format list that will then assemble the output string

## Type-level parsing

For the sake of simplicity, we're going to focus on two types of format
specifiers: decimals (`%d`) and strings (`%s`).

We represent these cases with a _custom kind_, which is like a regular
algebraic datatype, but lifted to the type-level. This means that these
constructors can be used _in types_.

{% highlight haskell %}
foreign import kind Specifier

foreign import data D   :: Specifier
foreign import data S   :: Specifier
foreign import data Lit :: Symbol -> Specifier
{% endhighlight %}

Of course, apart from the format specifiers `%d` and `%s`, everything else is a
literal, which we account for by wrapping them in the `Lit` type constructor.

The `foreign import` bit means that we're introducing types here that have no
constructors. That is to say, it's impossible to construct a value of type `D`
and `S`.  We'll see later how it is still possible to carry these types around
in terms (hint: proxies).
{: .notice}

Furthermore, we need a way of representing a sequence of these specifiers, for
which we introduce another kind:

{% highlight haskell %}
foreign import kind FList

foreign import data FNil  :: FList
foreign import data FCons :: Specifier -> FList -> FList
{% endhighlight %}

With this, we can now write types like `FCons D (FCons (Lit " foo") FNil)`,
corresponding to the string `%d foo`.

Kind-polymorphism is not supported by the current version (0.12) of PureScript,
so we can't define a parametric type-level list once and for all - we need a
new one for each type we want to store in lists. This limitation is likely to
be removed in a future version of the compiler.
{: .notice}

With these building blocks defined, now we have a vocabulary for talking about
the parser itself: it is a function that takes a `Symbol` as an input, and
returns a `FList`. We encode the computation in the following type class:

{% highlight haskell %}
class Parse (string :: Symbol) (format :: FList) | string -> format
{% endhighlight %}

The functional dependency `string -> format` states that the input `string`
determines the ouput `format`. This bit is crucial, as this is what tells the
compiler that knowing `string` is sufficient in determining what the value of
`format` is. It is then our task to ensure that this dependency indeed holds,
when writing out the instances.

To deconstruct the input symbol, we use the following type class available in 0.12:

{% highlight haskell %}
class ConsSymbol (head :: Symbol)
                 (tail :: Symbol)
                 (sym :: Symbol) |
                 sym -> head tail, head tail -> sym
{% endhighlight %}

The interesting functional dependency here is the `sym -> head tail`, which,
given some symbol, deconstructs it into its `head` (the first character) and
its `tail` -- the rest.

The parser is like a state machine, with the following legal states:
  * State 1: found a non-`%` character
  * State 2: found a `%` character

One possible way of representing these states is by having a separate type class
to deal with each.

Since in our simplified example, we know that the specifier symbols can
only be single characters, we can define the second state as:

{% highlight haskell %}
class Parse2 (head :: Symbol) (out :: Specifier) | head -> out
{% endhighlight %}

That is, it takes a symbol, and returns the matching specifier. The
implementation is straightforward:

{% highlight haskell %}
instance parse2D :: Parse2 "d" D
instance parse2S :: Parse2 "s" S
{% endhighlight %}

This is a partial function, which means that format strings that contain
unsupported specifier tokens will simply fail to compile.

The first state is more complicated, as it can consume an arbitrary number of
characters, so we pass it the remaining string (`tail`) as well.

{% highlight haskell %}
class Parse1 (head :: Symbol) (tail :: Symbol) (out :: FList) | head tail -> out
{% endhighlight %}

`Parse1` represents the parsing state where we have the current character
`head`, the rest of the input string `tail`, and we know that the previous
character was not a `%`.

The first case is when the tail is empty. In this case, we just return the
current character as the literal in a singleton list:

{% highlight haskell %}
instance parse1Nil :: Parse1 a "" (FCons (Lit a) FNil)
{% endhighlight %}

The second case is more interesting. This is when we find a `%`, so we need to
invoke the other function, `Parse2`, which handles parsing the specifier
itself. To do that, we use `ConsSymbol` to split our current tail `s` into
its head `h` and tail `t`. `h` contains the format specifier, which we pass on
to `Parse2`. Then, recursively invoke `Parse` on `t` to parse the rest of
the input. In addition to returning `spec` consed to `rest`, we also put
an empty string literal at the head of the output list: this is to maintain
the invariant that the head of the output list always contains a string literal.
This invariant will be useful for the last case...

{% highlight haskell %}
else instance parse1Pc ::
  ( ConsSymbol h t s
  , Parse2 h spec
  , Parse t rest
  ) => Parse1 "%" s (FCons (Lit "") (FCons spec rest))
{% endhighlight %}

...when we match any other character, i.e. other than `%`. Since we're in
`Parse1`, that means that the current character needs to be in a string
literal. For this, we first recursively parse the tail `s` into
`FCons (Lit acc) r`. The reason we want to know that at the head of parsing
the remaining string is a `Lit` is so that we can prepend the current character
to that literal - we need to rebuild long string literals
character-by-character after all. This is where the invariant from the previous
two cases is useful: we don't have to handle the cases where the head is not
a `Lit`, because the recursive calls guarantee that it is. `acc` is thus the
tail of the string literal we're currently parsing, so we put it together
with the current character by `ConsSymbol o acc rest` (recall that this type
class can both construct and deconstruct symbols via its functional
dependencies). Then we simply return `Lit rest` along with `r`.

{% highlight haskell %}
else instance parse1Other ::
  ( Parse s (FCons (Lit acc) r)
  , ConsSymbol o acc rest
  ) => Parse1 o s (FCons (Lit rest) r)
{% endhighlight %}

Notice how these instances actually overlap. In the third case, we can
easily imagine a particular instantiation of `o` and `r` such that it
matches the instance head in the second case. In other words, when the current
character is `%`, both `parse1Pc` and `parse1Other` match (because
`parse1Other` is more general).

To make sure that the instances are selected in the order we want them to be,
we use instance chains. That is, by writing `instance A else instance B` we
tell the compiler to try to match instance `A` first, and if it fails, then try
`B`. This is a new feature in PureScript 0.12, and a very powerful one - it
allows us to avoid the overlapping instance problem for good.

Finally, we need to actually kick off the parser. We do this by invoking it
in the first state.

{% highlight haskell %}
instance parseNil ::
  Parse "" (FCons (Lit "") FNil)
else instance parseCons ::
  ( ConsSymbol h t string
  , Parse1 h t fl
  ) => Parse string fl
{% endhighlight %}

## How the sausage gets made: computing the output type

But how do we know how many arguments we need to pass to the formatter? It
depends on the format string! No surprises here: just like all the previous
type-level computations, this one will also be encoded in a type class with a
functional dependency.

{% highlight haskell %}
class FormatF (format :: FList) fun | format -> fun where
  formatF :: @format -> String -> fun
{% endhighlight %}

The `@` symbol is special syntax, and in this case, it means that the `formatF`
function takes an `FList` (`format`) as an input. But because `FList` is a
_custom kind_, it has no value-level inhabitants. So, how can we still get
something whose type mentions `format`? This is what `@` does - it's a proxy
for a type. Its value is isomorphic to `Unit`, and carries no information,
other than its type. Notice that it works for any kind - indeed, proxies are
currently a special-cased type in PureScript, in that they are kind-polymorphic.
{: .notice}

Thus `formatF` takes a format list, and an accumulator string, and returns some
`fun` - this type depends on the actual format list.

Starting with the base case, when there's nothing to print, simply just
return the accumulated formatted string.

{% highlight haskell %}
instance formatFNil :: FormatF FNil String where
  formatF _ str = str
{% endhighlight %}

When the head of the list is `D`, we know that we will need an `Int` argument,
and the rest of the function's type can be computed by recursing on the tail of
the list. As for the implementation, since the return type is now refined to
be of the form `Int -> fun`, we are allowed to construct a lambda that takes
the `Int`, and appends it to the end of the accumulator, then recurses on the
rest. The implementation of `S` is identical, and is omitted for brevity.

{% highlight haskell %}
instance formatFConsD ::
  FormatF rest fun
  => FormatF (FCons D rest) (Int -> fun) where
  formatF _ str
    = \i -> formatF @rest (str <> show i)
{% endhighlight %}

Handling literals (`Lit`) is left as an exercise for the reader.

## Conclusion

Finally, as a matter of convenience, we can wrap the above type classes into
one, that serves as a bridge between the parser and the formatter, as such:

{% highlight haskell %}
class Format (string :: Symbol) fun | string -> fun where
  format :: @string -> fun

instance formatFFormat ::
  ( Parse string format
  , FormatF format fun
  ) => Format string fun where
  format _ = formatF @format ""
{% endhighlight %}

And that's it! It might be instructional to try and work out `FormatF`'s
instance resolution for a few simple examples by hand, to get a better idea why
this works. A fully working implementation of the code in this post can be
found [on github](https://github.com/kcsongor/purescript-safe-printf).
