---
layout: post
title: Parsing type-level strings in Haskell
excerpt: "A technique for manipulating Symbols in GHC for type-level programming."
modified: 2018-11-28
tags: [haskell, parsing, type-level]
comments: true
---

{% include _toc.html %}

Haskell, as implemented in GHC, has a very rich language for expressing computations in types. Thanks to the
[DataKinds](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=datakinds#datatype-promotion)
extension, any inductively defined data type can be used not only at the term level, but also at the type level.
A notable exception are strings, which provide the main theme for today's blog post.

The `String` type in Haskell is defined as a list of `Char`s. However,
the type-level equivalent, `Symbol`, is defined as a primitive in GHC,
presumably for efficiency. After all, the type checker passes these
types around, and the simpler their structure, the less potential work
the constraint solver needs to do.

The problem is this: since `Symbol` is defined as a primitive, there
is no way to pattern match on its structure, and the only way to
interact with them are by using the built-in primitive operations,
namely appending and (efficient, constant-time) comparison.

In this blog post, I will show how these primitives can be used to
recover the ability to do arbitrary introspection of these type-level
string literals, thereby enabling a whole range of applications where
statically known information can be exploited.

The technique presented here was inspired by Daniel Winograd-Cort's
[pull request for the generic-lens library](https://github.com/kcsongor/generic-lens/pull/69).

All of this is packaged into the
[symbols](https://github.com/kcsongor/symbols) library.

# Motivation

I have [written](/purescript-safe-printf) about type-level symbol
parsing in PureScript to implement a type-safe `printf`
function. (There, I achieved symbol decomposition by patching the
compiler, but no such thing is required here.)

Reusing that example, we will be able to write 
{% highlight haskell %}
>>> :t printf @"Wurble %d %d %s"
printf @"Wurble %d %d %s" :: Int -> Int -> String -> String
{% endhighlight %}

{% highlight haskell %}
>>> printf @"Wurble %d %d %s" 10 20 "foo"
"Wurble 10 20 foo"
{% endhighlight %}

The implementation of the printf example using the technique described in this blog post can be found on
[github](https://github.com/kcsongor/symbols/blob/master/src/Data/Symbol/Examples/Printf.hs).

# Primitives

First, let's have a look at the primitives GHC provides for
manipulating type of kind `Symbol`, namely `AppendSymbol` and
`CmpSymbol`.

These functions are implemented in the compiler, and exported from the
[GHC.TypeLits](http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-TypeLits.html) module:

{% highlight haskell %}
type family AppendSymbol (m :: Symbol) (n :: Symbol) :: Symbol
type family CmpSymbol (m :: Symbol) (n :: Symbol) :: Ordering
{% endhighlight %}

Note that there is no `Uncons` primitive that returns the head (first
character) and the tail of the symbol. It turns out that we can
implement `Uncons` using the two primitives above.

## AppendSymbol

The fact that `AppendSymbol` is a type family suggests a rather
straightforward semantics. It appends two symbols together resulting
in a third one:

{% highlight haskell %}
>>> :kind! AppendSymbol "foo" "bar"
= "foobar"
{% endhighlight %}

That is to say, it should only go in one way, so to speak.

However, if we have a look at the
[implementation](https://github.com/ghc/ghc/blob/1c2c2d3dfd4c36884b22163872feb87122b4528d/compiler/typecheck/TcTypeNats.hs#L835)
in GHC, we can see that there's more going on. There are special rules
for the interaction of `AppendSymbol` constraints with equality
constraints. In concrete terms, GHC will solve the following
constraint:

{% highlight text %}
(AppendSymbol "foo" b ~ "foobar") => (b ~ "bar")
{% endhighlight %}

That is, if we know a prefix of a symbol, we can decompose it to get
the matching suffix. Morally, the actual signature of
`AppendSymbol` would be closer to

{% highlight haskell %}
type family AppendSymbol m n = r | r m -> n, r n -> m
{% endhighlight %}

But this can't be expressed today in GHC (type family dependencies
only allow the inputs to be decided solely by the result, and no
such combination of inputs and outputs are allowed), so `AppendSymbol`
really is a lot more powerful than what the type system would like to admit!

Even with the ability to decompose symbols, there is a problem,
however. This decomposition only works if we _know_ what the prefix
is. And in general, we need to know two out of the three symbols
involved in the constraint to get the third.

As a result, the following won't work:

{% highlight haskell %}
bad :: AppendSymbol prefix suffix ~ "hello world" => Proxy suffix
bad = Proxy
{% endhighlight %}

{% highlight haskell %}
>>> :t bad
bad :: (AppendSymbol prefix suffix ~ "hello world") => Proxy suffix
{% endhighlight %}

that is, `suffix` is unsolved.

We might think that we can just try all possible characters as
potential prefixes until one matches, but that would require
backtracking in the constraint solver, and GHC's constraint solver
doesn't backtrack.

That is, trying a prefix that doesn't match results in an unsolvable
constraint:

{% highlight haskell %}
bad' :: AppendSymbol "a" suffix ~ "hello world" => Proxy suffix
bad' = Proxy
{% endhighlight %}

{% highlight haskell %}
>>> :t bad'
bad' :: (AppendSymbol "a" suffix ~ "hello world") => Proxy suffix
{% endhighlight %}

But since we can't backtrack, there is no way to try a different
character once we've committed to a particular prefix.

_If we knew_ what the first character was, we could strip it off
and get the remaining symbol this way, which would allow us to
treat Symbols as a list of characters essentially.

## CmpSymbol

It turns out that we can simply use alphabetical ordering to find out
what the first character of a string is. `CmpSymbol` compares two symbols,
and returns one of `LT`, `EQ`, or `GT` as a result.

Observe that for any string longer than one, it's always true that the
string follows its first character alphabetically, and precedes any
character after its first one. As an example, consider the string
`"hello world"`, whose first character is `h`, and the letter after
`h` is `i`. Then we have

{% highlight text %}
"h" < "hello world" < "i"
{% endhighlight %}

For strings of length one, they will simply return `EQ` when compared
with their first character (themselves).

# Decomposition

We now put the pieces together to implement an uncons function for
symbols. First, we need `Head`, a function that returns the first
character of a symbol. Second, we will use `Head` to interact with
`AppendSymbol` to retrieve the tail of the symbol. Doing this
repeatedly will allow us to turn a symbol into a list of characters,
which in turn can be consumed by ordinary type families.

## Head

So, to find out what the first character of a symbol is, we just
need to find the last character in the ASCII table that precedes
our symbol. To do this reasonably efficiently, we use binary search.
Since indexing into a type-level list takes linear time, we use a
balanced binary search tree instead. Recall that symbol comparisons
are constant-time, so the whole operation is constant time (as we're
working with a fixed size alphabet), so this optimisation simply
improves the constant factor by an order of magnitude.

{% highlight haskell %}
data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show
{% endhighlight %}

The printable subset of the ASCII character set can be encoded as the
following tree:

{% highlight haskell %}
type Chars
 = 'Node
  ('Node
  ('Node
    ('Node
      ('Node
        ('Node ('Node 'Leaf '(" ", "!") 'Leaf) '("!", "\"") 'Leaf)
        '("\"", "#")
        ('Node ('Node 'Leaf '("#", "$") 'Leaf) '("$", "%") 'Leaf))
      '("%", "&")
      ('Node
        ('Node ('Node 'Leaf '("&", "'") 'Leaf) '("'", "(") 'Leaf)
        '("(", ")")
        ('Node ('Node 'Leaf '(")", "*") 'Leaf) '("*", "+") 'Leaf)))
    '("+", ",")
    ('Node
      ('Node
        ('Node ('Node 'Leaf '(",", "-") 'Leaf) '("-", ".") 'Leaf)
        '(".", "/")
        ('Node ('Node 'Leaf '("/", "0") 'Leaf) '("0", "1") 'Leaf))
      '("1", "2")
      ('Node
        ('Node ('Node 'Leaf '("2", "3") 'Leaf) '("3", "4") 'Leaf)
        '("4", "5")
        ('Node ('Node 'Leaf '("5", "6") 'Leaf) '("6", "7") 'Leaf))))
  '("7", "8")
  ('Node
    ('Node
      ('Node
        ('Node ('Node 'Leaf '("8", "9") 'Leaf) '("9", ":") 'Leaf)
        '(":", ";")
        ('Node ('Node 'Leaf '(";", "<") 'Leaf) '("<", "=") 'Leaf))
      '("=", ">")
      ('Node
        ('Node ('Node 'Leaf '(">", "?") 'Leaf) '("?", "@") 'Leaf)
        '("@", "A")
        ('Node ('Node 'Leaf '("A", "B") 'Leaf) '("B", "C") 'Leaf)))
    '("C", "D")
    ('Node
      ('Node
        ('Node ('Node 'Leaf '("D", "E") 'Leaf) '("E", "F") 'Leaf)
        '("F", "G")
        ('Node ('Node 'Leaf '("G", "H") 'Leaf) '("H", "I") 'Leaf))
      '("I", "J")
      ('Node
        ('Node ('Node 'Leaf '("J", "K") 'Leaf) '("K", "L") 'Leaf)
        '("L", "M")
        ('Node ('Node 'Leaf '("M", "N") 'Leaf) '("N", "O") 'Leaf)))))
  '("O", "P")
  ('Node
  ('Node
    ('Node
      ('Node
        ('Node ('Node 'Leaf '("P", "Q") 'Leaf) '("Q", "R") 'Leaf)
        '("R", "S")
        ('Node ('Node 'Leaf '("S", "T") 'Leaf) '("T", "U") 'Leaf))
      '("U", "V")
      ('Node
        ('Node ('Node 'Leaf '("V", "W") 'Leaf) '("W", "X") 'Leaf)
        '("X", "Y")
        ('Node ('Node 'Leaf '("Y", "Z") 'Leaf) '("Z", "[") 'Leaf)))
    '("[", "\\")
    ('Node
      ('Node
        ('Node ('Node 'Leaf '("\\", "]") 'Leaf) '("]", "^") 'Leaf)
        '("^", "_")
        ('Node ('Node 'Leaf '("_", "`") 'Leaf) '("`", "a") 'Leaf))
      '("a", "b")
      ('Node
        ('Node ('Node 'Leaf '("b", "c") 'Leaf) '("c", "d") 'Leaf)
        '("d", "e")
        ('Node ('Node 'Leaf '("e", "f") 'Leaf) '("f", "g") 'Leaf))))
  '("g", "h")
  ('Node
    ('Node
      ('Node
        ('Node ('Node 'Leaf '("h", "i") 'Leaf) '("i", "j") 'Leaf)
        '("j", "k")
        ('Node ('Node 'Leaf '("k", "l") 'Leaf) '("l", "m") 'Leaf))
      '("m", "n")
      ('Node
        ('Node ('Node 'Leaf '("n", "o") 'Leaf) '("o", "p") 'Leaf)
        '("p", "q")
        ('Node ('Node 'Leaf '("q", "r") 'Leaf) '("r", "s") 'Leaf)))
    '("s", "t")
    ('Node
      ('Node
        ('Node ('Node 'Leaf '("t", "u") 'Leaf) '("u", "v") 'Leaf)
        '("v", "w")
        ('Node ('Node 'Leaf '("w", "x") 'Leaf) '("x", "y") 'Leaf))
      '("y", "z")
      ('Node
        ('Node ('Node 'Leaf '("z", "{") 'Leaf) '("{", "|") 'Leaf)
        '("|", "}")
        ('Node ('Node 'Leaf '("}", "~") 'Leaf) '("~", "~") 'Leaf)))))

{% endhighlight %}

(I generated this structure with the help of other type families, but
found that inlining the result into the source file results in much
faster lookups.)

Note that each node contains two consecutive characters: this is so
that we can easily decide when to stop: when the first element is less
than, and the second element is greater than our input string.

The `Lookup` type family (and `Lookup2`, to make up for a lack of local
declarations in type families) implements a standard binary search.

{% highlight haskell %}
type LookupTable = Tree (Symbol, Symbol)

type family Lookup (x :: Symbol) (xs :: LookupTable) :: Symbol where
  Lookup x (Node l '(cl, cr) r)
    = Lookup2 (CmpSymbol cl x) (CmpSymbol cr x) x cl l r

type family Lookup2 ol or x cl l r :: Symbol where
  Lookup2 'EQ _ _ cl _ _     = cl -- character matches
  Lookup2 'LT 'GT _ cl _ r   = cl -- found the right node
  Lookup2 'LT _ _ cl _ 'Leaf = cl -- we're at the rightmost node (~)
  Lookup2 'LT _ x _ _ r      = Lookup x r -- go right
  Lookup2 'GT _ x _ l _      = Lookup x l -- go left

{% endhighlight %}

Finally, `Head` is just a lookup in the binary tree.

{% highlight haskell %}
type Head sym = Lookup sym Chars
{% endhighlight %}

{% highlight text %}
>>> :kind! Head "Wurble"
= "W"
{% endhighlight %}

## Uncons

Next, we need to interact the `AppendSymbol` constraint with
`Head`. We now turn to a type class, `Uncons`:

{% highlight haskell %}
class Uncons (sym :: Symbol) (h :: Symbol) (t :: Symbol) where
  uncons :: Proxy '(h, t)
{% endhighlight %}

`sym` is our symbol, `h` is the head, and `t` is the tail.  It would
be nice to have a functional dependency `sym -> h t`, but
unfortunately we can't make that pass, as recall that the backwards
dependencies of `AppendSymbol` are essentially hidden from the type
system.

We write a single instance, which sets up the right constraints:

{% highlight haskell %}
instance ( h ~ Head sym
	 , AppendSymbol h t ~ sym
	 ) => Uncons sym h t where
  uncons = Proxy
{% endhighlight %}

First, we write `h ~ Head sym`, which unifies `h` with the first
element of the symbol using the binary lookup defined
previously. Then, the `AppendSymbol h t ~ sym` constraint will trigger
the solution of `t`, due to the now known prefix `h`.

The `uncons` member is not necessary for things to work out, but it
helps illustrate the working of the type class in the REPL:

{% highlight text %}
>>> :t uncons @"foo"
uncons @"foo" :: Proxy '("f", "oo")
{% endhighlight %}

Finally, we can write the `Listify` class to recursively break down a
symbol into a list of characters:

{% highlight haskell %}
class Listify (sym :: Symbol) (result :: [Symbol]) where
  listify :: Proxy result

instance {-# OVERLAPPING #-} nil ~ '[] => Listify "" nil where
  listify = Proxy

instance ( Uncons sym h t
 	 , Listify t result, result' ~ (h ': result)
	 ) => Listify sym result' where
  listify = Proxy
{% endhighlight %}

{% highlight text %}
>>> :t listify @"Hello"
listify @"Hello" :: Proxy '["H", "e", "l", "l", "o"]
{% endhighlight %}

And with this, we can parse anything we'd like.

# Conclusion

Of course all of the above could be done a lot more efficiently with
compiler support, and there's no reason for that not to happen at some
point in the future. This post is just a proof of concept that
something like this is already possible today, and the presented
technique is suitable for some lightweight applications. For anything
larger scale, Template Haskell is probably much better suited for the
job today.
