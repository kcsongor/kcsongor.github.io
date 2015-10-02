---
layout: post
title: Time travel in Haskell for dummies
excerpt: "Some interesting consequences of lazy evaluation."
modified: 2015-10-02
tags: [haskell, tutorial, monad, tardis]
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

###### module TimeTravel where

Browsing Hackage the other day, I came across the [Tardis Monad](https://hackage.haskell.org/package/tardis-0.3.0.0/docs/Control-Monad-Tardis.html).
Reading its description, it turns out that the Tardis monad is capable of sending state back in time. Yep. Back in time.

## How?
No, it’s not the reification of [some hypothetical time-travelling particle](https://en.wikipedia.org/wiki/Tachyon), rather a really clever way of exploiting Haskell’s laziness. 

In this rather lengthy post, I’ll showcase some interesting consequences of lazy evaluation and the way to work ourselves up from simple examples to ’time travelling’ craziness through different levels of abstraction.

## The repMax problem

Imagine you had a list, and you wanted to replace all the elements of the list with the largest element, by only passing the list once.
You might say something like "Easier said than done, how do I know the
largest element without having passed the list before?"

Let’s start from the beginning:
First, you ask the future for the largest element of the list, (don’t worry, this will make sense in a bit) let’s call this value `rep` (as in the value we replace stuff with).

Walking through the list, you do two things:

  * replace the current element with rep
  * ’return’ the larger of the current element and the largest element of the remaining list.

When only one element remains, replace it with rep, and return what was there
originally. (this is the base case)

Right, at the moment, we haven’t acquired the skill of seeing the future, so we just write the rest of the function with that bit left out.

{% highlight haskell %}
repMax :: [Int] -> Int -> (Int, [Int])
repMax [] rep = (rep, [])
repMax [x] rep = (x, [rep])
repMax (l : ls) rep = (m’, rep : ls’)
  where (m, ls’) = repMax ls rep
        m’ = max m l
{% endhighlight %}

So, it takes a list, and the rep element, and returns (Int, [Int])

`repMax [1,2,3,4,5,3] 6`
gives us `(5, [6,6,6,6,6,6])` which is exactly what we wanted: the elements are replaced with rep and we also have the largest element.
Now, all we need to do is use that largest element as rep:
  
{% highlight haskell %}
doRepMax :: [Int] -> [Int]
doRepMax xs = xs’
  where (largest, xs’) = repMax xs largest
{% endhighlight %}

## Wait, what?

This can be done thanks to lazy evaluation. Haskell systems use so-called ’thunks’ for values that are yet to be evaluated. When you say `(min 5 6)`, the expression will form a thunk and not be evaluated until it really needs to.
Here, rep can be thought of as a reference to a thunk. When we tell GHC to put `largest` in all slots of the list, it will in fact put a reference to the same thunk in those slots, not the actual data. As we pass the list, this thunk is building up with nested `max` expressions. For `[1,2,3,4]`, will end up with a thunk:
`(max (max (max 1 2) 3) 4)`.
A reference to this thunk will be placed everywhere in the list. By the time we finished traversing the list, the thunk will be finished too, and can be evaluated.

How about generalising this idea to other data structures?

There’s an old saying in the world of lists

> "Everything’s a fold".

Indeed, we could easily rewrite our doRepMax function using a fold:

{% highlight haskell %}
foldmax :: (Ord a, Num a) => [a] -> [a]
foldmax ls = 
  let (ls’, largest) = foldl (\(b, c) a -> (largest : b, max a c)) ([], 0) ls
  in ls’
{% endhighlight %}

Brilliant! Now we can use this technique on everything that is Foldable!
Or can we?

Taking a look at the type signature of the generalised `foldr` (from Data.Foldable):
`Data.Foldable.foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b` we realise that the returned value’s structure `b` is independent from that of the input `t a`. The reason we could get away with this in our fold example was that we knew we were dealing with a list, so we used the `:` operator explicitly to restore the structure.

No problem! There exists a type class that does just what we want, that is it lets us fold it while keeping its structure.
This magical class is called `Traversable`.

{% highlight haskell %}
{-# LANGUAGE DeriveFunctor,
             DeriveFoldable,
             DeriveTraversable #-}

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) 
  deriving (Show, Functor, Foldable, Traversable)
{% endhighlight %}

-- Thankfully, GHC is clever enough to derive Traversable for us from this 
data definiton. (But it wouldn’t be too difficult to do by hand anyway)

Traversable data structures can do a really neat trick (among many others):
`mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)`

This function is like combining a map with a fold (and so all Traversables also need to be Functors and Foldables).
We take a function `(a -> b -> (a, c))`, an initial `a` and a Traversable of `b`s (`t b`).

The elements will be changed with their respective `c`s. (the one calculated by `(a -> b -> (a, c))`)
So `c` is a perfect place for us to put our `rep` (the largest element in this case)

Apart from the final Traversable `t c`, it also returns the accumulated `a`s
(that’s where we return the largest).

{% highlight haskell %}
generalMax :: (Traversable t, Num a, Ord a) => t a -> t a
generalMax t 
  = let (largest, xs’) = mapAccumR (\a b -> (max a b, largest)) 0 t in xs’
{% endhighlight %}

This generalisation gives us new options!
What we’ve been doing so far is we’ve used `a`, `b` and `c` as the same types (as, say Ints). 
For instance, if we want to replace all the elements with the average of them, then we can accumulate the sum and the count of elements in a tuple (`a` will then take the role of this tuple) and `c` will be the sum divided by the count, for which we’re going to ask the future again!

{% highlight haskell %}
generalAvg :: (Traversable t, Integral a) => t a -> t a
generalAvg t = xs’
  where avg = s `div` c
        ((s, c), xs’) 
            = mapAccumR (\(s’, c’) b -> ((s’ + b, c’ + 1), avg)) (0,0) t
{% endhighlight %}

And so on, we can do all sorts of interesting things in a single traversal of our data structures.

### States travelling back in time

### Heading 3

#### Heading 4

##### Heading 5

###### Heading 6

### Body text

Lorem ipsum dolor sit amet, test link adipiscing elit. **This is strong**. Nullam dignissim convallis est. Quisque aliquam.

*This is emphasized*. Donec faucibus. Nunc iaculis suscipit dui. 53 = 125. Water is H2O. Nam sit amet sem. Aliquam libero nisi, imperdiet at, tincidunt nec, gravida vehicula, nisl. The New York Times (That’s a citation). Underline.Maecenas ornare tortor. Donec sed tellus eget sapien fringilla nonummy. Mauris a ante. Suspendisse quam sem, consequat at, commodo vitae, feugiat in, nunc. Morbi imperdiet augue quis tellus.

HTML and CSS are our tools. Mauris a ante. Suspendisse quam sem, consequat at, commodo vitae, feugiat in, nunc. Morbi imperdiet augue quis tellus. Praesent mattis, massa quis luctus fermentum, turpis mi volutpat justo, eu volutpat enim diam eget metus.

### Blockquotes

> Lorem ipsum dolor sit amet, test link adipiscing elit. Nullam dignissim convallis est. Quisque aliquam.

## List Types

### Ordered Lists

1. Item one
   1. sub item one
   2. sub item two
   3. sub item three
2. Item two

### Unordered Lists

* Item one
* Item two
* Item three

## Tables

| Header1 | Header2 | Header3 |
|:--------|:-------:|--------:|
| cell1   | cell2   | cell3   |
| cell4   | cell5   | cell6   |
|----
| cell1   | cell2   | cell3   |
| cell4   | cell5   | cell6   |
|=====
| Foot1   | Foot2   | Foot3
{: rules="groups"}

## Code Snippets

{% highlight haskell %}
#container {
  float: left;
  margin: 0 -240px 0 0;
  width: 100%;
}
{% endhighlight %}

## Buttons

Make any link standout more when applying the `.btn` class.

{% highlight html %}
<a href="#" class="btn btn-success">Success Button</a>
{% endhighlight %}

<div markdown="0"><a href="#" class="btn">Primary Button</a></div>
<div markdown="0"><a href="#" class="btn btn-success">Success Button</a></div>
<div markdown="0"><a href="#" class="btn btn-warning">Warning Button</a></div>
<div markdown="0"><a href="#" class="btn btn-danger">Danger Button</a></div>
<div markdown="0"><a href="#" class="btn btn-info">Info Button</a></div>

## Notices

**Watch out!** You can also add notices by appending `{: .notice}` to a paragraph.
{: .notice}
