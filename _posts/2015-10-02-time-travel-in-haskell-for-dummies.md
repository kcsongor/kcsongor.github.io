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

Browsing Hackage the other day, I came across the [Tardis
Monad](https://hackage.haskell.org/package/tardis-0.3.0.0/docs/Control-Monad-Tardis.html).
Reading its description, it turns out that the Tardis monad is capable of
sending state back in time. Yep. Back in time.

## How?
No, it’s not the reification of [some hypothetical time-travelling
particle](https://en.wikipedia.org/wiki/Tachyon), rather a really clever way of
exploiting Haskell’s laziness. 

In this rather lengthy post, I’ll showcase some interesting consequences of
lazy evaluation and the way to work ourselves up from simple examples to ’time
travelling’ craziness through different levels of abstraction.

## The repMax problem

Imagine you had a list, and you wanted to replace all the elements of the list
with the largest element, by only passing the list once.  You might say
something like "Easier said than done, how do I know the largest element
without having passed the list before?"

Let’s start from the beginning:
-- First, you ask the future for the largest element of the list, (don’t worry,
this will make sense in a bit) let’s call this value `rep` (as in the value we
replace stuff with).

Walking through the list, you do two things:

  * replace the current element with `rep`
  * ’return’ the larger of the current element and the largest element of the remaining list.

When only one element remains, replace it with `rep`, and return what was there
originally. (this is the base case)

Right, at the moment, we haven’t acquired the skill of seeing the future, so we
just write the rest of the function with that bit left out.

{% highlight haskell %}
repMax :: [Int] -> Int -> (Int, [Int])
repMax [] rep = (rep, [])
repMax [x] rep = (x, [rep])
repMax (l : ls) rep = (m', rep : ls')
  where (m, ls') = repMax ls rep
        m' = max m l
{% endhighlight %}

So, it takes a list, and the rep element, and returns (Int, [Int])

`repMax [1,2,3,4,5,3] 6`
gives us `(5, [6,6,6,6,6,6])` which is exactly what we wanted: the elements are
replaced with rep and we also have the largest element.
Now, all we need to do is use that largest element as `rep`:
  
{% highlight haskell %}
doRepMax :: [Int] -> [Int]
doRepMax xs = xs'
  where (largest, xs') = repMax xs largest
{% endhighlight %}

## Wait, what?

This can be done thanks to lazy evaluation. Haskell systems use so-called
’thunks’ for values that are yet to be evaluated. When you say `(min 5 6)`, the
expression will form a thunk and not be evaluated until it really needs to be.
Here, rep can be thought of as a reference to a thunk. When we tell GHC to put
`largest` in all slots of the list, it will in fact put a reference to the same
thunk in those slots, not the actual data. As we pass the list, this thunk is
building up with nested `max` expressions. For `[1,2,3,4]`, will end up with a
thunk: `(max (max (max 1 2) 3) 4)`.
A reference to this thunk will be placed everywhere in the list. By the time we
finished traversing the list, the thunk will be finished too, and can be
evaluated.

How about generalising this idea to other data structures?

There’s an old saying in the world of lists

> "Everything’s a fold".

Indeed, we could easily rewrite our doRepMax function using a fold:

{% highlight haskell %}
foldmax :: (Ord a, Num a) => [a] -> [a]
foldmax ls = ls'
  where 
    (ls', largest) 
      = foldl (\(b, c) a -> (largest : b, max a c)) ([], 0) ls
{% endhighlight %}

Brilliant! Now we can use this technique on everything that is Foldable!
Or can we?

Taking a look at the type signature of the generalised `foldl` (from
Data.Foldable): `Data.Foldable.foldl :: Foldable t => (b -> a -> b) -> b -> t a
-> b` we realise that the returned value’s structure `b` is independent from
that of the input `t a`. The reason we could get away with this in our fold
example was that we knew we were dealing with a list, so we used the `:`
operator explicitly to restore the structure.

No problem! There exists a type class that does just what we want, that is it
lets us fold it while keeping its structure.
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

This function is like combining a map with a fold (and so all Traversables also
need to be Functors and Foldables).  We take a function `(a -> b -> (a, c))`,
an initial `a` and a Traversable of `b`s (`t b`).

The elements will be changed with their respective `c`s. (the one calculated by
`(a -> b -> (a, c))`) So `c` is a perfect place for us to put our `rep` (the
largest element in this case)

Apart from the final Traversable `t c`, it also returns the accumulated `a`s
(that’s where we return the largest).

{% highlight haskell %}
generalMax :: (Traversable t, Num a, Ord a) => t a -> t a
generalMax t = xs'
  where
    (largest, xs')
      = mapAccumR (\a b -> (max a b, largest)) 0 t
{% endhighlight %}

This generalisation gives us new options!  What we’ve been doing so far is
we’ve used `a`, `b` and `c` as the same types (as, say Ints).

For instance, if we want to replace all the elements with the average of them,
then we can accumulate the sum and the count of elements in a tuple (`a` will
then take the role of this tuple) and `c` will be the sum divided by the count,
for which we’re going to ask the future again!

{% highlight haskell %}
generalAvg :: (Traversable t, Integral a) => t a -> t a
generalAvg t = xs'
  where 
    avg = s `div` c
    ((s, c), xs') 
      = mapAccumR (\(s', c') b -> ((s' + b, c' + 1), avg)) (0,0) t
{% endhighlight %}

And so on, we can do all sorts of interesting things in a single traversal of
our data structures.

## States travelling back in time

--------------------------------------------------------------------------------

##### What are states anyway?
In Haskell, whenever we want to write functions that operate on some sort
of environment or state, we write these functions in the following form:
statefulFunction :: b -> c -> d -> s -> (a, s)
that is, we take some arguments (`b`, `c`, `d` here), a state `s`, and
return a new, possibly modified state along with some value `a`.
Now, this involves writing a lot of boilerplate code, both in the type
signatures and in the actual code that is using the state.

For example, using the state as a counter:
{% highlight haskell %}
statefulFunction arg1 arg2 arg3 counter =
  (counter + 1, arg1 + arg2 + arg3)
{% endhighlight %}

{% highlight haskell %}
bindStatefulFunctions :: 
  (s -> (a, s)) ->
  (a -> s -> (b, s)) ->
  s -> (b, s)
bindStatefulFunctions f1 f2 = \initialState ->
  let (updateState, result) = f1 initialState
  in f2 result updatedState
{% endhighlight %}

Note that f2 takes an extra `a`, that’s the output of the first function.
That’s why this function is called bind, we bind the output of the first
function to the input of the second while passing the modified state.

The State monad essentially does something like the above code, but hides
it all and makes the state passing implicit. Also, being a monad, gives us
the all so convenient do notation!

`State s a` is basically just a type synonym for `s -> (a, s)`, so our
previous example could be written as
`statefulFunction :: b -> c -> d -> State s a`

and bindStatefulFunctions we get for free from State (known as `>>=` for monads)

Now we can do:
{% highlight haskell %}
statefulFunction arg1 arg2 arg3 = do
  counter <- get
  put (counter + 1)
  return (arg1 + arg2 + arg3)
{% endhighlight %}

(Did you know that Haskell is also the best imperative language?)
Notice how the state is not explicitly passed as an argument (thus our
function is partially applied), but is bound to counter by the get function.
Put then puts the updated counter back in the state.

--------------------------------------------------------------------------------

The nice thing about the State monad is that all the computations we do
within it are essentially just partially applied functions, so they can’t be
evaluated until provided with an initial state, which will then magically
flow through the pipeline of computations, each doing their respective
modifications in the meantime.

`mapAccumR` does a series of stateful computations (in nature, but it’s not
using the State monad), where it takes a value and a state, then returns a
new value with a modified state. (Accum refers to the fact that this state 
can be used as an accumulator as we traverse the data)

`mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)`

`a` is that state here, that is what we used to store the largest element.
This state, however, travels forward in time, so to speak, as we go through
the list. The trick we do only happens at the end, when we feed it its own
output. We can do so thanks to lazy evaluation.

So the State monad passes its `s` from computation to computation, that’s
how these computations are bound.

Imagine using the same laziness self-feeding trick, but for passing the
state:

{% highlight haskell %}
reverseBind stateful1 stateful2 = \s ->
  (x', s'')
  where (x, s'') = stateful1 s'
        (x', s') = stateful2 x s
{% endhighlight %}

So first we run stateful1 **with the state modified by stateful2**!
Then we run stateful2 with stateful1's output. Finally, we return the
state after running stateful1 along with the value `x'` from stateful2.
Note that because of the way this binding is done, stateful1’s ouput state
will actually be the *past* of stateful1. (That is, whatever we do with the
state in stateful1, will be visible to the computations preceding stateful1,
just like how stateful2’s effects are seen in stateful1. Lazy evaluation
rocks!)

Coming from an imperative background, this can be thought of as stateful1
putting forward references to the values it uses from the state, and once
those values are actually calculated in the future, stateful1 will be able
to do whatever it wanted. These references are not explicit though as they
would be in C (using pointers, for example), but implicitly placed there
by GHC as thunks.

That also means whatever do with these values has to be done lazily. (an
example below)

The above code is a modified version of the monadic binding found in the
rev-state package (which is in turn a modification of the original State
monad by reversing the flow of state).
{: .notice}

## Finally, the time machine, TARDIS

So we have the State monad, of which the state flows forwards, then we have
the Rev-State, which sends the state backwards. So what do we get if we
combine these two? Yes, a time machine! Also known as the Tardis monad: it
is in fact a combination of the State and Rev-State monads with some nice
functions to deal with the bidirectional states.

I say states, because naturally, we have data coming from the future and
data coming from the past, and those make two (a backwards travelling and a
forwards travelling state).

These could be of different types, say we can say Strings back in time and
Ints to the future.

### A single-pass assembler: an example

Writing an assembler is relatively straightforward. We go through a list of
assembly instructions and turn them into their binary equivalent for the given
CPU architecture.

However, there are some instructions that we can’t immediately convert.
One of such instructions is a label for branching. (jumps)
For these labels, we need a symbol table. 
{% highlight haskell %}
import qualified Data.Map.Strict as M

type Addr = Int
type SymTable = M.Map String Addr -- map label names to their addresses


data Instr = Add
            | Mov
            | ToLabel String
            | ToAddr Addr
            | Label String
            | Err
            deriving (Show)
{% endhighlight %}

`Instr` is a rather rudimentary representation of assembly instructions, but it
does the job for us now.

What we want to have is a function that takes a list of `Instr`s and returns
a list of `[(Addr, Instr)]` and also replace all the `ToLabel`s with `ToAddr`s
that point to the address of the label. If the label is never defined, we
put an `Err` there. (In real life, you would use some ExceptT monad transformer
to handle such errors.)

{% highlight haskell %}
runAssembler :: [Instr] -> [(Addr, Instr)]
{% endhighlight %}

Jumping to a label that is already defined is easy, we look it up in our
SymTable and convert `ToLabel` to `ToAddr`. This sounds like an application
of the State monad, doesn’t it? 
When we encounter a label definition, just add it to the state (`SymTable`).
Done!

The problem arises from the fact that some labels might be defined after they
are used. The 'else' block of an if statement will typically be done like this.
Implementing this in C, you could remember these positions and at the end, fill
in the gaps with the knowledge you have acquired. Thunks, anyone?

I’ll just use a Rev-State monad and send these definitions back in time.
Simple enough, right?

So at this point, we can see that we will need both types of these states:
one that’s travelling forward and one that is going backwards. And that is
exactly what the Tardis monad is!

Labels will not be turned into any binary, instead
the next actual instruction’s address will be used.
{: .notice}

{% highlight haskell %}
type Assembler a = Tardis SymTable SymTable a
{% endhighlight %}

Right, our `runAssembler` function will run some `assemble` function
in the Tardis monad. (That is, it will give it the initial states and extract
the final value at the end).

{% highlight haskell %}
runAssembler asm = instructions
  where (instructions, _)
          = runTardis (assemble 0 asm) (M.empty, M.empty)
{% endhighlight %}

The `assemble` function turns a list of instructions to `[(Addr, Instr)]`
in the Assembler monad (which is a synonym for Tardis SymTable SymTable).
What’s that 0 doing there, you ask?

We need to keep track of the address we will use for the next instruction.
This is because of labels. When we encounter a regular instruction, we put
that at the provided address, then increment that address by 1. If a label
comes around, we put it in the State then continue without incrementing the
address.

{% highlight haskell %}
assemble :: Addr -> [Instr] -> Assembler [(Addr, Instr)]
assemble _ [] = return []
-- label found, update state then go on
assemble addr (Label label : is') = do
  modifyBackwards (M.insert label addr) -- send to past
  modifyForwards (M.insert label addr)  -- send to future
  assemble addr is' -- assemble the rest of the instructions
-- jump to label found, replace with
-- jump to address
-- then do the rest starting at (addr + 1)
assemble addr (ToLabel label : is') = do
  bw <- getFuture
  fw <- getPast
  let union = M.union bw fw -- take union of the two symbol tables
      this = case (M.lookup label union) of
                Just a' -> (addr, ToAddr a')
                Nothing -> (addr, Err)
  rest <- assemble (addr + 1) is'
  return $ this : rest
-- regular instruction found,
-- assign it to the address
-- then do the rest starting at (addr + 1)
assemble addr (instr : is') = do
  rest <- assemble (addr + 1) is'
  return $ (addr, instr) : rest
{% endhighlight %}

Now we come up with some test instructions:

{% highlight haskell %}
input :: [Instr]
input = [Add,
         Add,
         ToLabel "my_label",
         Mov,
         Mov,
         Label "my_label",
         Label "second_label",
         Mov,
         ToLabel "second_label",
         Mov]
{% endhighlight %}

...and we can try running the assembler on this data:

`> runAssembler input`

`> [(0,Add),(1,Add),(2,ToAddr 5),(3,Mov),(4,Mov),(5,Mov),(6,ToAddr 5),(7,Mov)]`

Yay! Just what we wanted!

### IO doesn’t mix with the future! (The past is fine)
Be careful about what you do with the state coming from the future. 
Everything has to be lazily passed through.

You might be tempted to use the TardisT monad transformer to interleave IO
effects in your time-travelling code. Most IO computations, however are
strict.

Let’s say you want to get the label from the future and print its address.
IO’s print will try to evaluate its argument (which is a partial thunk at this
point). It will block the thread until the evaluation is completed, which will
result in the program breaking, as the thread block prevents it from
progressing further. In this case, I’d advise the use of a Writer monad which
has a lazy mechanism, and the results can be printed at the end using IO.

### Thanks
Thanks for reading this lengthy post, in which we saw how we can mimic the use
of pointers in pure Haskell code (altough time-travel is an arguably better
name for this). This comes at a price though: accumulating 
unevaluated thunks can use up quite a bit of memory, so be careful if you want 
to use these techniques in a memory critical environment.

If you find any bugs or mistakes, please make sure to let me know!
