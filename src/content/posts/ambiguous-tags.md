---
title: "Tripping up type inference"
description: "Improving code readability by enforcing type annotations."
pubDatetime: 2019-09-18T00:00:00Z
modDatetime: 2019-09-18T00:00:00Z
tags: ["haskell"]
legacyId: "https://kcsongor.github.io/2019/09/18/ambiguous-tags"
---


One of the main selling points of Haskell is that despite (or because)
of its strong static type system, it frees us from the burden of having
to spell out tedious type signatures everywhere.

Type inference is a blessing, but sometimes it can also be a
curse. Inference too good can hinder the readability of code, because
the compiler knows what the type of an identifier is even when we
don't. It's not just readability though: correctness
can be imperilled too.

As an example, consider the `Tagged` type, which allows
us to attach type information to some other type.

```haskell
newtype Tagged (s :: k) a = MkTagged a
```

Then we might want to define a `Person` type consisting of a first
name and a last name, both of type `String`, tagged by (type-level)
symbols accordingly:

```haskell
data Person = MkPerson
  (Tagged "firstName" String)
  (Tagged "lastName" String)
```

We can then construct values of this type:

```haskell
joseph :: Person
joseph = MkPerson
  (MkTagged "Joseph")
  (MkTagged "Knecht")
```

And here is the problem. Since both fields are constructed just with
the `MkTagged` constructor, nothing is stopping us from mixing up the
field names if we misremember the ordering:

```haskell
joseph' :: Person
joseph' = MkPerson
  (MkTagged "Knecht")
  (MkTagged "Joseph")
```

We would wish to get a type error, but GHC happily infers that
`MkTagged "Joseph"` indeed has type `Tagged t String` for any `t`,
thus it fits perfectly into the `"lastName"` field.

We can fix this example by providing explicit type applications to
the `MkTagged` constructor. Then, mixing up the order _is_ a type error.

```haskell
joseph' :: Person
joseph' = MkPerson
  (MkTagged @"lastName" "Knecht")
  (MkTagged @"firstName" "Joseph")
```

results in:

```text
    • Couldn't match type ‘"lastName"’ with ‘"firstName"’
```

This works, but these annotations are entirely optional, and if we
forget about them, we're in trouble once again.

To summarise, the problem is that GHC can infer the type of `MkTagged "Joseph"`,
and due to the generality of the result, it can also unify
it with any arbitrary tag.

So the question is this: how do we stop GHC from inferring the type of
expressions like `MkTagged "Joseph"`? In other words, how do we enforce
that the tag must be provided by explicit type annotation?

## An ambiguous smart constructor

We're going to write a smart constructor that can only be invoked by
explicit type annotation of the tag type.

```haskell
mkTagged :: forall t a. a -> Tagged (???) a
mkTagged = MkTagged
```

What to put in the `???` hole? The idea is that we want `t` in this
type to be _ambiguous_, in other words, it should be impossible to
infer `t` even if we know what `Tagged (???) a` is. If it can't be
inferred, then GHC will insist that we specify a type annotation at
the use site for what `t` should be.

The obvious thing to plug into `???` would be `t` itself, but that
doesn't work of course, because from knowing `Tagged t a`, `t` can be
trivially inferred.
For example, when given a value of type `Tagged "firstName" String`,
we can infer that `t` must be `"firstName"`.

As always (at least this seems to be a recurring theme here on my
blog), we reach for type families to solve this problem. In
particular, we define a rather funny-looking variant of the identity
type family, which I'm going to call `Ambiguous`:

```haskell
type family Ambiguous (a :: k) :: j where
  Ambiguous x = x
```

The first thing that might strike you is the kind signature:
`Ambiguous` takes an argument of kind `k`, and returns something of
kind `j`. It helps to think of these kind parameters as additional
_inputs_ to the type family. 

That is, `Ambiguous "firstName"` will get stuck:

```text
>>> :kind! Ambiguous "firstName"
Ambiguous "firstName" :: j
= Ambiguous "firstName"
```

because GHC doesn't know at which `j` we want to evaluate the type
family (and indeed, in principle this choice could change the
behaviour of the type family, since in GHC, type families are not
parametric).

In order to properly reduce the family, we must provide the result
kind as an input, like so:

```text
>>> :kind! (Ambiguous "firstName" :: Symbol)
(Ambiguous "firstName" :: Symbol) :: Symbol
= "firstName"
```

Now let us plug this type family into the type of `mkTagged`, and see what happens.

```haskell
mkTagged :: forall t a. a -> Tagged (Ambiguous t) a
mkTagged = MkTagged
```

Now, when GHC's given `Ambiguous t`, it can't work out what `t`
is. Why? Suppose we know that `Ambiguous t :: Symbol`, that is, we
expect it to reduce to a symbol. That still doesn't tell us anything
about the kind of `t`! According to the kind signature of `Ambiguous`, the
kind of `t` could be _anything_. Indeed, the only way to disambiguate this
is to provide the kind of `t`. As the signature of `mkTagged` does
not have an explicit kind annotation on `t`, the only way to provide
the kind of `t` is to provide `t` itself (since only visibly quantified
variables can be applied with visible type applications).

Now, the following code

```haskell
joseph :: Person
joseph = MkPerson
  (mkTagged "Joseph")
  (mkTagged "Knecht")
```

results in the error:

```text
    • Couldn't match type ‘Ambiguous t0’ with ‘"firstName"’
```

To fix it, we now _must_ provide type applications:

```haskell
joseph :: Person
joseph = MkPerson
  (mkTagged @"firstName" "Joseph")
  (mkTagged @"lastName" "Knecht")
```
