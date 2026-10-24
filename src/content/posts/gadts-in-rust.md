---
title: "Trait-Constrained Enums in Rust"
description: "Simulating Haskell-style GADTs with phantom witnesses and specialisation."
pubDatetime: 2025-06-10T00:00:00Z
modDatetime: 2025-11-07T00:00:00Z
tags: ["rust", "gadts", "type-level-programming", "specialisation"]
legacyId: "https://kcsongor.github.io/2025/06/10/gadts-in-rust"
---




Rust doesn’t have GADTs (generalised algebraic data types), but we can get
surprisingly close with some creative type-level tricks.

This post might look like a departure from my usual (in the sense of _typical_,
not _frequent_) Haskell posts since we'll be writing Rust today. Don’t let that
fool you; we’ll just be writing Haskell in Rust.

GADTs are a Haskell feature that let
constructors carry richer type information. They can enforce constraints or
refine type parameters per constructor -- which is what we'll achieve here in
Rust.

As this post is mainly for Rust programmers, I’ll start by motivating why GADTs
are useful.
For that, we’ll build a small expression language and see where plain algebraic
data types fall short.
Then we’ll introduce GADTs to fix the problem, first through type refinement and
then with per-constructor constraints.
After that, we’ll move to Rust and reconstruct both mechanisms: type equality
witnesses (a known trick) and constraint witnesses (the new bit this post is
really about). You’ll know when we’ve switched from Haskell to Rust — the syntax
gets ugly.

## A simple expression language

Let's start with a small expression language, encoded as a Haskell datatype. It
supports defining integer literals, and adding them together.

```haskell
data Expr
  = LitInt Int
  | Add Expr Expr
```

The Rust equivalent of this is an enum with two constructors and more parentheses
(and explicit heap indirection).

We can evaluate expressions recursively:

```haskell
eval :: Expr -> Int
eval (LitInt n) = n
eval (Add a b) = eval a + eval b
```

Evaluating `eval (Add (LitInt 3) (Add (LitInt 4) (LitInt 5)))` yields `12`.

This is not a very useful expression language, so let's add another literal type
and another binary operator:

```haskell
data Expr
  = LitInt Int
  | Add Expr Expr
  | LitBool Bool
  | Or Expr Expr
```

Now we can write expressions like `Add (LitInt 1) (LitInt 2)` and `Or (LitBool False) (LitBool True)`.
But we can also write `Add (LitInt 1) (LitBool False)`, which shouldn’t
type-check!

Worse, we're in trouble when writing the return type of `eval :: Expr -> ???`.
What it should return depends on the input, but the input type doesn't contain
enough information.

## More precise types with GADTs

GADTs let us say more about each constructor's result type.
We can extend our `Expr` definition so that `Add` only exists for integers, and
`Or` only for booleans.

```haskell
{-# LANGUAGE GADTs #-}

data Expr a where
  LitInt :: Int -> Expr Int
  Add :: Expr Int -> Expr Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Or :: Expr Bool -> Expr Bool -> Expr Bool
```

Notice that `Expr a` is now _parameterised_ (this would read `Expr<A>` in Rust),
and each constructor specifies the type of expression it builds.  `LitInt` takes
an `Int` and produces an `Expr Int`, and `Add` combines two `Expr Int`s into
another `Expr Int`.

As a result, `Add (LitInt 1) (LitBool False)` is rejected at compile time
because the second operand has the wrong type.

The evaluation function can now have a precise type:

```haskell
eval :: Expr a -> a
eval (LitInt n) = n
eval (Add a b) = eval a + eval b
eval (LitBool b) = b
eval (Or a b) = eval a || eval b
```

`eval` now takes an expression of any type, and returns a value of that type.
When pattern matching on `Expr a`, if we see a `LitInt`, we learn that `a` is
`Int`, so the result must be an integer.
In the `Add` branch, both sub-expressions are `Expr Int`, so `eval` produces two
`Int`s which can be added together.

In other words, we not only *restrict* what types of expressions can be used
when constructing `Add`, but also *learn* type information when destructuring
it.

So far, every constructor fixes a concrete return type.
But what if we wanted to support other types that can also be added together?

## More flexible types with GADTs

Let's say we want to support `Double`s in our language too:

```haskell
data Expr a where
  LitInt :: Int  -> Expr Int
  LitDouble :: Double  -> Expr Double
  Add :: Expr Int -> Expr Int  -> Expr Int
  ...
```

Doubles, being numeric values, can also be added together, but the current `Add`
constructor only works on integers. We can relax this by constraining the `a`
type parameter just in this constructor:

```haskell
data Expr a where
  LitInt     :: Int  -> Expr Int
  LitDouble  :: Double -> Expr Double
  Add        :: Num a => Expr a -> Expr a -> Expr a
  ...
```

The `Num a =>` part is a *type class constraint* in Haskell, equivalent to a
trait bound in Rust.
In other words, `Add` can now take any two expressions of the same type, as long as that type
supports numeric operations.

The `eval` function simply gains one extra case:

```haskell
eval :: Expr a -> a
eval (LitInt n) = n
eval (LitDouble n) = n
eval (Add a b) = eval a + eval b
eval (LitBool b) = b
eval (Or a b) = eval a || eval b
```

`Add` now supports `Add (LitDouble 1.0) (LitDouble 2.0)` and `Add (LitInt 1) (LitInt 2)`.
Crucially, the `Num a` constraint is attached to just the `Add` constructor, not
the entire type. It's still possible to construct `LitBool` values, even though
booleans don't support addition.

Each `Add` value carries evidence that its type parameter `a` satisfies `Num`.
When we pattern match on `Add`, the type checker brings that constraint into
scope automatically, allowing us to use `(+)` in the corresponding branch.

This is a subtle but powerful idea: constraints can be local to a constructor.
Even the precise return types we saw earlier are another form of locality —
`LitInt` locally records that `a` is equal to `Int`.

Next, we’ll rebuild the expression language in Rust, and see how to emulate both
of these features: constructor-local type equalities and constructor-local
constraints.
To start adopting the Rust nomenclature, we’ll build an enum whose constructors
are trait-constrained.

## Encoding GADTs in Rust

We'll be encoding both properties of GADTs:

1. **Constructor-local type equalities** — like `LitInt` refining `a ~ Int`.
2. **Constructor-local constraints** — like `Add` requiring `Num a`.

We'll start with the first one, since the idea is already well known in the Rust community.

As a baseline, here's the simple expression language, with the promised
parentheses and heap indirections:
```rust
enum Expr {
    LitInt(i64),
    Add(Box<Expr>, Box<Expr>),
    LitBool(bool),
    Or(Box<Expr>, Box<Expr>),
}
```

As things stand, we have the same issues as the original Haskell version, namely that we can construct invalid combinations, and can't give a precise type to `eval`.

### Type equality witnesses

In Haskell, specifying the return type of a GADT allowed us to express a type
equality which the typechecker could then automatically use to unify the type
variable with the concrete type.

This relies on the type checker’s ability to make progress with locally learned
information, which Rust doesn’t natively support. Our encoding will instead
rely on an explicit *witness* of type equality, which we then use where Haskell
would use the GADT constraint.

In Rust, we can encode this concept as a zero-sized type:  [^is-invariance]

```rust
use core::marker::PhantomData;

struct Is<A, B>(PhantomData<(A, B)>);

impl<A> Is<A, A> {
    fn refl() -> Self {
        Is(PhantomData)
    }
}
```

[^is-invariance]: Using `PhantomData<fn(A) -> B>` would make `A` and `B` invariant, which is slightly more robust if lifetimes are involved. For this post, `PhantomData<(A, B)>` is simpler and works fine.

`Is<A, B>` is our equality witness: a value of type `Is<A, B>` is only constructible when `A` and
`B` are equal. `refl` is the only _safe_ way to construct values of type `Is`.

If you’ve seen this trick before, you can safely skim this part. The more
interesting bit is how to do the same thing for **trait bounds**, not just type
equalities.

We can now write `Expr<A>` where each constructor stores a type equality witness:

```rust
enum Expr<A> {
    LitInt(Is<i64, A>, i64),
    Add(Is<i64, A>, Box<Expr<i64>>, Box<Expr<i64>>),
    LitBool(Is<bool, A>, bool),
    Or(Is<bool, A>, Box<Expr<bool>>, Box<Expr<bool>>),
}
```

`Expr::LitInt(Is::refl(), 42)` has type `Expr<i64>`, because the `refl()` constructor forces the `A` variable to unify with `i64`.

So
```rust
Expr::Add(
    Is::refl(),
    Box::new(Expr::LitInt(Is::refl(), 1)),
    Box::new(Expr::LitInt(Is::refl(), 2)),
)
```
typechecks, but
```rust
Expr::Add(
    Is::refl(),
    Box::new(Expr::LitInt(Is::refl(), 1)),
    // wrong type, expected an Expr<i64> but got Expr<bool>
    Box::new(Expr::LitBool(Is::refl(), false)),
)
```
doesn't.

This machinery allows us to *restrict* the types of expressions that can be used
in `Add`, but how do we *learn* type information?

```rust
fn eval<A>(expr: Expr<A>) -> A {
    match expr {
        Expr::LitInt(p, n) => ??? // n is of type 'i64', we need to return 'A'
        ...
    }
}
```

In the Haskell version, the type equality bound by the GADT constructor is a
native language feature that the typechecker knows about, so it freely converts
between `a` and `Int` under a pattern match.

In Rust, we created a custom encoding of type equality, and the typechecker
doesn't (and shouldn't, in general) use it to unify types.

This means that we need to write a function that actually performs the conversion:

```rust
impl<A, B> Is<A, B> {
    fn convert(self, a: A) -> B {
        unsafe { std::intrinsics::transmute_unchecked(a) }
    }
}
```

`transmute_unchecked` is a very unsafe function in general, but in our case, we
only invoke it when we have a type equality witness available (which can only be
constructed via `refl`), so we *know* the types `A` and `B` are actually equal.

With this, we can now use the equality witnesses in the constructors to rewrite
the results into the desired `A`:

```rust
fn eval<A>(expr: Expr<A>) -> A {
    match expr {
        Expr::LitInt(p, n) => p.convert(n), // i64 -> A
        Expr::Add(p, left, right) => p.convert(eval(*left) + eval(*right)),
        Expr::LitBool(p, b) => p.convert(b),
        Expr::Or(p, left, right) => p.convert(eval(*left) || eval(*right)),
    }
}
```

### Trait constraint witnesses

The type equality witnesses from the previous section are relatively simple,
because the only thing we need to record about our type parameter is that it's
equal to a known type in the local context.

Trait implementations are more complicated, because we need to know how certain
functionality is implemented for our type parameter.

Haskell's GADTs store references to type class dictionaries in their
constructors -- essentially dynamic dispatch.
While Rust supports dynamic dispatch via `dyn Trait`, it's severely limited
(requiring "object safe" traits), so we'll need a different approach.

We'll start with a similar witness idea, but this time, the witness will record
the fact that a trait implementation exists for a type.

We’ll define a witness for the existence of a `Add`-like capability, corresponding
to the `Num` constraint in the Haskell version.

```rust
struct CanAdd<T: ?Sized> {
    _phantom: PhantomData<T>,
}

impl<T: std::ops::Add<Output = T>> CanAdd<T> {
    fn new() -> Self {
        CanAdd { _phantom: PhantomData }
    }
}

fn can_add<T: std::ops::Add<Output = T>>() -> CanAdd<T> {
    CanAdd::new()
}
```

`CanAdd<T>` can only be constructed (via `can_add`) if `T` supports the `Add` operation
with result type `T`. This mirrors the `Num a =>` constraint on the Haskell side.

We can now extend our expression type with a constructor that carries this
constraint witness:

```rust
enum Expr<A> {
    LitInt(Is<i64, A>, i64),
    LitDouble(Is<f64, A>, f64),
    Add(CanAdd<A>, Box<Expr<A>>, Box<Expr<A>>),
    LitBool(Is<bool, A>, bool),
    Or(Is<bool, A>, Box<Expr<bool>>, Box<Expr<bool>>),
}
```

This version of `Expr` is the Rust analogue of the final Haskell GADT.
The `Add` constructor now carries a `CanAdd<A>` witness that proves
`A` implements `Add<Output = A>`.

So far this handles the *construction* side of the story, but not the
*destruction* side. When we pattern match on an `Expr<A>`, Rust doesn’t know
that `A` satisfies the constraint carried by `CanAdd<A>`.

```rust
fn eval<A>(expr: Expr<A>) -> A {
    match expr {
        ...
        Expr::Add(w, a, b) => eval(*a) + eval(*b), // cannot add `A` to `A`
        ...
    }
}
```

To recover that information, we’ll need to encode it in a trait that can
selectively enable the operation based on the presence of a witness.

### Using specialisation to recover constraints

We now want to *use* the information stored in `CanAdd<A>` when pattern
matching on an expression. In Haskell, this happens automatically:
matching on `Add` brings the `Num a` constraint into scope.
Rust has no mechanism for this, so we’ll need an indirection.

We’ll introduce a helper trait `MaybeAdd` that acts like a type class dictionary.
It provides an operation `maybe_add`, which only exists when the type supports
addition. We’ll use specialisation to make that conditional.

```rust
#![feature(specialization)]

trait MaybeAdd {
    fn maybe_add(self, rhs: Self) -> Self;
}
```

We define a *default* implementation for all types:

```rust
impl<T> MaybeAdd for T {
    default fn maybe_add(self, _rhs: Self) -> Self
    {
        unreachable!("no Add implementation for this type")
    }
}
```

and a *specialised* implementation for types that implement `Add`:

```rust
impl<T: std::ops::Add<Output = T>> MaybeAdd for T {
    fn maybe_add(self, rhs: Self) -> Self
    {
        self + rhs
    }
}
```

With this machinery, we can now use the constraint witness inside `eval`:

```rust
fn eval<A>(expr: Expr<A>) -> A {
    match expr {
        Expr::LitInt(p, n) => p.convert(n),
        Expr::LitDouble(p, d) => p.convert(d),
        Expr::LitBool(p, b) => p.convert(b),
        Expr::Add(w, a, b) => eval(*a).maybe_add(eval(*b)),
        Expr::Or(p, a, b) => p.convert(eval(*a) || eval(*b)),
    }
}
```

Rather than directly using `+` (which we can’t, since `A` isn’t known to
implement `std::ops::Add` in this context), we delegate to `maybe_add`, which
uses specialisation to select the correct implementation at monomorphisation
time.

```rust
#[test]
fn eval_test() {
    let expr_int = {
        let a = Expr::LitInt(Is::refl(), 3);
        let b = Expr::LitInt(Is::refl(), 4);
        Expr::Add(can_add(), Box::new(a), Box::new(b))
    };
    assert_eq!(eval(expr_int), 7);

    let expr_double = {
        let a = Expr::LitDouble(Is::refl(), 2.5);
        let b = Expr::LitDouble(Is::refl(), 4.0);
        Expr::Add(can_add(), Box::new(a), Box::new(b))
    };

    assert_eq!(eval(expr_double), 6.5);
}
```

### Why this works

If you’re coming from Haskell, it might be surprising that `eval` works at all.
In Haskell, type class resolution is coupled with evidence generation: when the
compiler decides that a type satisfies a constraint, it also produces a
reference to the corresponding dictionary. If Rust worked the same, then that
algorithm would pick the catch-all default implementation of `MaybeAdd` under
the `Expr::Add` arm of `eval`, because at that point, no information is known
about the type (and our `CanAdd` witness is invisible to the typechecker).

However, Rust’s specialisation works differently. During type checking, the
compiler only checks that *some* implementation of `MaybeAdd` exists -- it
doesn’t commit to which one.
This step is **proof-irrelevant**: the fact that a trait is implemented matters,
but not which implementation it resolves to.

The actual selection happens later, during **monomorphisation**, once all type
parameters are concrete. At that point, the specialiser sees that `A = i64` (or
`A = f64`, etc.) and picks the more specific implementation that performs real
addition. The default `unreachable!()` version is never instantiated, precisely
because our witness mechanism disallows constructing expressions that try to add
values without `Add` implementations.

This is the crucial distinction between Haskell and Rust: in Haskell, dictionary
resolution is part of type checking; in Rust, it’s deferred until code
generation. The specialiser makes the final decision once it knows the concrete
types, and because our witness types restrict what can actually be constructed,
the correct implementation is always chosen.

In effect, Rust’s specialisation system lets us recover local constraint
learning at compile time, without runtime dictionaries or dynamic dispatch.
Everything is resolved statically and erased before code generation.
A truly zero-cost abstraction![^zero-cost]

[^zero-cost]: Luckily the cost analysis of abstractions doesn't include developer ergonomics.

## Limitations

This technique has a few obvious caveats.

First, it relies on **specialisation**, which is still unstable and only
available on nightly Rust. The feature also has some unsound edge cases that
the compiler can’t currently detect, though this particular usage is benign
because it doesn’t overlap implementations in unsafe ways.

Second, the design doesn’t generalise to **existential types** — Rust simply
has no equivalent. We can simulate type refinement (as with `Expr<A>`), but not
“forgetting” type information safely.

Finally, while the runtime cost is zero, the cognitive cost certainly isn’t.
The type signatures are verbose, the ergonomics are questionable, and the
amount of ceremony required to recover what Haskell gives you by default is
non-trivial.

## Conclusion

Until now we've been preoccupied with whether or not we could.
Now it's time to stop and think if we should.
