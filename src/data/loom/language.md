# The Loom language

Loom 0.1.0.0. Commands and scenario syntax are in [RUNBOOK.md](RUNBOOK.md).

## Modules and storage

A module has a name and an address:

```loom
module Sample at @sample {
  resource Reading { value: u64 }
}
```

Names and addresses are unique across published modules. Publication is ordered:
a module is checked against modules already published. Published modules cannot
be replaced. Publication records no publisher identity.

Storage maps `(address, instantiated type)` to a value. One slot can hold one
resource of that type. Generic type arguments are part of the key.
Only the declaring module may construct or unpack its types, access their
fields, or use their storage operations. Other modules may receive and pass
values of those types. Storage operations themselves take no signer argument.

## Types and abilities

| Type | Copy | Drop |
|---|---|---|
| `u64`, `bool`, `address`, `Signer`, `unit` | yes | yes |
| `struct` with concrete Copy+Drop fields | yes | yes |
| `resource` | no | no |
| interface handle | yes | yes |
| `&T` | yes | yes |
| `&mut T` | no | yes |
| tuple | if every component has Copy | if every component has Drop |

A resource may contain fields of any value type except references. A struct's
concrete fields must have Copy and Drop. A generic struct containing an abstract
type is linear. A type parameter without a known Copy ability is treated as
linear too.

Using a non-Copy binding moves it. Every non-Drop binding must be consumed before
its scope ends, and both branches of an `if` must consume the same bindings.
Function calls consume arguments according to their abilities. Unpacking a
resource consumes it and binds its fields. Overwriting a field is permitted only
when the field's type has Copy and Drop.

References may target declared types or type parameters, not primitive types.
`&x` and `&mut x` require a variable. References may be parameters or local
bindings; they cannot be returned, stored in fields or global storage, placed in
tuples, or produced by a block or conditional. A reference cannot outlive its
frame. Moving or shadowing a local with a live reference is rejected.

`r.f` reads a Copy field or projects a reference to a non-Copy field. Assignment
`r.f = expression;` requires a mutable reference. Field access still requires the
declaring module's privilege. While a storage borrow is live, no function call
or `move_from` is allowed. A block can limit a borrow's scope.

A transaction may supply `_` to a reference parameter. The runtime selects the
slot of that type belonging to a transaction signer. There must be exactly one
such slot among the signers. It remains locked against `move_from` until the
transaction ends.

## Functions and expressions

Functions are private unless declared `public`:

```loom
public fn read(value: &Reading): u64 { value.value }
```

A function body is a block of statements with an optional trailing expression.
Statements end in `;`. A trailing expression supplies the result; otherwise the
block's result is `unit`. Omit the result annotation for `unit`.

```loom
let x = expression;
let Reading { value } = reading;
let Reading { value: n } = reading;
let (a, b) = pair;
```

Patterns occur in `let`. `_` may discard a field only when its type allows Drop.
An `if` is an expression and requires an `else`. No loops, `return`, rebinding,
or user constants are available. Braced atoms are not allowed directly in an
`if` condition. Parentheses can group expressions.

Operators, from tightest to loosest, are `!`, then `* / %`, `+ -`, `< <= > >=`,
`== !=`, `&&`, `||`. Operators at the same precedence associate left.
Both operands of `&&` and `||` evaluate. Equality is defined for `u64`, `bool`,
and `address`. `u64` ranges from 0 through 18446744073709551615. Overflow,
underflow and division by zero abort; division truncates.

## Calls and interfaces

```loom
f(x)                        // same module
call Sample::read(x)        // named module
call handle.read(x)         // interface handle
call S::read(x)             // bounded type parameter
```

Cross-module calls and transaction entrypoints must name public functions.
Calls are synchronous. A callee may call back into another module; call depth
is limited to 64. The same-module form omits `call`.

A plain interface declares methods and a module can implement it:

```loom
interface Label { fn number(): u64; }
```

`bind<Label>(address)` produces a handle to a module that declared this
implementation and satisfied its method signatures at publication.

An interface with `Self` describes operations on an implementing type:

```loom
interface Measure { fn read(value: &Self): u64; }
```

A module declares `implements Metric::Measure for Reading`. Each method must be
public and match the signature after substituting `Reading` for `Self`.
A consumer can declare `<S: Metric::Measure>` and call `S::read`. Type arguments
are written at calls, for example `call Reader::read<Sample::Reading>(_)`.
A generic resource is declared with parameters such as `resource Box<T> { x: T }`.
Generic function and resource type arguments are explicit.

When a type's module does not provide an interface implementation, a transaction
can supply a resolution hint with `using`.

## Builtins

| Expression | Result or operation |
|---|---|
| `move_to(address, resource)` | insert into an empty slot |
| `move_from<T>(address)` | remove and return the resource |
| `borrow<T>(address)` | shared reference to a stored resource |
| `borrow_mut<T>(address)` | mutable reference to a stored resource |
| `exists<T>(address)` | whether the slot is occupied |
| `addr_of(signer)` | the signer's address |
| `signer_of_self()` | a signer for the currently executing module's address |
| `assert(condition, code)` | unit when true; abort with the u64 code when false |
| `panic(code)` | a bottom-typed value; inspecting it aborts with the u64 code |
| `force(value)` | recursively evaluate fields and tuple elements; return the value; no references |
| `bind<I>(address)` | a plain-interface handle |

There is no signer literal in program code. A transaction supplies a signer
only for an address in its `signed_by` list; `signer_of_self()` is the other
source of a signer. The runtime has no ambient caller identity.

`panic(code)` fits any expected type.

## Transactions

Execution is serial. An abort restores the complete store and transaction loans
to their pre-transaction state. At transaction completion, the runtime examines
the returned value and rejects any remaining non-Drop resource. The command's
trace records calls, writes, return values and transaction outcomes.
