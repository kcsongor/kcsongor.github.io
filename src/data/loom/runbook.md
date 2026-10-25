# Running Loom

Run commands from the directory containing `BRIEF.md`.

## Commands

```sh
loom check exercise/standard.loom exercise/gold.loom exercise/silver.loom exercise/exchange.loom
loom run exercise/smoke.chain
loom run examples/sample.chain
```

`check` parses and checks modules in the order given. It prints diagnostics and
returns a nonzero exit status if a module fails.

`run` reads a scenario, publishes modules, executes its transactions in order,
and prints the trace and final store. Module or scenario loading failures return
a nonzero exit status. Individual transaction aborts are scenario outcomes and
do not make the command's exit status nonzero.

## Scenario format

```loom
publish "../examples/metric.loom";
publish "../examples/sample.loom";
publish "../examples/reader.loom";

tx signed_by [] { call Sample::put(@alice, 17) }
tx signed_by [@alice] { call Reader::read<Sample::Reading>(_) }
```

Each scenario starts with an empty chain. Publish paths are relative to the
scenario file. Statements execute in file order. `//` starts a comment.

A transaction contains one public function call. Arguments may be u64, boolean
or address literals, `signer(@address)`, or `_`. Expressions and nested function
calls belong in a module. Type arguments go after the function name.
`signer(@address)` requires that address in `signed_by`. `_` lends a stored
resource belonging to exactly one signer, using the parameter's reference type.
An empty signer list is written `signed_by []`.

Optional resolution hints follow the signer list:
`using [I for T => @module]`. Separate multiple hints with commas.

## Output

The trace prints publication, calls and returns, storage operations, and each
transaction's result. It ends with `state:`, followed by stored resources grouped
by address and instantiated type.

| Trace item | Meaning |
|---|---|
| `→ Module::function(...)` | function entry |
| `→ dispatch ...` | interface dispatch |
| `← value` | returned value |
| `write` | store update |
| `take` | resource removed from storage |
| `committed` | transaction accepted |
| `aborted with code N` | abort with a numeric code |
| `aborted: message` | runtime failure |
| `rolled back` | preceding transaction's changes were restored |

Indentation is call depth. A bounded dispatch line identifies the interface
method. Abort stack entries are printed innermost first. Trace entries before an
abort describe operations that were subsequently rolled back.
Values print as literals, named records, tuples, handles or references. Deferred
values print as `panic(N)`. Record fields are sorted by name.
