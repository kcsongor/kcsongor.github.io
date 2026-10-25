# The work: a venue for listed assets

We are standing up a small trading venue on Loom. It needs two pieces, and they
are the two halves of this brief:

1. **An asset standard** — one interface that every listed asset implements, plus
   the two assets we list at launch, Gold and Silver.
2. **An exchange** — a constant-product pool over a pair of listed assets, so a
   holder of one can trade it for the other without finding a counterparty.

The point of the standard is that the exchange never names an asset. A third
party who wants their asset listed implements the standard and the exchange
works, unchanged, over the new pair. Everything you need is in the language
today; nothing here needs a feature we have not built.

Read [docs/LANGUAGE.md](docs/LANGUAGE.md) and
[docs/RUNBOOK.md](docs/RUNBOOK.md) first, and skim `examples/`. Start from the
skeletons in [exercise/](exercise/) — they check clean as they stand, so
`loom check` is a signal from the first edit.

Budget a few hours. The code is not large; the decisions are the work.

---

## Part 1 — the asset standard

`exercise/standard.loom` holds the interface. `exercise/gold.loom` and
`exercise/silver.loom` implement it, each over its own resource, with its own
supply.

The standard has to make all of the following possible. **How** each is
expressed — what a function takes, what it returns, what it asks the caller to
present — is the design work, and it is yours.

- **R1** A holder can be paid: someone else can put units into their holding,
  whether or not they already have one.
- **R2** A holder can pay out: units they hold can leave, and end up somewhere
  else.
- **R3** Anyone can read a holding's size. Reading is not a privileged operation.
- **R4** Units can be split and combined without changing the total.
- **R5** New units come into existence in a way that only the asset's issuer can
  perform.
- **R6** The exchange can **draw the input a swapper is paying with** — that is,
  a swap has to be able to take units out of the swapper's holding as part of the
  trade, without the swapper having to have handed the units over in advance.
- **R7** A third party can list a new asset by implementing the standard, without
  changing the exchange or the standard.

Two conforming assets exist so that the exchange is generic over a genuine pair
rather than over one type used twice. Gold and Silver are otherwise
interchangeable in shape.

## Part 2 — the exchange

`exercise/exchange.loom`. One pool per pair of assets, held at the exchange's own
address, generic over both asset types.

- **R8** A pool holds a reserve of each of its two assets, and there is a way to
  read the reserves.
- **R9** Creating a pool is restricted to whoever deploys the exchange. Somebody
  who merely holds both assets must not be able to create one.
- **R10** A pool is created seeded: a reserve of each asset goes in at creation.
- **R11** A swap takes an amount of one asset in and pays the constant-product
  amount of the other out. With reserves `ra` and `rb` and an input `amount_in`:

  ```
  new_ra = ra + amount_in
  out    = rb - (ra * rb) / new_ra
  ```

  All of it is `u64`: division truncates, and overflow or underflow aborts the
  transaction rather than wrapping. Seeded at 1000 and 1000, an input of 100
  should pay out 91.
- **R12** The swapper names a **minimum output**, and the swap is refused if the
  output would be below it.
- **R13** The swapper names the **address the proceeds go to**. It need not be
  their own.
- **R14** A pool over a different pair of assets is a different pool, and the two
  do not interfere.

## Part 3 — the scenarios

`.chain` files, run with `loom run`. `exercise/smoke.chain` is the starting
point; grow it, and add others alongside it.

- **R15** An end-to-end run, in `exercise/smoke.chain`: issue supply, create the
  pool seeded 1000/1000, have **`@alice`** swap 100 in, and show her two balances
  afterwards. State the numbers you expect in comments, and make the run produce
  them. Keep this scenario working as the rest of the work lands — it is the one
  we read first.
- **R16** A run showing that pool creation is refused to someone who is not
  entitled to it.

---

## Deployment

Modules share one chain. Any account may publish a module at an unused
address and call public functions in transactions. Transactions execute in order.

---

## What to hand back

- `exercise/standard.loom`, `exercise/gold.loom`, `exercise/silver.loom`,
  `exercise/exchange.loom`, checking clean together:

  ```sh
  loom check exercise/standard.loom exercise/gold.loom exercise/silver.loom exercise/exchange.loom
  ```

- The scenarios from part 3, each running and producing the numbers its comments
  claim.
- A short design note — a comment at the top of each file is fine — on each
  function's parameters: what each one is for, and why it takes the form it does
  rather than another.
