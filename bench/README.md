# Benchmarks for "Well-scheduled kernels cannot go wrong*"

`run.py` times every schedule in the post's complete example,
[`public/code/cachet-contractions.cachet`](../public/code/cachet-contractions.cachet),
beside the best implementations we know of on the same machine:

| Ceiling | For | Library |
| --- | --- | --- |
| `openblas_neon`, `openblas_neon_1` | the CPU's vector units, all cores and one | OpenBLAS with `OPENBLAS_CORETYPE=NEOVERSEN1`, its NEON kernels |
| `openblas_sme` | the CPU's matrix unit | OpenBLAS as it is, which picks its `vortexm4` (SME) kernels on an M4 |
| `accelerate`, `accelerate_f64` | the CPU's matrix unit | Apple's Accelerate |
| `mlx` | the GPU | MLX's `a @ b` |

Each kernel computes `c = a * b` for N × N matrices, in `f32` except `sme` and
the `_f64` ceilings. Every result is checked before it's timed: a schedule
that claims `matmul`'s bits must match, bit for bit, a C loop that does each
output's `fma`s in ascending order of k (`bits`), and a kernel that regroups
the sum, or a library that doesn't promise an order, must be within a bound on
rounding error (`close`). A wrong result stops the run.

## Running it

```sh
uv venv bench/.venv --python python3.12
uv pip install --python bench/.venv/bin/python mlx==0.32.4 numpy
brew install openblas
bench/run.py --cachet ~/Dev/Cachet --sizes 1024
```

`--cachet` is a Cachet checkout, for its library and platform files, and
`--cachetc` the compiler (by default, `cabal list-bin exe:cachetc` there).
`--sizes` takes several sizes, and `--only matmul,tiled,mlx` a subset. Results
go to `bench/results/<timestamp>/`, or `--out`.

For each schedule, the script writes the example without its `main`, plus an
exported wrapper that calls the schedule at N × N, and builds it with `cachetc
build --target llvm --emit-llvm --release`. The emitted module, its runtime
and, for a GPU schedule, its Metal bridge are linked with
[`harness.c`](harness.c), which allocates through Cachet's allocator (the
Metal bridge only binds buffers it allocated) and waits for the GPU after each
call. The ceilings use the same harness and inputs; MLX uses
[`mlx_matmul.py`](mlx_matmul.py), which reproduces them in NumPy.

Everything is built before anything is timed. Each kernel is called once to
check it, once more to warm up, then at least five times and for at least a
second, at most 50 calls; the result is the median. A kernel whose first call
took over ten seconds, such as the loop nest a schedule starts from at the
larger sizes, is warm after it, and gets three calls instead. GPU times include the
dispatch and the wait, as MLX's include `mx.eval`. Below a millisecond or so
they are noisy, so compare GPU kernels at N = 2048 or more.

The schedule stages (`tower_s2`, `sme_s7`, ...) are a schedule's first steps
alone, as the post's bars show them. The post's attention, `attend` and
`flash` in [`attention.cachet`](attention.cachet), is timed by
[`attention_harness.c`](attention_harness.c) at 64 queries, 32 keys and
width 16, checked to a bound against a double-precision reference, and
compared with MLX's `scaled_dot_product_attention` in
[`mlx_attention.py`](mlx_attention.py).

`manifest.json` records the Cachet revision and whether its tree was clean,
the compiler's hash, the example's and harness's hashes, the library versions,
the machine and whether it was on AC power. `results.json` has every
measurement and the command that produced it, and `results.md` a table.
