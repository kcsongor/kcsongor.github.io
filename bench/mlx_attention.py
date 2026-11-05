"""Times MLX's attention on the GPU, the ceiling for the post's `flash`.

    bench/.venv/bin/python bench/mlx_attention.py

Prints one line in the harness's format: `result mlx_attention 64 median_s
min_s gflops reps check`. The shapes and inputs are `attention_harness.c`'s:
64 queries, 32 keys, width 16, f32, and no scale factor (`scale=1.0`), as in
the post. The result is checked against NumPy's in f64 to the harness's
bound.
"""
import sys
import time

import mlx.core as mx
import numpy as np


def inputs(rows, columns, salt):
    i, j = np.meshgrid(np.arange(rows, dtype=np.uint64), np.arange(columns, dtype=np.uint64), indexing="ij")
    x = ((i * np.uint64(2654435761)) ^ (j * np.uint64(40503)) ^ np.uint64(salt * 2246822519)) & np.uint64(0xFFFFFFFF)
    x ^= x >> np.uint64(13)
    x = (x * np.uint64(0x5BD1E995)) & np.uint64(0xFFFFFFFF)
    x ^= x >> np.uint64(15)
    return ((x % np.uint64(129)).astype(np.int64) - 64).astype(np.float32) / np.float32(64)


def main():
    queries, keys, width = 64, 32, 16
    q_host, kt_host, v_host = inputs(queries, width, 1), inputs(width, keys, 2), inputs(keys, width, 3)
    # MLX takes (batch, heads, sequence, width), and keys untransposed.
    q, k, v = (mx.array(x)[None, None] for x in (q_host, kt_host.T.copy(), v_host))
    mx.eval(q, k, v)

    def attend():
        return mx.fast.scaled_dot_product_attention(q, k, v, scale=1.0)

    y = attend()
    mx.eval(y)
    s = q_host.astype(np.float64) @ kt_host.astype(np.float64)
    w = np.exp(s - s.max(axis=1, keepdims=True))
    want = (w @ v_host.astype(np.float64)) / w.sum(axis=1, keepdims=True)
    got = np.array(y)[0, 0].astype(np.float64)
    good = bool(np.all(np.abs(got - want) <= 1e-4 * np.maximum(1.0, np.abs(want))))

    times = []
    total = 0.0
    mx.eval(attend())
    while len(times) < 5000 and (len(times) < 5 or total < 1.0):
        started = time.perf_counter()
        mx.eval(attend())
        times.append(time.perf_counter() - started)
        total += times[-1]
    times.sort()
    median = times[len(times) // 2] if len(times) % 2 else 0.5 * (times[len(times) // 2 - 1] + times[len(times) // 2])
    flops = 2 * 2 * queries * keys * width
    print(f"result mlx_attention {queries} {median:.9f} {times[0]:.9f} {flops / median * 1e-9:.2f} {len(times)} {'close' if good else 'WRONG'}")
    sys.exit(0 if good else 2)


if __name__ == "__main__":
    main()
