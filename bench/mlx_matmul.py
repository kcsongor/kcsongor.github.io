"""Times MLX's f32 matrix multiplication on the GPU, the GPU ceiling.

    bench/.venv/bin/python bench/mlx_matmul.py N

Prints one line in the harness's format: `result mlx N median_s min_s
gflops reps check`. The inputs are the harness's, and the result is checked
against NumPy's in f64 to the same bound the harness uses for a kernel that
doesn't promise Cachet's order.
"""
import sys
import time

import mlx.core as mx
import numpy as np


def inputs(n, salt):
    i, j = np.meshgrid(np.arange(n, dtype=np.uint64), np.arange(n, dtype=np.uint64), indexing="ij")
    x = ((i * np.uint64(2654435761)) ^ (j * np.uint64(40503)) ^ np.uint64(salt * 2246822519)) & np.uint64(0xFFFFFFFF)
    x ^= x >> np.uint64(13)
    x = (x * np.uint64(0x5BD1E995)) & np.uint64(0xFFFFFFFF)
    x ^= x >> np.uint64(15)
    return ((x % np.uint64(129)).astype(np.int64) - 64).astype(np.float32) / np.float32(64)


def main():
    n = int(sys.argv[1])
    a_host, b_host = inputs(n, 1), inputs(n, 2)
    a, b = mx.array(a_host), mx.array(b_host)
    mx.eval(a, b)

    c = a @ b
    mx.eval(c)
    exact = a_host.astype(np.float64) @ b_host.astype(np.float64)
    magnitude = np.abs(a_host.astype(np.float64)) @ np.abs(b_host.astype(np.float64))
    good = bool(np.all(np.abs(np.array(c).astype(np.float64) - exact) <= 2 * 2.0 ** -23 * n * magnitude + 1e-30))

    times = []
    total = 0.0
    mx.eval(a @ b)
    while len(times) < 50 and (len(times) < 5 or total < 1.0):
        started = time.perf_counter()
        mx.eval(a @ b)
        times.append(time.perf_counter() - started)
        total += times[-1]
    times.sort()
    median = times[len(times) // 2] if len(times) % 2 else 0.5 * (times[len(times) // 2 - 1] + times[len(times) // 2])
    print(f"result mlx {n} {median:.9f} {times[0]:.9f} {2 * n ** 3 / median * 1e-9:.2f} {len(times)} {'close' if good else 'WRONG'}")
    sys.exit(0 if good else 2)


if __name__ == "__main__":
    main()
