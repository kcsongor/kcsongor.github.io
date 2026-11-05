/*
 * Times the post's attention, y = softmax(q kt) v, for 64 queries, 32 keys
 * and width 16, in f32, with no scale factor. `run.py` builds one image per
 * kernel from this file, linked with a Cachet wrapper -DKERNEL_CACHET=<symbol>
 * taking q, kt, v and y, and -DMETAL for one that dispatches to the GPU.
 *
 * The result is checked against the same computation in double precision.
 * `flash` regroups the reduction and the hardware's `exp` rounds as it
 * likes, so the check is a bound, not bits: each output within 1e-4 of the
 * reference, relative to its magnitude when that is above one.
 *
 * It prints one line in `harness.c`'s format: `result NAME N median_s
 * min_s gflops reps check`, with N the number of queries and the flops of
 * the two products.
 */
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifndef NAME
#define NAME "unnamed"
#endif
enum { Q = 64, KEYS = 32, D = 16 };

unsigned char *cachet_allocate(uint64_t bytes, uint64_t alignment);
void cachet_require_strict_floating(void);
void KERNEL_CACHET(unsigned char *q, unsigned char *kt, unsigned char *v, unsigned char *y);
#if defined(METAL)
void cachet_metal_wait(void);
#define WAIT() cachet_metal_wait()
#else
#define WAIT() ((void) 0)
#endif

static float *q, *kt, *v, *y;

static void kernel(void)
{
    KERNEL_CACHET((unsigned char *) q, (unsigned char *) kt, (unsigned char *) v, (unsigned char *) y);
    WAIT();
}

static double seconds(void)
{
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC_RAW, &t);
    return (double) t.tv_sec + 1e-9 * (double) t.tv_nsec;
}

/* The matmul harness's inputs, multiples of 1/64 in [-1, 1]. */
static float input(size_t i, size_t j, unsigned salt)
{
    uint32_t x = (uint32_t) (i * 2654435761u) ^ (uint32_t) (j * 40503u) ^ (salt * 2246822519u);
    x ^= x >> 13; x *= 0x5bd1e995u; x ^= x >> 15;
    return (float) ((int) (x % 129u) - 64) / 64.0f;
}

static int compare(const char **verdict)
{
    size_t mismatched = 0;
    for (size_t i = 0; i < Q; i++) {
        double s[KEYS], m = -INFINITY, l = 0;
        for (size_t j = 0; j < KEYS; j++) {
            s[j] = 0;
            for (size_t c = 0; c < D; c++) s[j] += (double) q[i * D + c] * (double) kt[c * KEYS + j];
            if (s[j] > m) m = s[j];
        }
        for (size_t j = 0; j < KEYS; j++) l += exp(s[j] - m);
        for (size_t d = 0; d < D; d++) {
            double o = 0;
            for (size_t j = 0; j < KEYS; j++) o += exp(s[j] - m) * (double) v[j * D + d];
            double want = o / l, got = (double) y[i * D + d];
            if (!(fabs(got - want) <= 1e-4 * fmax(1.0, fabs(want)))) mismatched++;
        }
    }
    *verdict = mismatched == 0 ? "close" : "WRONG";
    return mismatched == 0;
}

static int by_value(const void *x, const void *z)
{
    double a = *(const double *) x, b = *(const double *) z;
    return (a > b) - (a < b);
}

int main(void)
{
    cachet_require_strict_floating();
    q = (float *) cachet_allocate(Q * D * sizeof(float), 16384);
    kt = (float *) cachet_allocate(D * KEYS * sizeof(float), 16384);
    v = (float *) cachet_allocate(KEYS * D * sizeof(float), 16384);
    y = (float *) cachet_allocate(Q * D * sizeof(float), 16384);
    if (!q || !kt || !v || !y) { fprintf(stderr, "allocation failed\n"); return 1; }
    for (size_t i = 0; i < Q; i++)
        for (size_t c = 0; c < D; c++) q[i * D + c] = input(i, c, 1);
    for (size_t c = 0; c < D; c++)
        for (size_t j = 0; j < KEYS; j++) kt[c * KEYS + j] = input(c, j, 2);
    for (size_t j = 0; j < KEYS; j++)
        for (size_t d = 0; d < D; d++) v[j * D + d] = input(j, d, 3);

    kernel();
    const char *verdict;
    int good = compare(&verdict);

    /* At least five calls and a second in all, at most 5000: one call is a
       few microseconds. */
    enum { MOST = 5000 };
    static double times[MOST];
    int reps = 0;
    double total = 0;
    kernel();
    while (reps < MOST && (reps < 5 || total < 1.0)) {
        double started = seconds();
        kernel();
        times[reps] = seconds() - started;
        total += times[reps++];
    }
    qsort(times, (size_t) reps, sizeof(double), by_value);
    double median = reps % 2 ? times[reps / 2] : 0.5 * (times[reps / 2 - 1] + times[reps / 2]);
    double flops = 2.0 * 2.0 * Q * KEYS * D;
    printf("result %s %d %.9f %.9f %.2f %d %s\n", NAME, Q, median, times[0], flops / median * 1e-9, reps, verdict);
    return good ? 0 : 2;
}
