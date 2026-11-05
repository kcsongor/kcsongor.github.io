/*
 * Times one square matrix multiplication, c = a * b, at N x N, and checks its
 * result. `run.py` builds one image per kernel from this file:
 *
 *   -DKERNEL_CACHET=<symbol>  a Cachet schedule's exported wrapper, linked
 *                             with its emitted module and runtime
 *   -DKERNEL_OPENBLAS         cblas_{s,d}gemm from OpenBLAS
 *   -DKERNEL_ACCELERATE       cblas_{s,d}gemm from Accelerate
 *
 * and -DSIZE=<n>, -DELEM=float or double, -DNAME="...", -DMETAL for a kernel
 * that dispatches to the GPU, so each call waits for it to finish, and
 * -DCHECK_EXACT for a kernel that must match bit for bit.
 * -DSCRATCH_BYTES=<bytes> passes the kernel a fourth argument, scratch it may
 * overwrite, allocated once. Inputs are deterministic and dyadic, so no
 * product or sum is subnormal. An exact kernel must match, bit for bit, the contraction's own
 * order: each output a chain of fmas over k, ascending, from -0.0. A close
 * one (one that regroups the sum, or a library that doesn't promise an order)
 * must be within a bound on the rounding error of any order.
 *
 * It prints one line: `result NAME N median_s min_s gflops reps check`.
 */
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef SIZE
#error "SIZE must be the matrix size"
#endif
enum { N = SIZE };
#ifndef ELEM
#define ELEM float
#endif
#ifndef NAME
#define NAME "unnamed"
#endif

typedef ELEM elem;
#define IS_DOUBLE (sizeof(elem) == sizeof(double))

#if defined(KERNEL_CACHET)
unsigned char *cachet_allocate(uint64_t bytes, uint64_t alignment);
void cachet_require_strict_floating(void);
#if defined(METAL)
void cachet_metal_wait(void);
#define WAIT() cachet_metal_wait()
#else
#define WAIT() ((void) 0)
#endif
static elem *allocate(size_t bytes) { return (elem *) cachet_allocate(bytes, 16384); }
#if defined(SCRATCH_BYTES)
void KERNEL_CACHET(unsigned char *a, unsigned char *b, unsigned char *c, unsigned char *scratch);
static unsigned char *scratch;
#define CALL(a, b, c) KERNEL_CACHET(a, b, c, scratch)
#else
void KERNEL_CACHET(unsigned char *a, unsigned char *b, unsigned char *c);
#define CALL(a, b, c) KERNEL_CACHET(a, b, c)
#endif
static void kernel(elem *a, elem *b, elem *c)
{
    CALL((unsigned char *) a, (unsigned char *) b, (unsigned char *) c);
    WAIT();
}
#elif defined(KERNEL_OPENBLAS) || defined(KERNEL_ACCELERATE)
#if defined(KERNEL_ACCELERATE)
#define ACCELERATE_NEW_LAPACK
#include <Accelerate/Accelerate.h>
#else
#include <cblas.h>
#endif
static elem *allocate(size_t bytes)
{
    void *p = NULL;
    if (posix_memalign(&p, 16384, bytes) != 0) return NULL;
    return p;
}
static void kernel(elem *a, elem *b, elem *c)
{
    if (IS_DOUBLE)
        cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, N, N, N, 1.0,
                    (const double *) a, N, (const double *) b, N, 0.0, (double *) c, N);
    else
        cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, N, N, N, 1.0f,
                    (const float *) a, N, (const float *) b, N, 0.0f, (float *) c, N);
}
#else
#error "define KERNEL_CACHET, KERNEL_OPENBLAS or KERNEL_ACCELERATE"
#endif

static double seconds(void)
{
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC_RAW, &t);
    return (double) t.tv_sec + 1e-9 * (double) t.tv_nsec;
}

/* Multiples of 1/64 in [-1, 1]: products are multiples of 1/4096, and sums of
   up to a few thousand of them are exact enough to stay away from subnormals. */
static elem input(size_t i, size_t j, unsigned salt)
{
    uint32_t x = (uint32_t) (i * 2654435761u) ^ (uint32_t) (j * 40503u) ^ (salt * 2246822519u);
    x ^= x >> 13; x *= 0x5bd1e995u; x ^= x >> 15;
    return (elem) ((int) (x % 129u) - 64) / (elem) 64;
}

static int compare(const elem *a, const elem *b, const elem *c, const char **verdict)
{
    size_t mismatched = 0;
    for (size_t i = 0; i < N; i++) {
        for (size_t j = 0; j < N; j++) {
            elem acc = (elem) -0.0, magnitude = 0;
            for (size_t k = 0; k < N; k++) {
                acc = IS_DOUBLE ? (elem) fma((double) a[i * N + k], (double) b[k * N + j], (double) acc)
                                : (elem) fmaf((float) a[i * N + k], (float) b[k * N + j], (float) acc);
                magnitude += (elem) fabs((double) (a[i * N + k] * b[k * N + j]));
            }
            elem got = c[i * N + j];
#if defined(CHECK_EXACT)
            if (memcmp(&got, &acc, sizeof(elem)) != 0) mismatched++;
#else
            double epsilon = IS_DOUBLE ? 0x1p-52 : 0x1p-23;
            double bound = 2.0 * epsilon * (double) N * (double) magnitude + 1e-30;
            if (!(fabs((double) got - (double) acc) <= bound)) mismatched++;
#endif
        }
    }
    *verdict = mismatched == 0 ?
#if defined(CHECK_EXACT)
        "bits"
#else
        "close"
#endif
        : "WRONG";
    return mismatched == 0;
}

static int by_value(const void *x, const void *y)
{
    double a = *(const double *) x, b = *(const double *) y;
    return (a > b) - (a < b);
}

int main(void)
{
#if defined(KERNEL_CACHET)
    cachet_require_strict_floating();
#if defined(SCRATCH_BYTES)
    scratch = cachet_allocate(SCRATCH_BYTES, 16384);
    if (!scratch) { fprintf(stderr, "allocation failed\n"); return 1; }
#endif
#endif
    size_t bytes = (size_t) N * N * sizeof(elem);
    elem *a = allocate(bytes), *b = allocate(bytes), *c = allocate(bytes);
    if (!a || !b || !c) { fprintf(stderr, "allocation failed\n"); return 1; }
    for (size_t i = 0; i < N; i++)
        for (size_t j = 0; j < N; j++) {
            a[i * N + j] = input(i, j, 1);
            b[i * N + j] = input(i, j, 2);
            c[i * N + j] = (elem) 7;
        }

    /* Check first, on the first call, which is also a warm-up. */
    double first = seconds();
    kernel(a, b, c);
    first = seconds() - first;
    const char *verdict;
    int good = compare(a, b, c, &verdict);

    /* Then time: at least five calls and a second in all, at most 50. A
       kernel whose first call took over ten seconds, such as the loop nest a
       schedule starts from at the larger sizes, is warm after that call and
       gets three. */
    enum { MOST = 50 };
    double times[MOST];
    int reps = 0, least = first > 10.0 ? 3 : 5;
    double total = 0;
    if (least == 5) kernel(a, b, c);
    while (reps < MOST && (reps < least || total < 1.0)) {
        double started = seconds();
        kernel(a, b, c);
        times[reps] = seconds() - started;
        total += times[reps++];
    }
    qsort(times, (size_t) reps, sizeof(double), by_value);
    double median = reps % 2 ? times[reps / 2] : 0.5 * (times[reps / 2 - 1] + times[reps / 2]);
    double flops = 2.0 * (double) N * (double) N * (double) N;
    printf("result %s %d %.9f %.9f %.2f %d %s\n", NAME, N, median, times[0], flops / median * 1e-9, reps, verdict);
    return good ? 0 : 2;
}
