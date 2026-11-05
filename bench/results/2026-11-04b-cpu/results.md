| Kernel | Runs on | Type | N | Median ms | GFLOP/s | Result |
| --- | --- | --- | --- | --- | --- | --- |
| matmul | CPU | f32 | 1024 | 1843.33 | 1.2 | bits |
| swapped | CPU | f32 | 1024 | 65.64 | 32.7 | bits |
| tower | CPU | f32 | 1024 | 79.35 | 27.1 | bits |
| packed | CPU | f32 | 1024 | 44.21 | 48.6 | bits |
| fast | CPU | f32 | 1024 | 18.55 | 115.8 | bits |
| bands | CPU, all cores | f32 | 1024 | 2.84 | 756.6 | bits |
| swapped_s1 | CPU | f32 | 1024 | 1836.16 | 1.2 | bits |
| tower_s2 | CPU | f32 | 1024 | 1428.42 | 1.5 | bits |
| tower_s3 | CPU | f32 | 1024 | 1440.12 | 1.5 | bits |
| tower_s4 | CPU | f32 | 1024 | 1447.36 | 1.5 | bits |
| packed_s6 | CPU | f32 | 1024 | 55.58 | 38.6 | bits |
| packed_s7 | CPU | f32 | 1024 | 55.50 | 38.7 | bits |
| packed_s8 | CPU | f32 | 1024 | 58.63 | 36.6 | bits |
| packed_s9 | CPU | f32 | 1024 | 58.55 | 36.7 | bits |
| openblas_neon | CPU, all cores | f32 | 1024 | 2.49 | 863.1 | close |
| openblas_neon_1 | CPU, one core | f32 | 1024 | 19.87 | 108.1 | close |
