| Kernel | Runs on | Type | N | Median ms | GFLOP/s | Result |
| --- | --- | --- | --- | --- | --- | --- |
| matmul | CPU | f32 | 4096 | 211498.49 | 0.7 | bits |
| rows | GPU | f32 | 4096 | 57.26 | 2400.3 | bits |
| tiled | GPU | f32 | 4096 | 31.26 | 4396.6 | bits |
| big_tiles | GPU | f32 | 4096 | 14.51 | 9469.7 | bits |
| mlx | GPU | f32 | 4096 | 43.54 | 3156.9 | close |
