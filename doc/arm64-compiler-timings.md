# ARM64 compiler timing comparison

Milestone 8, bullet 1 of issue #223 / PR #224.

The existing final milestone 7 GC reports show **30.10 seconds for ARM64
versus 14.87 seconds for X64 under Rosetta 2** across the 19 programs with
compiler timings: ARM64 takes **2.02 times** the measured compiler CPU time.
The largest difference is in code generation (`CG`): **11.08 versus 1.16
seconds**, or **9.55 times** as much time.

## Reports and scope

- [ARM64 GC report](arm64-performance/test_report-native-arm64-gc-2026-09-21.html)
- [X64 GC report](arm64-performance/test_report-native-x64-gc-2026-09-21.html)

These are preserved, unmodified `kittester` reports from the final milestone 7
runs on September 21, 2026, using native ARM64 MLKit and X64 MLKit under
Rosetta 2 on the same Apple Silicon machine. Both runs used `all.tst -gc`
and passed all 181 tests. Only the 19 entries marked `tc` in `test/all.tst`
appear in the “Timings of the Compiler” section. Both compiler executables
were built with MLKit using `-gc`.

The reports came from `/tmp/mlkit-regressions.UP4txn/gc/test/test_report.html`
(ARM64) and `/tmp/mlkit-regressions.yi45nM/gc/test/test_report.html` (X64).
Both identify MLKit v4.7.22 with the build stamp
`029c205-dirty - 2026-09-21T13:56:14+02:00`; these were development builds
used for milestone 7, whose changes were subsequently committed as `eb6fdf8`.
All 563 available SML/signature/MLB/test-configuration source files in each
saved test tree match the current checkout. The two reports contain the same
19 timing rows and phase columns. No benchmark reruns were needed for this
initial comparison.

The table below uses the displayed HTML values, rounded to hundredths of a
second. “Total” is the sum of the measured compiler phases, not elapsed
compilation time. As the reports explain, timings are user CPU time excluding
garbage collection. They do not include assembler/linker time or program
execution. Each report represents one run; small differences and values near
zero should not be overinterpreted. This compares the two compiler/host
combinations, not just the cost of emitting different instruction sets.

## Comparison

An ARM/X64 ratio greater than 1 means ARM64 took more measured time.

| Measurement | ARM64 (s) | X64 (s) | ARM/X64 |
| --- | ---: | ---: | ---: |
| Sum of reported totals | 30.10 | 14.87 | 2.02 |
| Code generation (`CG`) | 11.08 | 1.16 | 9.55 |
| Total minus `CG` | 19.02 | 13.71 | 1.39 |

`CG` represents about 36.8% of the ARM64 total versus 7.8% of X64.
Its 9.92-second difference accounts for approximately 65% of the
15.23-second total difference. In both backends, `Execution*.sml` places
the `CG` timer around `CodeGen.CG`, before assembly output and linking.

| Program | ARM64 total (s) | X64 total (s) | ARM/X64 | ARM64 CG (s) | X64 CG (s) |
| --- | ---: | ---: | ---: | ---: | ---: |
| kittmergesort.sml | 0.06 | 0.21 | 0.29 | 0.03 | 0.02 |
| kitqsort.sml | 0.04 | 0.18 | 0.22 | 0.02 | 0.02 |
| kitmandelbrot.sml | 0.05 | 0.16 | 0.31 | 0.02 | 0.02 |
| kitlife35u.sml | 0.41 | 0.38 | 1.08 | 0.20 | 0.04 |
| klife_eq.sml | 0.38 | 0.36 | 1.06 | 0.19 | 0.03 |
| kitkbjul9.sml | 1.55 | 0.87 | 1.78 | 0.50 | 0.07 |
| kkb_eq.sml | 1.36 | 0.81 | 1.68 | 0.47 | 0.06 |
| kkb36c.sml | 1.56 | 0.87 | 1.79 | 0.50 | 0.07 |
| kitsimple.sml | 5.94 | 2.40 | 2.48 | 1.71 | 0.12 |
| fft.sml | 0.32 | 0.29 | 1.10 | 0.13 | 0.03 |
| logic.mlb | 1.22 | 0.75 | 1.63 | 0.61 | 0.06 |
| barnes-hut.mlb | 2.34 | 1.61 | 1.45 | 0.85 | 0.16 |
| nucleic.mlb | 5.50 | 1.36 | 4.04 | 2.33 | 0.08 |
| ray.mlb | 1.09 | 1.05 | 1.04 | 0.37 | 0.10 |
| ratio-regions.sml | 1.34 | 0.62 | 2.16 | 0.41 | 0.04 |
| kitmolgard.sml | 4.46 | 1.51 | 2.95 | 1.54 | 0.10 |
| msort.mlb | 0.05 | 0.24 | 0.21 | 0.02 | 0.02 |
| FuhMishra.mlb | 1.27 | 0.76 | 1.67 | 0.54 | 0.07 |
| testdyn1.sml | 1.16 | 0.44 | 2.64 | 0.64 | 0.05 |

ARM64 has lower totals on four small inputs (`kittmergesort`, `kitqsort`,
`kitmandelbrot`, and `msort`), but higher totals on the other 15.
The largest absolute gaps are `nucleic.mlb` (+4.14 s), `kitsimple.sml`
(+3.54 s), and `kitmolgard.sml` (+2.95 s). Their ARM64 `CG` times are
2.33 s, 1.71 s, and 1.54 s respectively, compared with 0.08 s, 0.12 s,
and 0.10 s for X64.

The gap is not confined to `CG`. For example, summing the displayed phase
values gives 4.57 versus 2.06 seconds for `OptLam`, 2.74 versus 1.48 seconds
for `RegAlloc`, and 2.67 versus 1.20 seconds for `RegInf`. Independent
rounding means phase-column sums need not equal the sum of the Total column.

These results make code generation a useful starting point for milestone 8's
remaining investigation of list concatenation. They do not establish which
uses of `@`, if any, cause the extra time. No code-generation changes were
made as part of this report.
