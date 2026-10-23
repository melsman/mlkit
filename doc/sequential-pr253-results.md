# Standard sequential programs: PR #253 assessment

Superseded by the [fifteen-program mlkit-bench assessment](mlkit-bench-pr253-results.md), using prepared Basis libraries and longer workloads. This earlier screen is retained for provenance.

Ten unchanged repository benchmarks were run on native X64 (ThinkPad) and
ARM64, exactly three times per before/after binary: 120 executions total.
Every stdout result matched its repository `.out.ok` file; ray tracing also
checks its generated image against the repository image itself. There were no
warm-up runs, excluded samples or additional timing runs.

Both compiler variants use `-gc -no_par --no_basislib`, ordinary GC and sequential
MLKit. This is an assessment of the complete PR, not just the reset helper.
X64 uses `bin/mlkit-before` from the clean plan-only #253 baseline and the final
candidate. ARM64 uses the preserved pre-#253 #251 compiler and the final candidate;
#253's earlier register-info change affects X64 only. All programs on a given
host link the same current `runtimeSystemGC.a`. The intervening #250 change is
in the C runtime, which is common to both variants. Fresh separate cache suffixes
force each compiler to compile the Basis and program sources, without prepared
Basis library reuse. All compilation finishes before timing starts.

Each program runs in before/after, after/before, before/after order. X64 is
pinned to CPU 0 under a temporary idle inhibitor; ARM64 is not CPU-pinned.
The manual C timer reads each child's `wait4` user/system CPU usage and monotonic
wall time, avoiding `/usr/bin/time`'s 0.01-second rounding. Times include ordinary
process startup. Higher reporting precision does not remove noise in short
programs. RSS, wall and system time are retained in the raw CSVs.

## Results

User CPU seconds: median [minimum, maximum]. Negative changes mean faster.
Only three samples were requested, so these are observations, not statistical
confidence estimates. Quicksort and Nucleic take just a few milliseconds; their
percentage changes are not reliable performance evidence.

### Native X64

| Program | Before | PR #253 | Median change |
| --- | --- | --- | --- |
| Fibonacci | 0.070751 [0.069624,0.070909] | 0.070729 [0.070588,0.071748] | -0.0% |
| Life | 0.126786 [0.126764,0.126930] | 0.123955 [0.123141,0.124133] | -2.2% |
| Quicksort | 0.003909 [0.003840,0.004585] | 0.002238 [0.001983,0.003275] | -42.7% (very short) |
| Mergesort | 0.026853 [0.026742,0.027130] | 0.026629 [0.025704,0.026757] | -0.8% |
| Knuth–Bendix | 0.108271 [0.103630,0.108515] | 0.087353 [0.085178,0.089108] | -19.3% |
| Simple | 0.127152 [0.126435,0.127864] | 0.126987 [0.126893,0.127030] | -0.1% |
| Mandelbrot | 0.084181 [0.083894,0.084757] | 0.083869 [0.083741,0.084739] | -0.4% |
| FFT | 0.049087 [0.048895,0.052204] | 0.045342 [0.043457,0.047828] | -7.6% |
| Nucleic | 0.005656 [0.001974,0.006090] | 0.005375 [0.004657,0.005589] | -5.0% (very short) |
| Ray tracing | 0.083080 [0.081683,0.083124] | 0.073795 [0.070556,0.075638] | -11.2% |

### Native ARM64

| Program | Before | PR #253 | Median change |
| --- | --- | --- | --- |
| Fibonacci | 0.101542 [0.101112,0.130872] | 0.101175 [0.101046,0.130895] | -0.4% |
| Life | 0.112728 [0.112032,0.130839] | 0.112365 [0.111960,0.134143] | -0.3% |
| Quicksort | 0.003645 [0.003553,0.008281] | 0.004408 [0.003330,0.007403] | +20.9% (very short) |
| Mergesort | 0.023199 [0.022874,0.042938] | 0.026620 [0.022672,0.043660] | +14.7% |
| Knuth–Bendix | 0.099816 [0.099668,0.120164] | 0.099656 [0.099108,0.119952] | -0.2% |
| Simple | 0.085673 [0.085494,0.108460] | 0.086499 [0.085629,0.115781] | +1.0% |
| Mandelbrot | 0.052467 [0.052118,0.077619] | 0.052500 [0.052412,0.079458] | +0.1% |
| FFT | 0.058837 [0.057190,0.081987] | 0.057556 [0.056721,0.082351] | -2.2% |
| Nucleic | 0.005578 [0.005350,0.005794] | 0.006561 [0.005253,0.007133] | +17.6% (very short) |
| Ray tracing | 0.087658 [0.087474,0.112239] | 0.087461 [0.087327,0.110983] | -0.2% |

## Interpretation

X64 shows the clearest real-program benefits in Knuth–Bendix (19.3% lower median
user CPU time) and ray tracing (11.2%). FFT improves 7.6% and Life 2.2% in this
sample; each of their before/after ranges is separated. Fibonacci, Simple,
Mandelbrot and mergesort are broadly unchanged. The very short quicksort and
Nucleic results should not be used to claim large benefits. This comparison
combines the C-preserved-register and reset changes and does not apportion gains
between them.

ARM64's longer programs are broadly unchanged, consistent with retaining its
existing sequential reset-site fast path and leaving its C-preserved-register
support unchanged. Its apparent mergesort regression (14.7% in the median) has
overlapping ranges, including first-run costs in both variants; one of the
three pairs favours the candidate. Quicksort (+20.9%) and Nucleic (+17.6%) also
have overlapping ranges and millisecond-scale run times. These results do not
establish regressions in those programs. They do show why the large synthetic
parallel/profiling reset gains should not be extrapolated to ordinary sequential
ARM64 programs. No further runs were made to resolve uncertainty, respecting the
three-run limit.

## Reproduction and data

Run `src/Compiler/Backend/tests/benchmark-sequential.sh` manually with `SML_LIB`,
`MLKIT_BEFORE` and `MLKIT_AFTER`. It builds all ten programs before measurement,
checks every output and retains its output directory. It is not called by CI.
The selected source files and input data are unmodified tracked files in `test/`.

Raw samples, executable sizes, output/executable hashes and host/compiler/runtime
identities are under [native-performance/sequential](native-performance/sequential/).
Build/output directories remain named in the logs and hash manifests. All 120
measurements are retained. No performance tests or timing collection enter CI.
