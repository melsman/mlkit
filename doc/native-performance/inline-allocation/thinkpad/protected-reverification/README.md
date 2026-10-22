# Independent protected-allocation re-verification

Collected 2026-10-22 on the ThinkPad (Intel i7-1360P, Linux X64), AC power,
balanced profile, with a temporary idle inhibitor. This rechecks the original
milestone-8 comparison: `ac46a18e` baseline versus `03e40cf2` candidate. The later
no-spawn policy is deliberately excluded. It does not claim that every parallel
program slows down; the workload forces protection on otherwise local allocation.

## Fresh program builds and protection audit

Both programs were rebuilt from source into a new temporary directory, using
the already built native compiler binaries whose SHA-256 hashes are in
`host-before.txt`. All application caches were fresh; the retained compile logs
show each source and link unit being generated. The flags are identical:
`-no_gc -par --alloc_protect_always --no_basislib`. Both versions link the same
runtime archive, SHA-256
`f3d132595619b860330804a97491098c910816dd4099f2abacbe21dd0fc6cd82`.
No compiler source changes are part of this re-verification.

The short source is the original 100-million-cell local-allocation fixture.
The long source changes only the number of outer iterations from 1000 to 5000
and the checked sum to 500000000. Every run verifies its output.

Separate, untimed audit executables use the same generated main object. They
count entry to the protected helper and wrap region construction to assert
`protect == 1` and a non-NULL mutex pointer. Both compilers report:

| Source | Protected helper calls | Protected region constructors |
| --- | ---: | ---: |
| Short | 100,000,000 | 1,006 |
| Long | 500,000,000 | 5,006 |

An additional untimed short-case audit counts entry to the shared C fallback:
**195,000 for each version**. This matches 195 page expansions per reclaimed
local region, repeated 1000 times. The candidate is not doing extra fallback
work. Instrumented helper copies, audit wrappers and counters are absent from
all timed executables. Audit logs and fresh-build size/binary-hash reports are
retained in `short/` and `long/`.

## Timing controls and results

Short runs use CPUs 0 and 2, distinct physical performance cores. Each has six
ABBA/BAAB blocks: twelve observations per version, with equal mean position in
each block. Long CPU-0 runs use five blocks, ten observations per version.
Because CPU 2's short samples were noisier, a separately recorded five-block
long comparison was added there, retaining all earlier samples.

User CPU seconds, median [minimum, maximum]:

| Source / core | Baseline | Candidate | Median increase |
| --- | ---: | ---: | ---: |
| Short / CPU 0 | 1.07 [1.07, 1.07] | 1.11 [1.11, 1.11] | 3.7% |
| Short / CPU 2 | 1.085 [1.07, 1.14] | 1.11 [1.11, 1.13] | 2.3% |
| Long / CPU 0 | 5.35 [5.35, 5.35] | 5.56 [5.55, 5.56] | 3.9% |
| Long / CPU 2 | 5.39 [5.35, 5.51] | 5.565 [5.55, 5.71] | 3.2% |

The 1.085/5.565 medians average the two central hundredth-second samples; they
do not imply finer timer precision. CPU 2 has overlapping ranges and one short
block slightly favors the candidate. Every CPU-0 block and every long-workload
block favors the baseline. The longer CPU-0 comparison confirms a roughly 4%
regression beyond the rounding/startup sensitivity of the short measurement.
These results verify the workload-specific regression; they do not identify
its underlying microarchitectural cause.

No samples were removed or replaced. `primary-samples.csv` retains the initial
68 observations; `second-core-samples.csv` the additional 20; `samples.csv`
combines all 88. Host settings, compiler/runtime hashes, commands, sources and
all correctness/audit logs are retained. Benchmarks remain manual, with no
performance testing or timing collection added to CI.

## Reproduction

Given the original milestone-8 directory containing baseline and candidate trees:

```sh
M8_ROOT=/tmp/mlkit-m8-20261022 \
  systemd-inhibit --what=idle --mode=block sh benchmark.sh
# Use the experiment directory printed by benchmark.sh:
M8_ROOT=/tmp/mlkit-m8-20261022 REVERIFY_BENCHMARK=/tmp/mlkit-protected-reverify.XXXXXX \
  sh audit-fallbacks.sh
REVERIFY_BENCHMARK=/tmp/mlkit-protected-reverify.XXXXXX \
  systemd-inhibit --what=idle --mode=block sh benchmark-second-core.sh
```
