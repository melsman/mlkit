# ARM64 register preservation and peephole optimisation

Milestone 12 implements smaller register-save sets and a local instruction
optimiser. Instruction scheduling is deferred.

Runtime helpers now explicitly preserve only C-clobbered ML registers:
38 registers instead of 54, including the FP spill temporaries. The shared
preserving stubs use 320 stack bytes instead of 448, including LR and alignment.
ARM64's C-preserved GPR predicate also lets the existing spill pass keep values
in `x19–x26` across C calls. The collector's root image has its own protocol.

`InstsArm64.optimise` runs before emission for program and link code. It removes
redundant 64-bit integer moves and branches to the next label, simplifies
conditional/unconditional branch sequences, forwards adjacent accesses to the
same stack slot, and combines eligible adjacent stack accesses into `ldp`/`stp`.
It keeps labels and directives, restricts memory rewrites to exact SP-relative
slots, and checks register classes, distinct load destinations, and pair offsets.
Constant construction also omits unnecessary zero `movk` instructions.

## Small benchmark check

Nucleic, mlyacc, and professor use the same inputs and sources as
[milestone 11](arm64-runtime-performance.md). Old and new ARM64 executables run
on the same M2 Max, both with GC and without GC. The two versions use the same
previously compiled Basis; the new compiler recompiles the benchmark units and
link code. Thus this checks these changes in the benchmark code without adding
the effects of rebuilding the Basis.

Times are median elapsed seconds from three runs after one warm-up, alternating
old/new order, with compilation excluded. Every output is checked against the
benchmark's expected output. These are a few before/after checks, not a repeat
of the full benchmark study.

| Benchmark | Mode | Before (s) | After (s) | Time change |
|---|---|---:|---:|---:|
| nucleic | GC | 1.7066 | 1.6802 | -1.5% |
| nucleic | No GC | 0.8288 | 0.7895 | -4.7% |
| mlyacc | GC | 0.3763 | 0.3951 | +5.0% |
| mlyacc | No GC | 0.2546 | 0.2747 | +7.9% |
| professor | GC | 0.4620 | 0.4004 | -13.3% |
| professor | No GC | 0.4123 | 0.3647 | -11.5% |

Results are mixed: professor is about 11–13% faster, nucleic is roughly
unchanged with GC and about 5% faster without GC, and mlyacc is about 5–8%
slower. This limited check does not attribute the mlyacc regression to an
individual transformation. All 48 warm-up/measured executions pass their
expected-output checks.

[Individual samples and compiler commands](arm64-performance/optimisation-results/measurements.json).

## Correctness checks

MLKit `-gc` builds pass for MLKit, ReML, and the emitter harness. Nineteen
peephole checks exercise useful rewrites and unsafe lookalikes, including
32-bit self-moves, mixed register banks, label/directive barriers, writeback
addresses, duplicate load destinations, and pair offsets at the encoding limit.
The register-palette check covers every allocatable GPR.

The native MLKit/ReML integration suite passes: ordinary and tail calls,
closures, exceptions, register spills, large frames, long branches, scalar C
arguments/results, allocation/reset fast and slow paths in plain/GC/generational
modes, forced collection, callbacks, profiling, and REPL recovery. GC metadata
relocation checks pass. The full regression and 20-benchmark suites were not
rerun for this change.

The [initial investigation](arm64-optimisation.md) records further possibilities,
including live-register masks on cold allocation edges and selective branch
widening. Those remain possible follow-ups.
