# Standalone no-spawn region policy (milestones 9–12)

Both native backends record `may_spawn` when generating a `spawnone` call.
The boolean is reset per unit and serialized in native link metadata. Final
standalone linking ORs metadata for units retained after link-time elimination.
An unused spawning function in a retained unit conservatively counts as spawning.
Startup writes `mlkit_regions_may_spawn` before constructing global regions.
The runtime default is true; both REPL startup generators explicitly retain true.
Incremental REPL linking never changes this policy.

`allocateRegion0` obtains a mutex only when protection was requested and the
policy is true. Specialized and profiling constructors share this entry point.
Otherwise the existing mutex field is NULL, selecting ordinary allocation in
inline code, shared helpers and C fallback. Descriptor layout, compiler defaults,
protection inference and ReML's separation controls are unchanged.

Native link metadata has a new format. Old cache compatibility is outside scope,
as requested: prepared libraries were rebuilt. The same eight standard variants
remain sufficient, including default ReML; no ReML `-no_par0` libraries are prepared.

## Correctness

`make check-spawn-policy` runs correctness checks only. Its standalone cases
cover both compilers, inferred/forced protection, inline allocation disabled,
profiling and ReML separation. Repeated cold/warm builds verify both boolean
values survive serialization. C probes inspect global and local mutex fields,
exact fills, expansion, large objects, reset and deallocation. A prepared Basis
client verifies that ordinary cached libraries permit the no-spawn policy.
The REPL test checks protection before and after an actual later spawn.

Executed on macOS ARM64:

- The new policy checks pass, including default ReML with prepared Basis caches.
- Full native and pthread/Argobots suites pass (one and four execution streams).
- Developer regressions pass: 260 checks, plus all 13 parallel cases.
- Read-only installed API checks and explicit no-spawn clients with installed
  Basis caches pass for both compilers; installed-file checksums are unchanged.
- Both X64 front ends build. Under Rosetta, allocation correctness checks,
  standalone policy cases and all 13 parallel regression cases pass.

The initial translated X64 REPL failure was isolated to an unsafe test fixture,
not an established backend fault. Its raw `spawnone` wrapper returned a
`foreignptr`, hiding the closure's region dependencies and letting the parent
return while the child still used its stack closure. The reduced reproducer
fails without the C policy probe. Crash reports show the child jumping to address
`0x1`, and emitted code places the closure in the parent's stack frame.

The corrected fixture follows `basis/Thread.sml`: it joins before returning and
keeps the function region live through the join with a typed alternative branch.
Both MLKit and ReML now pass the full REPL policy probe, including actual later
spawning, on ARM64 and translated X64. The earlier ARM64 pass of the unsafe
fixture did not establish its validity. The actual Basis `Thread.spawn`/`Thread.get` implementation also passes in the
translated X64 REPL, returning the expected integer 42. No backend/runtime fix
was required.
Native X64 validation and performance of this later policy remain separate from
the completed milestone-8 native comparison.

Direct policy-check invocation:

```sh
SML_LIB="$PWD" MLKIT="$PWD/bin/mlkit" REML="$PWD/bin/reml" \
  CC='gcc -arch arm64' sh src/Compiler/Backend/tests/check-spawn-policy.sh
```

## Manual ARM64 assessment

`benchmark-no-spawn.sh` is manual only and is not run by CI. It builds a list of
ten million integers in a global region and consumes it in another unit. All
three executables use the same compiler and `-no_gc`; the protected case includes
an unexecuted retained spawning function. Nine runs alternate order and verify
output. Times include process startup and are rounded to hundredths of a second.

| Mode | User CPU min / median / max (s) | Wall min / median / max (s) | Mach-O text segment |
| --- | --- | --- | ---: |
| Sequential `-no_par` | 0.03 / 0.03 / 0.03 | 0.04 / 0.04 / 0.04 | 32,768 B |
| Parallel, no spawning | 0.03 / 0.03 / 0.03 | 0.04 / 0.04 / 0.04 | 49,152 B |
| Parallel, retained spawning | 0.37 / 0.38 / 0.40 | 0.39 / 0.40 / 0.42 | 49,152 B |

[Raw samples](arm64-performance/no-spawn-policy/samples.csv) and segment-size
outputs are stored alongside this document's data. Segment sizes include alignment
and linked runtime code; they are not instruction counts. Parallel executables
also have a larger data segment (98,304 versus 16,384 bytes).

This workload shows removal of region synchronization, with no measured timing
difference from sequential execution at this resolution. It does not establish
zero overhead generally: descriptor checks, linked parallel runtime/code size,
startup and other parallel-runtime operations remain. Unused CAS instructions
remain in shared helpers. Protected programs retain normal synchronization.
Native X64 timings for this policy remain a separate assessment; no timings are
inferred from Rosetta.
