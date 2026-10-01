# Selected-region allocation attribution (issue #239, M1–M3)

Compile **all ML units** with `-rp -allocation_profile`. This is an explicit
opt-in, separate from ordinary snapshot profiling. Run once with `-rp` and
identify an infinite region in `rpview`. The region tooltip includes its
`UNIT:BINDING` allocation selector. Rerun the same executable:

```
./program -rp -rp_interval 0 -rp_region 'UNIT:BINDING' -rp_file allocations.rp
rpview allocations.rp -o allocations.html
```

`-rp_alloc_depth 1` is optional; other depths are rejected. `-rp_build ID`
checks the build identifier recorded in the discovery profile before opening
an output file. A new link produces a new build identifier. Unit/binding IDs
are reproducible for the same executable, not a promise across recompilation.
Global bindings use `<global>:TYPE`, where TYPE is the compiler's run-type ID
(strings 1, pairs 2, arrays 3, refs 4, triples 5, top 6).

The final table aggregates all logical threads over the entire run. Snapshot
pause/start, snapshot time selection, and snapshot thread filters do not change
this histogram. An abrupt exit can leave an incomplete profile. Live worker
threads at process exit are explicitly reported as incomplete; join workers
before a normal exit to obtain their complete contributions.

## M1: accounting and ABI audit

An event is a **logical program allocation into an infinite region**. Bytes are
requested storage words times the machine word size, including any ordinary
object header actually stored. Tag-free pairs/refs/triples do not acquire an
extra header for attribution. Page headers/slack, profiler metadata, allocator
rounding and GC copies are excluded. An allocation at bottom, a reset, region
release, or collection never subtracts from these cumulative counters.
Zero-word requests count as one event with zero bytes. Static objects and finite
stack reservations are outside this accounting.

| Path | Accounting boundary |
| --- | --- |
| X64 inline, page expansion, protected/unprotected and large ML allocations | `alloc_kill_tmp01` diverts selected destinations to `alloc_profiled` |
| ARM64 inline, page expansion, protected/unprotected and large ML allocations | `allocateInRegionInto` diverts selected destinations to `alloc_profiled` |
| C strings, tables, arrays, I/O and other runtime-created objects | `alloc` / `alloc_unprotected` consult the destination and current ML/C origin |
| GC ordinary/pair/ref/triple copies | `allocGen` directly; deliberately no event hook |
| Page acquisition / large-object backing allocation | No extra logical event |
| Finite objects, static data, old object-header profiler | No attribution instrumentation |

`alloc_profiled` shares `allocGen` with `alloc` but bypasses the foreign-origin
hook, preventing double counting. Specialized allocation and GC paths retain
their existing tag-free pointer bias. The selection check must preserve the
infinite status bit when it falls through to a slow allocator.

Each dynamic region descriptor gains one trailing pointer to selected static
binding metadata. Region allocation initializes it to null; generated binding
registration installs it only for the selected unit/binding. Existing fields,
including the parallel mutex, retain their offsets. The common descriptor cost
is **8 bytes per infinite region**, even without attribution capability. A
trailing context pointer costs **8 bytes per logical thread**. Existing context
and ThreadInfo leading offsets remain unchanged. `Layout.c` checks the C layout;
`BackendInfo.size_of_reg_desc` supplies both native backends. All ML cache
variants receive `_A1`, with `_AP1` for attribution and `_APG1` for the global
experiment. Rebuild the runtime and all ML objects together.

Static site descriptors contain the ML compilation-unit name, generated
function symbol, source filename and a unit-local site number. They are retained
by direct relocations from generated code. Addresses are used only as in-process
lookup keys, never as portable identities; output includes definitions once and
therefore needs neither the executable nor debug symbols. Source precision is
currently **file plus generated function/site**, not source line/column or
inlined ancestry. Flat attribution requires no ML stack walk and does not use
safe-point frame maps as allocation-site unwind maps. M4 must audit allocation
anchors separately; tail-call history is not reconstructed.

ML/C entry saves an origin in the logical context and ordinary return restores
it. Nested C helpers inherit that origin. A C-to-ML callback's explicit ML hooks
use their own sites; nested ML/C calls push their own origins. Native exception
unwind removes origins belonging to discarded stack frames, including a C call
abandoned by an exception from a callback. Anchors use downward-growing native
stack addresses, matching both supported backends. This state lives in Context,
not OS TLS, so the design does not assume scheduler affinity. Argobots execution
has not been validated by this change. GC plus parallelism remains unsupported.

## M2: recording and viewer

Attribution-capable executables emit binary version 6. Ordinary snapshot builds
continue to emit version 5; `rpview` accepts both. Existing records are unchanged.
The new little-endian records use the existing length-prefixed framing:

| Tag | Record | uint64 fields | String fields |
| --- | --- | --- | --- |
| 12 | allocation_session | enabled, depth | build_id, selector |
| 13 | allocation_site | definition, site | unit, function, source |
| 14 | allocation | thread, definition, count, bytes | — |
| 15 | allocation_incomplete | thread | reason |
| 16 | allocation_region | binding | unit, name, source |

Each logical thread owns its counter hash table. Allocation updates take no
shared-counter lock. A worker serializes its final counters before retirement;
main serializes at process exit. Definition assignment and output use the
existing stream lock. `runtime/unknown` is explicit when no initiating ML site
exists. There are no per-object headers, per-allocation output records or
per-event heap allocations after a site's counter exists. Counters are bounded
at 65,536 distinct sites per thread and foreign-origin depth at 1,024; exceeding
these limits or overflowing a 64-bit counter fails explicitly and leaves the
profile incomplete. The `counter_bytes` diagnostic includes the logical-thread
state, counter nodes and allocated origin stack; it excludes output buffers,
static descriptors and serialized-definition bookkeeping.

## M3: experiment and defaults

`-allocation_profile_global` additionally diverts every infinite ML allocation
while runtime attribution is enabled, then filters by destination in C. This
provides an explicit comparison against selective diversion. It has a separate
ML cache identity and is not the default. The disabled selective path performs
one descriptor load/test/branch per eligible ML allocation. The prototype also
saves/restores C origins at foreign calls, including when collection is disabled;
this overhead is a reason to keep capability opt-in.

`test/region_profile/benchmark-allocation.sh` reports medians of seven runs,
executable size, profile size and counter storage for the same tiny-pair/foreign
consumer workload. Build `allocation.sml` with the C fixture using `-rp`,
`-rp -allocation_profile`, and `-rp -allocation_profile
-allocation_profile_global`, respectively. Set `AP_ITERATIONS` to change the
workload (default 10,000,000). This is deliberately allocation-intensive and
includes a C consumer per pair; its numbers are not general application
slowdown estimates. The baseline includes the common descriptor ABI cost, so
these timings do not isolate that cost against a pre-change compiler.

Validation and measured results are recorded below after running the checks.
M4 caller expansion, M5 deltas/filtering, and M6 shipping/archive policy remain
separate milestones. Dynamic REPL images are not part of this executable-only
validation; do not infer their support from ordinary snapshot REPL support.

### Local results (2026-10-01)

Both native backends were compiled with an installed MLKit ARM64 compiler.
ARM64 executables ran natively on macOS; X64 executables ran through Rosetta on
the same host. X64 timings therefore **are not native X64 hardware timings**.
The complete `check-allocation.sh` matrix passed for both targets: inline pairs,
C arrays and strings (including large objects), region reset, nested C-to-ML callbacks, exception escape, ordinary GC,
generational GC, and four no-GC pthread workers. Both collectors produced the
same 100,000 allocations / 1,600,000 bytes with collection enabled and disabled.
The pthread test reported 1,000 allocations / 32,000 bytes for each worker and
one shared site definition. The existing ARM64 snapshot/accounting/API checks
also passed, as did binary-format and viewer regression checks and allocation
table DOM assertions. Runtime archives, including legacy profiling variants, built.

Seven-run elapsed-time medians for 10,000,010 tiny-pair allocations plus C
consumers (seconds; `/usr/bin/time -p`, 0.01-second resolution):

| Mode | ARM64 native | X64 under Rosetta |
| --- | ---: | ---: |
| Snapshot-capable baseline, runtime disabled | 0.09 | 0.10 |
| Attribution-capable, runtime disabled | 0.15 | 0.31 |
| Selective diversion, hot region selected | 0.28 | 0.51 |
| Selective diversion, hot region unselected | 0.18 | 0.35 |
| Global diversion, hot region selected | 0.28 | 0.51 |
| Global diversion, hot region unselected | 0.24 | 0.47 |

The selected profile occupied 3,408 bytes (including two region snapshots),
with 2,264 bytes of counter/origin state for its single thread/site. The
unselected profile occupied 3,191 bytes with 2,232 bytes of origin state and no
counters. Baseline/attribution executable sizes were 123,000/123,728 bytes on
ARM64 and 95,600/100,104 bytes on X64 (100,112 for the X64 global experiment).
Paths and generated identifiers affect these file sizes. No universal overhead
percentage follows from this deliberately small FFI-heavy benchmark.

Decision: retain **selective diversion** and keep `-allocation_profile`
**opt-in**. Global diversion paid additional cost for unselected allocations
without improving the selected case. Disabled capability was not cheap enough
in this workload to justify enabling it in every `-rp` build. The investigation
below adds native Linux X64 timings and reduces that disabled overhead. Broader
application benchmarks, Argobots migration tests, line/column metadata, and
release-cache policy remain further validation and integration work.

### Disabled C-call wrapper investigation

The original attribution wrappers always saved/restored caller-clobbered ML
registers and called `mlkit_rp_foreign_enter` / `mlkit_rp_foreign_leave`, even
when the helpers immediately returned because attribution was disabled.
Both backends now test `mlkit_rp_allocation_enabled` before that work. The
enabled path retains the same origin stack and callback/exception handling.
Selection is fixed at process startup; the guard does not depend on the
snapshot profiler's start/pause state. Entry and return each test the flag,
avoiding duplication of the foreign call or a new saved flag across callbacks.

`benchmark-allocation-wrappers.sh OLD_REML NEW_REML` compares three workloads:
the existing pair-plus-C-consumer loop, the same pair loop with an ML projection
instead of the C consumer, and a scalar C-call loop without per-iteration ML
allocation. The baseline is compiled with `-rp`; before/after add
`-allocation_profile`. All three execute without runtime profiling options.
Both compiler executables must use a compatible runtime; set `SML_LIB` to that
tree. The script uses separate caches, retains generated assembly, warms each
executable, and rotates measurement order across nine runs. The default is
30,000,010 loop iterations, including the ten iterations after region reset.
These are deliberately small diagnostic loops, not application benchmarks.

ARM64 native measurements (macOS, nine-run elapsed-time medians in seconds):

| Workload | Snapshot-only baseline | Attribution before | Attribution after |
| --- | ---: | ---: | ---: |
| Pair allocation plus C consumer | 0.22 | 0.43 | 0.25 |
| Pair allocation plus ML projection | 0.20 | 0.21 | 0.21 |
| Scalar C call, no loop allocation | 0.05 | 0.33 | 0.06 |

The pair-plus-C workload is about 42% faster; roughly 86% of its measured
disabled-attribution overhead relative to the baseline disappears. The
allocation-only control is unchanged. With attribution enabled in the
pair-plus-C workload, nine-run medians were 0.80/0.81 seconds before/after for
the selected hot region and 0.51/0.52 seconds when selecting another region.
The guards therefore have a small enabled-path cost in this measurement.
`/usr/bin/time -p` has 0.01-second reporting resolution; these ratios should
not be interpreted as precise predictions for other workloads. The complete
ARM64 allocation-attribution test matrix passed with the guards, including
callbacks, exception escape, both collectors and pthreads.

Native Linux X64 measurements on the ThinkPad, using the same workload and
nine-run protocol (seconds):

| Workload | Snapshot-only baseline | Attribution before | Attribution after |
| --- | ---: | ---: | ---: |
| Pair allocation plus C consumer | 0.38 | 0.70 | 0.42 |
| Pair allocation plus ML projection | 0.34 | 0.34 | 0.36 |
| Scalar C call, no loop allocation | 0.05 | 0.40 | 0.06 |

The pair-plus-C workload is 40% faster, removing about 88% of its measured
disabled-attribution overhead. The allocation-only control shows no benefit
(and a 0.02-second increase in this run); this change targets C-call wrappers,
not allocation selection checks. Enabled-attribution medians were 1.18/1.19
seconds for the selected hot region and 0.84/0.85 seconds for another region.
The complete native X64 allocation-attribution matrix also passed, including
disabled discovery runs, callbacks, exceptions, both collectors and pthreads.

### Third status bit audit

Bit 2 (`0x4`) is unused in the **region pointer** on the supported 64-bit
backends: region storage is word aligned, and bits 0 and 1 currently encode
infinite and at-bottom status. A propagated attribution-selection bit could
replace the descriptor metadata load with `tbz` on ARM64 or a register bit
test on X64 for a region parameter used by many allocations. This approach is
not pursued here because it requires broader pointer-representation changes.

It is not a local substitution for the current descriptor check. Both backends
reconstruct local region pointers from stack offsets (`REG_I_ATY`) and add the
ordinary status bits; they do not retain the allocator's returned pointer.
These paths would need to recover selection from the descriptor, or retain a
canonical tagged handle. Parameter passing, at-bottom/reset operations, global
handles and every C/assembly dereference must preserve or clear the new bit as
appropriate. The current `clearStatusBits` and native `-4` masks clear only two
bits, so using bit 2 without that audit would offset accesses by four bytes.
Selection being fixed at startup avoids a separate alias-invalidation problem.

The similarly named fields **inside the descriptor** have different tags:
bit 2 of `g0.fp` already participates in the GC region-type encoding (triples
use `0x7`), while `g0.a` is used as an ordinary allocation pointer throughout
the allocator and collectors. Neither can acquire this bit without further
representation changes. The wrapper optimization keeps the existing pointer
ABI; no speedup from a third-bit implementation is claimed here.
