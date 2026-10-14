# T6a: ordinary-build wall sampling

[T9](t9-linux-x64.md) extends ordinary, detailed and pthread sampling to Linux X64.
The platform-specific details below describe the original ARM64 implementation.

[T7](t7-parallel-sampling.md) adds static macOS ARM64 no-GC pthread sampling,
per-thread compact buffers and thread/stream views. Earlier parallel rejection
statements below describe the pre-T7 implementation.

Time sampling is dormant in the existing runtime variants. No time-specific
runtime archive or library-cache directory is introduced. On macOS ARM64,
compile an ordinary static executable with `-no_par`, then run:

```
./run +RTS -tp -tp_interval 1ms -p_file run.prof
bin/pview run.prof -o run.html
```

GC, GenGC and no-GC configurations use their usual archives and caches.
All runtimes recognize time options. Static macOS ARM64 and Linux X64
single-thread GC/GenGC/no-GC and no-GC pthread builds are supported;
activation in other configurations is rejected. Dynamic REPL sampling remains unsupported. Region
snapshots still require region-profile compilation and runtime `-rp`.

## Recording and draining

One preallocated buffer keeps a 2 MiB default budget: 131,072 passive records
(16 bytes each) or 65,536 detailed records (32 bytes each). `-tp_buffer` selects
2..1,048,576 records explicitly (at most 16 MiB passive or 32 MiB detailed);
metadata and serialization storage are additional. At the nominal 1 ms rate,
the passive default covers about 131 seconds between drains; at 100 us,
about 13 seconds. Detailed capacity is half that; actual delivery can be lower.
See [T6d compact records](t6d-compact-records.md).

No compiler polls drain time samples. The buffer drains at normal shutdown,
explicit `TimeProfile.flush()`, and start/pause lifecycle operations. Delivery
is blocked while serializing a drain so the buffer can be safely reused.
Drain duration is therefore an observation gap; pending timer signals may
coalesce, and missed expirations cannot be reconstructed. Slow destinations
increase that gap. This design does not claim a measurement of drain cost
from recorder samples.

When the buffer fills, later delivered samples are counted as drops until
the next drain. Stored samples are retained, so overflow can bias a report
toward earlier execution. The viewer reports cumulative buffer/routing drops.
An abnormal termination may lose buffered samples. No buffers or timers are
created when time profiling is disabled.

## Passive attribution

The ARM64 compiler emits half-open function ranges and stable unit/function,
source and IR identities in ordinary objects. This adds data and labels,
without boundary markers or changes to the ordinary function instructions.
IR documents are generated independently of region profiling. Existing ARM64
cache directories are reused; missing/inconsistent IR companions cause an
ordinary cache refresh rather than selection of another profile cache. Function-label
and parent metadata is retained without `-rp`, including for repeated printed names.
Without an object manifest, pview automatically searches `MLB` directories beside
recorded sources and in their enclosing directories. `--ir-dir` supplies additional
locations for moved artifacts; identity and unit checks still apply.

Compiler ranges cover shared allocation/reset bridges and per-unit GC
bridges, including builds with stripped symbols. For named C functions in the
main executable, Mach-O `LC_FUNCTION_STARTS` supplies range boundaries and
exact-start symbols supply names. Runtime-owned GC/reset entry addresses
also supply names when optional symbols have been stripped. A symbol never
extends across the next function start or an ML range. Missing symbols and unsupported universal-file
symbol lookup remain unresolved. Shared libraries are not yet catalogued.

The table aggregates recognized shared code as **Allocation**, **Reset**, and
**GC**. Other named C code uses `<runtime/C>@OFFSET` unit identities, distinguishing
it from ML functions and separating equal static C names in different objects.
These are self-time observations; callees get their own rows when known. For example, libc `malloc` time is not charged to its ML
caller. Inline allocation/reset instructions within an ML function stay with
that ML function. The sampler reads the atomic `doing_gc` flag and substitutes
the `gc` address throughout a passive GC sample, including helpers such as
`evacuate` and GC calls to `alloc_new_page`. Ordinary `alloc_new_page` samples
remain separate. Detailed mode retains actual helper PCs and GC state.

Profiled builds retain their existing optional C-origin/GC state recording.
That instrumentation is not required for ordinary sampling. [T6b measures](t6b-ordinary-measurement.md)
this simpler design. [T6c](t6c-optional-attribution.md) evaluates optional detailed
attribution and provides explicit `-tp_attribution passive|detailed` selection.

## Validation

Local macOS ARM64 checks passed for ordinary GC/GenGC/no-GC output parity,
passive ML/C resolution, matching IR, exact shared category ranges, bounded
shutdown overflow, stripped metadata and explicit parallel rejection. All
existing runtime variants build. Recorder lifecycle/combined recording,
phase checks, callbacks/exceptions, timer conflicts, ASLR/image mismatch,
standalone metadata/deep-stack checks, binary/viewer regressions and IR5 passed.

Two existing CI checks were repaired: the fixture encoder now accepts
version-10 code/time tags, and the callback assertion permits a callback's own
C-return restoration window while still rejecting an enclosing C origin.
These checks describe the T6a implementation; later CI status is recorded in
[the final milestone map](t8-unified-profiling.md). Real-browser visual layout
inspection remains unverified.
