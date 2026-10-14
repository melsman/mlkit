# T9: Linux X64 time profiling

Linux X64 now provides the time sampling features available on macOS ARM64:
ordinary-build passive sampling, optional single-thread detailed C-origin
attribution, combined region/time output, offline function/IR navigation and
no-GC pthread sampling. This reuses the existing runtimes and library-cache
families; there is no time-specific runtime archive or cache variant.

```sh
mlkit -no_par -tp -o run program.mlb
./run +RTS -tp -tp_interval 1ms -p_file program.prof -RTS
pview program.prof -o program.html

mlkit -no_par -p -o run program.mlb
./run +RTS -p -p_file combined.prof -RTS
pview combined.prof -o combined.html
```

Ordinary X64 compilation already emits time metadata, so compiler `-tp` is
optional. Compiler and runtime `-p` each mean `-rp -tp`. Region recording still
requires instrumentation and explicit runtime activation. The existing
instrumented cache gains the metadata/attribution format revision `_CODE1_TP1`;
ordinary caches retain their names and regenerate objects without matching IR
companions. Static IR identities derive from fixed-layout IR content and marks.
Ordinary links omit object-path maps, preserving bootstrap reproducibility.
The viewer automatically searches nearby `MLB` caches using recorded source
paths; `--ir-dir` is an optional fallback for moved files. Function-label/parent
metadata is emitted without `-rp` to distinguish repeated printed names.

## Native delivery and clocks

The shared recorder uses a small platform layer for Linux `ucontext_t`
`REG_RIP`/`REG_RSP`, `CLOCK_MONOTONIC` nanoseconds, and registration-time
`pthread_getattr_np` stack bounds. Single-thread recording uses
`SIGALRM`/`ITIMER_REAL`, with independent region and time deadlines. Pthread
recording uses a coordinator to target registered workers with `SIGUSR2`.
Its condition variable uses monotonic absolute deadlines. Changing the calendar
clock cannot move these deadlines. CPU totals use each owner's
`CLOCK_THREAD_CPUTIME_ID` at registration and normal exit.

Handlers only read interrupted registers, the native clock, errno and lock-free
atomics. Stack discovery, allocation, ELF inspection, file writes, mutexes and
pthread calls run outside handlers. See the Linux man-pages project's
[signal-safety list](https://man7.org/linux/man-pages/man7/signal-safety.7.html).

Time stamps retain the actual delivery time on a common monotonic origin.
Suspend is excluded. Delayed/coalesced delivery is not reconstructed as synthetic
samples. Buffer overflow and routing loss remain separately visible; missing
expirations remain unobservable. CPU sampling timers are not implemented;
`-tp_clock wall` is the supported sampling clock.

Passive records are 16 bytes; detailed records are 32 bytes. The default remains
2 MiB per sampled thread (131,072 passive or 65,536 detailed entries).
`-tp_buffer` remains an explicit record-count override. Drains occur at explicit
API/lifecycle operations and normal shutdown. Draining creates an observation
gap. Parallel descriptors retain the existing 1024-lifetime bound and one-to-one
logical-thread/stream identity; CPU time for unjoined workers still alive at
shutdown is explicitly unavailable.

## ELF function identity and stripped code

The loaded main executable's GNU build ID and minimum loaded segment address
identify the image. PCs and exact half-open function ranges serialize relative
to that address. This covers both PIE and fixed-address executables without
assuming an address prefix. Loaded and on-disk build IDs are checked before
using optional file symbols. The viewer accepts 16–64-byte hexadecimal executable
identifiers, including existing Mach-O UUIDs and GNU SHA-1 build IDs.

Compiler metadata supplies ML function bounds and shared allocation/reset/GC
bridge bounds. Native `STT_FUNC` symbols supply names and declared sizes;
zero-size, out-of-text, overlapping and ML-overlapping symbols are omitted.
The [ELF symbol-table specification](https://refspecs.linuxfoundation.org/elf/gabi4+/ch4.symtab.html)
defines these symbol kinds and size fields. No boundary is guessed through
unnamed code. Aliases share one range; equal names at distinct starts have
separate address-based runtime unit identities.

Linux GC and reset entry functions occupy dedicated executable sections. Linker
end symbols retain their hot-code bounds after stripping, independently of the
optional symbol table. Compiler/shared ranges also survive stripping. Optional
native function names disappear after stripping; shared-library/vDSO PCs and
unretained cold fragments stay unresolved. ELF images without a supported GNU
build ID report unavailable identity; no fabricated ID is substituted.

## Detailed attribution and supported scope

Single-thread `-rp` compilation emits aligned ML origin labels, preserves the
old context and GC policy around C calls, and restores them on return. It handles
raw and converted arguments, nested C-to-ML callbacks, and nonlocal exceptions.
The scratch arguments survive publication and the additional frame preserves
ABI alignment. Callback entry records ML state; exit restores its enclosing
foreign context. Exception handlers preserve the GC-deferral bit in their saved
aligned stack pointer, restore that policy on unwind and clear stale foreign
state. GC entry/exit uses the shared runtime attribution hooks.

Ordinary builds default to passive; instrumented single-thread builds default to
detailed and can choose `-tp_attribution passive`. Detailed activation on
ordinary or parallel builds rejects clearly. All modes use the existing shared
profile representation, `pview` controls and versioned thread records.

Supported configurations are Linux X64 GC/GenGC/no-GC single-thread and no-GC
pthreads. Parallel GC, Argobots time sampling, dynamic REPL/image time sampling
and macOS X64 time sampling remain outside T9. Existing region-profiler
callback-boundary restrictions remain unchanged. Unsupported activation rejects
clearly; the dynamic REPL still supports region profiling.

## Validation and measurement protocol

`test/region_profile/check-ci.sh` runs native time checks on Linux X64 as well
as macOS ARM64. Linux uses `check-elf.sh` for ELF identity/PIE/ASLR, stripping,
offline resolution and IR. Common checks cover activation, buffering, attribution,
callbacks/exceptions, masking/conflicts, pthread lifetimes, stream selection and
combined reports. The T1 prototype program has been retired; its feasibility
results remain documented. Native feasibility is exercised through the production recorder's
compute/allocation/C/sleep phase checks and targeted pthread checks.

The historical opt-in `test/time_profile/measure-linux.sh` built GC/GenGC/no-GC ordinary and
instrumented workloads, warmed each executable, and shuffled settings within each
of five repeats. Child processes are pinned to one allowed CPU. It measures off,
paused, 1ms/100us/10us, optional passive recording on instrumented code, small
buffers and large buffers. Output parity and record/count consistency are checked.
Linux sleeps use a fixed monotonic deadline to avoid remainder rounding growing
under frequent interruptions. Node wall timing includes wrapper launch;
`/usr/bin/time` child wall/CPU have centisecond precision, and RSS is in KiB.
Coalescing and drain gaps limit interpretation of recorded wall fractions.

Manual examples and historical timing studies are not rerun as part of T9.
The manual receives no new time profiling documentation. Results and validation are recorded below.


## ThinkPad measurements

The retained [210 measurements](t9-linux-measurements.json) and
[grouped summaries](t9-linux-summary.json) cover five repeats on an Intel
Core i7-1360P / Linux 7.0.0-31-generic. These observations concern one mixed
compute/allocation/C/sleep workload, not a universal cost bound or a pthread
performance study. No comparison to a pre-T9 compiler establishes disabled
metadata cost.

| Ordinary passive mode | GC median wall overhead | GenGC | No GC |
| --- | ---: | ---: | ---: |
| Paused | 0.2% | approximately 0% | 0.2% |
| 1ms | 0.4% | 0.2% | 0.3% |
| 100us | 1.4% | 1.2% | 1.9% |
| 10us | 9.4% | 8.9% | 8.0% |

The default passive buffer increased median peak RSS by about 2.3–2.5 MiB.
Recorded wall fractions were about 99.9% at 1ms, 99.8% at 100us and 99.3–99.4%
at 10us; the denominator includes shutdown draining and cannot identify timer
coalescing independently. Default passive buffers reported zero buffer/routing
loss in this workload at all three intervals.

Instrumentation with recording disabled cost about 3.5–4.4% median wall time
relative to ordinary builds. Its no-GC peak RSS was about 114 MiB higher for this
allocation-heavy workload. This is the full region/instrumented build cost,
not just a C-marker or handler cost. Passive/detailed measurements on the same
instrumented binary remain close at 1ms/100us; no handler-speed claim follows.

Every default detailed 10us run filled its 65,536-entry buffer: 683,490 visible
drops across 15 runs. The 64-entry passive 100us runs also overflowed: 147,734
drops across 15 runs. A 1,048,576-entry passive buffer at 10us had no visible loss
but increased RSS by roughly 16 MiB. All measurements preserved program output
and final recorded counts; no routing loss was reported. Coalescing remains
unobservable even in a zero-drop run.


## Native validation

The ThinkPad built all existing Linux runtime variants without warnings and
passed the complete X64 three-stage bootstrap's stripped stage-2/stage-3 byte
comparison. The compiler's ordinary metadata therefore remains reproducible
across bootstrap cache directories. All six release Basis/API configurations
built; the Linux installation manifest now includes the revised instrumented
cache names and IR companions.

Native checks passed ELF identity/PIE/ASLR, exact offline resolution and mismatch
handling, stripped ML/shared/GC/reset bounds, ordinary GC/GenGC/no-GC sampling,
raw/converted C calls (including register and stack arguments and boxed Int64
results), nested callbacks and caught exceptions, GC-policy restoration,
pause/start/flush, overflow, masked delivery, timer/signal ownership, combined
reports, targeted pthread sampling, CPU/lifetime records, per-stream filtering,
concurrent flushing and matching worker IR. Read-only installed region/time API
clients leave installed files unchanged. The Linux production handler's normal
external calls are errno access and `clock_gettime`; distro GCC also emits a
stack-canary failure branch. No normal handler path allocates, performs I/O,
locks, uses pthread APIs or resolves metadata.

ARM64 runtime builds, unified activation/removed options, lifecycle/overflow,
combined reports, pthread sampling/IR, binary-reader and build-seed checks also
passed after the shared platform changes. Cloud CI at the T9 milestone was not yet verified. The pre-review head later passed all
three CI configurations; see [the final design and validation status](t8-unified-profiling.md).
The retained timing datasets predate later IR-navigation, UI and GC-address
substitution fixes. They were not rerun for those changes and do not establish
the exact cost of the current head.

The dedicated Linux timing driver is now retired; its [last snapshot](https://github.com/melsman/mlkit/blob/536bb934/test/time_profile/measure-linux.sh)
remains in Git history. New measurements use `measure-ordinary.sh`, shared with
ARM64, with Linux resource-time/RSS parsing and CPU affinity. Retained T9 data
uses the original steady workload and must not be compared as if produced by
the new phase-separated driver.
