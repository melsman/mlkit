# Region profiler M1: explicit snapshots

This records the initial M1 implementation. For the implemented M2–M7 controls,
runtime combinations, version 2 format, and current checks, see
[the profiler guide](region-profiler.md).

M1 implements explicit, single-threaded snapshots without garbage collection.
The runtime counts region pages by traversing links and subtracts the unused
last-page tail. It records finite-region stack reservations and separately
tracks large-object sizes. Objects and region descriptors retain their normal
layout, and the executable links the ordinary runtime archive.

Periodic sampling, thread rendezvous, GC sampling, REPL sessions, and live
viewing remain later milestones in [the design](region-profiler-design.md).
M1 does not add polling points or maintain page counters.

## Build and use

Build the compiler and runtime together; an older runtime does not contain
these entry points. Include the library in an MLB file:

```sml
$(SML_LIB)/basis/basis.mlb
$(SML_LIB)/kitlib/region-profile.mlb
main.sml
```

For example, instrument a phase in `main.sml`:

```sml
val () = RegionProfile.start ()
val () = RegionProfile.mark "work begins"
val values = List.tabulate (1000, fn n => n)
val () = RegionProfile.sample ()
val () = print (Int.toString (length values) ^ "\n")
val () = RegionProfile.pause ()
val () = RegionProfile.flush ()
```

Compile and run:

```sh
mlkit -no_gc -region_profile -o program program.mlb
./program -rp -rp_paused -rp_file experiment.rp -- application-arguments
rpview experiment.rp --output experiment.html
```

Use `reml -no_par -region_profile` for a ReML executable. X64 ReML enables
parallelism by default; `-no_par` explicitly selects the M1-supported mode.
The option is now accepted by both native backends without changing their
existing defaults. Instrumented caches have an `_RP1` suffix, so the Basis and
other ML units are rebuilt with compatible metadata.

Only these runtime options are implemented in M1:

| Option | Behavior |
| --- | --- |
| `-rp` | Enable the session before ML initialization. |
| `-rp_file PATH` | Choose output; default `profile.rp`. The file is replaced. |
| `-rp_paused` | Start the enabled session paused. |
| `--` | End runtime options; later arguments belong to the application. |

`-rp_file` and `-rp_paused` require `-rp`. Unknown `-rp...` options are errors
before the application-argument boundary. In particular, the proposed timer,
GC-sampling, and reporting flags are not implemented. With no `-rp`, the API
is inert and creates no output file, including in uninstrumented programs.
An executable without profiling metadata rejects `-rp` before starting ML.

The API is process-wide:

- `start ()` takes a snapshot when transitioning from paused to active.
- `pause ()` takes a snapshot when transitioning from active to paused.
- `sample ()` always takes an explicit snapshot in an enabled session, even
  while paused.
- `mark s` writes a timestamped label, including while paused.
- `flush ()` flushes the output without sampling.

Repeated `start`/`pause` calls in the same state do not add samples. M1 has no
periodic samples, so an enabled session records only explicit and transition
snapshots. Large-object bookkeeping continues while paused, allowing a later
snapshot to include storage allocated during warmup. The file closes at normal
process exit; call `flush` when another process needs to read a prefix.

## Measurements and format

`profile.rp` is a version 1 JSON-lines stream. The header identifies the format,
byte/nanosecond units, machine-word size, and region-page size. Counts are
unsigned 64-bit integers; readers must preserve their integer precision.
Timestamps use a monotonic wall clock relative to session initialization.

A snapshot consists of `sample_begin`, zero or more `region` records, and
`sample_end`. Only the end record commits a snapshot. Readers can ignore a
partial final line or an unfinished final snapshot; malformed complete
records are errors. M1 serializes synchronously while the single ML thread is
stopped. Later parallel sampling will need copied records and serialization
outside the rendezvous.

Region records identify the compilation unit, binding, and thread (always 0
in M1), and contain these byte measurements:

- `page_footprint`: page count times page size minus the unused last-page tail.
  This includes page headers and slack in earlier pages, not just payload.
- `large_bytes`: requested words on the large-object allocation path, including
  language-level object headers but excluding malloc bookkeeping and the
  runtime's large-object list node.
- `finite_bytes`: reserved stack space for the active finite-region binding,
  even if its contents have not been initialized yet. A caller's result region
  can therefore appear while the callee is still computing its result.
- `descriptor_bytes`: space for infinite-region descriptors, kept separate from
  finite storage and page usage.

Records also contain page count and unused tail, so reserved page capacity can
be recovered. Global regions are discovered through the context's region
chain and labelled `<global>`; regions already visited through frame maps are
not counted again. Compiler-generated unit/binding IDs identify local regions;
source-level ReML display names are deferred to M4. Recursive instances can
produce several records with the same unit and binding; sum them for a binding
view instead of overwriting them.

The original M1 reader emitted CSV summaries and JSON snapshots. It has since
been replaced by the SML `rpview` file-to-HTML tool; see the current guide. The raw stream also
contains markers; ML string labels are escaped byte-by-byte, preserving NUL
and control bytes. M1 does not export legacy object-allocation-site data or
claim to measure process RSS, reachable data, cached free pages, or total
non-region stack storage.

## Native maps and limitations

Each native call continuation carries a reverse-order map immediately before
its return address. Explicit sampling calls receive a separate map for the
current frame. The maps contain frame offsets and active lexical bindings
using offsets already assigned by `CalcOffset`. The emitters track nested
`LETREGION` scopes and do not enumerate region arguments as additional owners.
Finite slots can be reused across disjoint scopes.

Maps carry a version magic and a relative reference to the unit-name string,
which avoids absolute text relocations on macOS. The caller-base delta accounts
for spilled results, and the return-slot offset accounts for spilled arguments
and each backend's frame header. Compilation-unit entry stubs terminate the
walk. The native ABI is specified in `src/Runtime/RegionProfile.h`.

The compiler rejects sampled profiling combined with GC, parallelism, value
tagging, or the old profiler. The REPL rejects the new mode until M4. A sample
inside an exported C-to-ML callback is diagnosed explicitly: such a boundary
cannot yet connect the callback to an enclosing suspended ML stack. Ordinary
C calls, including allocations and explicit sampling, are supported. The old
profiler remains available independently.

## Validation

Run the M1 suite with freshly built tools and runtime:

```sh
MLKIT="$PWD/bin/mlkit-arm64" REML="$PWD/bin/reml-arm64" \
  sh test/region_profile/check.sh
```

On X64, select the X64 compiler executables and matching runtime/CC. The suite
retains its temporary artifacts and tests controlled runtime maps, generated
ReML region lifetimes, page growth, resets, finite reservations, large objects,
recursive frames, exception handlers, spilled arguments, API state transitions,
embedded-NUL markers, application arguments, disabled sessions, missing
metadata, and rejected callback sampling. Reader checks cover incomplete
snapshots, malformed records, and integers beyond IEEE-754 precision.

Local validation on Apple Silicon:

- Built native ARM64 MLKit/ReML and the X64-generating MLKit/ReML compilers.
- Passed the M1 generated-code suite on ARM64, including an instrumented Basis.
- Passed the standalone runtime fixture under AddressSanitizer and UBSan.
- Built all ten standard runtime variants; passed existing allocation,
  old-profiler streaming, and parallel C-allocation smoke tests.
- Assembled profiler-enabled X64 code. X64 execution checks remain pending for
  the ThinkPad; cross-assembly is not a substitute for those checks.

Overhead benchmarks and a decision about maintaining page counts belong to M2.
