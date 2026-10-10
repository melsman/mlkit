# T2: function ranges and image identity

The macOS ARM64 backend emits code metadata when compiling with `-rp` (the
alias for `-region_profile`). This supplies the function-address foundation for
issue [#241](https://github.com/melsman/mlkit/issues/241).
[T3](t3-recording.md) adds buffered time sampling; attribution and reporting
remain T4–T5 work.

## Native tables and identities

Each generated ML function has a half-open range `[begin, end)`. The start is
its existing function label; the end is a new label immediately after its
complete generated body. Prologues, epilogues, inline continuations, and branch
relaxation are inside the range. Alignment preceding the next function is
outside it. A tail jump belongs to its issuing function until control transfers
to the target; an optimized self-tail loop remains in its own range. Inlined
source functions have no separate emitted code and are charged to the generated
function containing them. This describes generated functions, not exact source
calls or call counts.

Native metadata version 1 uses six machine words per function: relocated start
and end pointers, then pointers to the existing ML strings for unit, function,
source, and IR identity. A unit table has a version/count header. The linker
emits a null-terminated table of pointers to the code tables of **all linked ML
units**, including separately compiled Basis/library units. References retain
these tables together with their code. Function identities reuse the allocation
profiler's `(unit, function)` pair; assembly symbols add the `F.` prefix.

T2 introduced a `_CODE1` suffix for profiled ARM64 compilation caches;
[T4](t4-attribution.md) advances it to `_CODE1_TP1` for boundary-state instrumentation. This prevents old
cached objects without the new unit-table symbols from being reused. External
precompiled ML libraries must likewise be rebuilt with the new compiler and
`-rp`. Ordinary, unprofiled compilation keeps its cache namespace and code path.

## Recording and ASLR

At profiler initialization, before arming a sampling timer, the runtime reads
the main Mach-O image's UUID, load address, and `__TEXT,__text` bounds. It verifies
that each declared function range lies inside this executable's text section.
It then writes an additive, independently versioned extension to the current
version-10 profile stream:

| Tag | Record | Integer fields | String fields |
| --- | --- | --- | --- |
| 20 | `code_metadata` | `metadata_version`, `function_count` | `scope` |
| 21 | `code_image` | `image`, `load_address` | `build_id`, `path` |
| 22 | `code_function` | `image`, `start`, `end` | `unit`, `function`, `source`, `ir_identity` |

Integers use the existing explicit little-endian encoding. Function start/end
values are byte offsets from the loaded Mach-O header, not absolute addresses.
Image ID 1 denotes the initial executable; `build_id` is its 32-character,
lowercase hexadecimal Mach-O UUID. The path is descriptive and is not used to
establish identity. Relaunching the same executable preserves its UUID and
relative ranges despite ASLR; linking another executable obtains its own UUID.
The recording's load address translates its interrupted absolute PCs into those
relative ranges. The allocation session's existing build ID is retained for its
existing purpose, separately from the image UUID.

Scope is `static-executable`, `unavailable`, `unsupported-repl`,
`unsupported-platform`, or `missing-image-id`. Missing code metadata in older
version-10 streams is supported and treated as unavailable. The updated reader
understands these records; older viewers reject their unknown tags, so use a
viewer built with this change for new recordings.

The reader validates identities, image references, range bounds, non-overlap,
and metadata version. It requires the declared function count for resolution:
a truncated metadata block returns unavailable rather than trusting a partial
index. A completed recording with an incomplete block is invalid. PC arithmetic
uses `IntInf` throughout, including addresses beyond JavaScript's exact integer
range. A sorted vector provides binary search outside signal context.

## Inspect a PC

Compile an executable with `-rp` and record its metadata with
`+RTS -rp -rp_interval 0 -rp_file profile.rp`. This disables periodic region
snapshots while retaining initialization metadata. Obtain the interrupted PC and
image UUID from the **same sampled executable**. The T1 helper does this for the
regression test; production time-sample records are not implemented yet.

```sh
bin/rpview profile.rp --format json -o records.json
bin/rpview profile.rp --resolve-pc 0xADDRESS --image-build IMAGE_UUID
```

`--resolve-pc` requires the expected image UUID and prints one JSON result.
`function` results contain the reused function identity, source, IR identity,
range, and image identity. Other statuses are `unknown-pc`,
`metadata-unavailable`, and `build-mismatch`. PCs at a function's end are outside
that function. A UUID mismatch never resolves to a function. Raw PCs must also
use the recorded execution's load-address coordinate; a PC from another launch
needs that launch's recording or an explicit relocation first.

The resolver does not read the executable or use a host symbol lookup. All
required ranges and identities are copied into the recording, so the original
binary can be moved or removed before offline inspection. The internal resolver
may omit the expected UUID only when its caller already knows that samples and
metadata belong to the same recording; the CLI requires it for external PCs.

## Supported scope and attribution

This first implementation covers statically linked macOS ARM64 ML executables
and their linked ML library units. REPL/dynamically loaded ML code and other
platforms explicitly report unavailable metadata; they do not acquire invented
ranges or reuse the static executable's identity. Runtime entry bridges,
allocation/GC stubs, C primitives, and GC implementation functions do not have
ML ranges and resolve as `unknown-pc` here.

T4 will distinguish GC and foreign execution using state maintained at GC and
ML/C boundaries. It will charge foreign execution to the initiating ML function
and let an actual ML range match take precedence during a C-to-ML callback.
T2 does not infer these categories from symbol names or perform any lookup in a
signal handler.

## Validation

```sh
sh test/time_profile/check-code-metadata.sh
```

The regression covers GC and no-GC builds, generated tail-recursive code,
retention of separately compiled Basis and List metadata, range boundaries,
real interrupted ML PCs, explicit unknown C/GC PCs, repeated launches under ASLR,
unsupported REPL scope, and rejection of a different linked image's UUID. It also generates a standalone
HTML region report to check the added metadata does not break existing output.
An SML resolver test checks addresses above 2^53, truncated metadata, overlapping
ranges, duplicate identities/images, empty and overflowing ranges, unsupported
metadata versions, and image references. Existing viewer/linker metadata checks
remain applicable. The native regression is included in macOS ARM64 profiler CI.
