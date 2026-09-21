# ARM64 static-data and code-suffix results

## Changes

Static data now accumulates as a list of fragments in reverse insertion order,
with one ordered flattening per output. Adding a constant, frame descriptor,
worker entry, or exported hook no longer copies all previously accumulated data.

The emitter now passes an instruction-list suffix through low-level helpers,
ML calls/results, allocation-helper calls, record construction, GC entry,
foreign argument placement, nested scopes, regions, handlers, and switches.
Functions and statements prepend directly to their suffix instead of repeatedly
flattening completed bodies. Indexed folds avoid intermediate lists of fragments.
Small list-returning wrappers remain for bounded primitive fragments and existing
utility callers; final assembly of text and metadata still uses a linear append.
There is no function-valued continuation allocated per emitted instruction.

The register palette, save/restore sets, argument staging, stack layout, and ABI
are unchanged. In particular, internal calls still preserve the same 54 registers.
Statement generation now visits bodies right-to-left, so fresh symbol identities
and static-data order can change. Labels retain their associated metadata, and
function frame context stays fixed while that function is emitted. The runtime
sorts return-PC entries before lookup.

## Nucleic CG comparison

One fresh compilation of `test/nucleic.mlb` with each native ARM64 compiler,
using `-gc`, the same warmed Basis cache, and no test-local compilation caches.
The old compiler was saved before these changes (emitter at `8349c59`); the new
compiler contains both the static-data and suffix changes above. All compiler
builds used MLKit with `-gc`.

| Compiler | CG user CPU time |
| --- | ---: |
| Old | 1.733 s |
| New | 0.556 s |

CG time decreases by **1.177 s (67.9%)**, about **3.1× faster**. Each figure sums
the CG entries for `nucleic.sml` and `main.sml`; this is a single-run comparison,
not a repeated benchmark. Register handling is unchanged.

The command, run from a temporary copy of `test`, was:

```sh
SML_LIB=/private/tmp/mlkit-m2-source "$compiler" \
  --mlb-subdir M8Timingbase --no_delete_target_files \
  --timings --log_to_file -gc nucleic.mlb
```

`$compiler` selected the saved old or newly built compiler. All test-local `MLB`
directories, including `nucleic/MLB`, were removed before each compilation.
Raw phase timings: [old](arm64-performance/nucleic-continuations/old.KITtimings)
and [new](arm64-performance/nucleic-continuations/new.KITtimings).

These are code-generation phase times, excluding assembly/linking and program
execution. They **include compiler garbage collection**: `Timing.sml` uses
`Timer.checkCPUTimer`, which includes collector time in MLKit. The misleading
HTML and LaTeX kittester captions are corrected alongside this change.

## Validation

Both nucleic executables produce the expected output. Their two assembly files
contain the same 243,120 text instructions after normalizing fresh symbol names;
branch targets and register operands are preserved. Static-data order can change.

The focused native ARM64 suite passes for MLKit and ReML, including GC, closures,
exceptions, foreign calls, spills, large frames, profiling, and REPL modes. A new
regression compiles 2,000 nested scope/region wrappers and checks that their
following continuation executes exactly once in order (output `AB`).
