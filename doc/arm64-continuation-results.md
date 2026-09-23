# ARM64 static-data and code-suffix results

## Initial changes (`9423b92`)

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

## Validation of the initial changes

Both nucleic executables produce the expected output. Their two assembly files
contain the same 243,120 text instructions after normalizing fresh symbol names;
branch targets and register operands are preserved. Static-data order can change.

The focused native ARM64 suite passes for MLKit and ReML, including GC, closures,
exceptions, foreign calls, spills, large frames, profiling, and REPL modes. A new
regression compiles 2,000 nested scope/region wrappers and checks that their
following continuation executes exactly once in order (output `AB`).

## Completing suffix passing

The follow-up extends suffix passing to all primitive lowering, constructor and
scratch-memory setup, automatic C-call conversions, worker/export bridges,
link-time runtime helpers, and REPL entry code. Record prefixes now describe
constant/address fields instead of storing prebuilt instruction fragments.
Foreign-argument loaders also take the suffix directly.

`CG` emits metadata and static data before the completed function text, avoiding
the final copy of the whole text list. Static chunks are folded onto that text
once. Remaining `@` operations join operand/register lists or string components,
not generated instruction lists. The register palette and ABI remain unchanged.

The evaluation first compiles and runs `nucleic.mlb`, comparing the previous
compiler (`9423b92`) with the fully converted compiler using the same command,
warmed Basis, and cleared test-local caches as above. The initial result was small, so the comparison was repeated for five compilations
per version, alternating which compiler ran first. No builds or other tests ran
concurrently with the measurements.

| Run | Previous compiler CG | Full suffix passing CG |
| --- | ---: | ---: |
| 1 | 0.547 s | 0.537 s |
| 2 | 0.545 s | 0.534 s |
| 3 | 0.546 s | 0.540 s |
| 4 | 0.546 s | 0.535 s |
| 5 | 0.545 s | 0.536 s |
| **Median** | **0.546 s** | **0.536 s** |

The additional improvement is **0.010 s (1.8%)** at the median. It is consistent
across these runs, but modest: most of the benefit measured earlier came from
the first conversion. These measurements do not isolate the contribution of each
helper or of the final text-copy removal. [Raw timing files](arm64-performance/nucleic-full-suffixes/)
retain both CG entries for every run.

All ten nucleic executables produce the expected output. The first pair's two
assembly files have the same 243,120 text instructions after normalizing fresh
symbol names, including the same branch structure and register operands.

This follow-up has been evaluated on nucleic only. The wider native, ReML,
parallel, profiling, and regression suites have **not** been rerun for this
version; the earlier validation section refers to `9423b92`. The change remains
in the draft integration PR pending that broader validation.

The subsequent [allocation/reset implementation](arm64-allocation-paths.md)
retains full suffix passing and has passed the wider native, parallel, developer,
GC, and explicit-region validation listed in that report.
