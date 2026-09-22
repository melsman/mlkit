# simple and mpuz without GC

Both `-no_gc` slowdowns reproduce, but their main causes differ from UF's
C-call setup. Isolated code-generation experiments recover simple's gap and
reduce most of mpuz's gap. Production compiler and runtime sources are
unchanged.

## Final paired measurements

| Benchmark | Current ARM (s) | Diagnostic ARM (s) | X64 / Rosetta 2 (s) | Diagnostic / X64 |
|---|---:|---:|---:|---:|
| simple | 0.5420 | 0.4402 | 0.4552 | 0.967 |
| mpuz | 0.3905 | 0.3091 | 0.2763 | 1.119 |

Simple improves by 18.8%, reaching slightly better than X64. Mpuz improves
by 20.8%; a residual 11.9% slowdown remains unattributed by these experiments.
The diagnostic changes preserve the algorithm, arithmetic overflow checks,
and bounds checks. They are experiments, not fully validated compiler fixes.

The setup is the same Apple M2 Max, macOS 26.5.1, and rebuilt non-GC
compilers, runtimes and Basis libraries as the
[20-program report](arm64-nogc20-current.md). Measurement date: September 23,
2026. Each configuration receives one warmup and seven measured runs in
shuffled order; tables use medians. All builds and profiling runs finish
before timing, and all executions match expected stdout and exit successfully.
No MLton is used. Experimental variants replace only the benchmark's ARM
assembly object and link the same remaining objects.

## simple: a poor paired-load choice in the list traversal

The benchmark implements arrays as lists of references. Its `sub'` routine
walks those lists repeatedly. A diagnostic CPU-time sampler recorded 1,036
of 1,164 samples (89%) in that routine.

The hot traversal contains:

```asm
ldp x3, x1, [x1, #0]
```

The second result, x1, becomes the pointer for the next iteration. Replacing
this one instruction with:

```asm
ldr x3, [x1, #0]
ldr x1, [x1, #8]
```

reduces time from 0.5389 s to 0.4665 s in the isolated load experiment: a
13.4% improvement despite executing more instructions. This is evidence
against this pairing in a pointer-dependent loop on this machine. We did
not measure hardware load-latency counters or establish a general cycle
count for `ldp`.

The current self-loop recognition also misses this routine because its
whitelist includes tagged integer comparisons but omits the corresponding
untagged 64-bit comparisons. As a result, each iteration restores the frame
header and then saves it again on re-entry. Reusing the frame alone gives no
measurable improvement (0.5376 s baseline versus 0.5412 s diagnostic in the
initial batch). Once the paired load is split, frame reuse gives a further
benefit, reaching 0.4403 s in that batch and 0.4402 s in the final repeat.

Recommendation: make load-pair selection sensitive to pointer dependencies,
and extend the audited self-loop primitive set to equivalent untagged
operations. Preserve profitable pairing elsewhere: splitting mpuz's paired
loads does not help.

## mpuz: unnecessary work on the already-used-digit path

Mpuz enumerates assignments of digits to letters. Its assignment-loop
function (`app15` in the assembly) accounts for 442 of 822 samples (54%);
five string-to-integer folds account for another 324 (39%). Those folds
compute `value * 10 + digit` with checked integer arithmetic.

The assignment loop performs three stack stores before determining whether
the digit is already used: it saves the closure/context, the remaining list,
and the digit's mutable flag reference. A used digit immediately skips the
recursive assignment call, so those stores and the subsequent frame teardown
and rebuild are unnecessary on that path.

The largest improvement comes from keeping the current frame for the next
iteration and delaying those stores until the unused-digit branch that
actually makes the recursive call. The used-digit path advances directly to
the next list node. That isolated combination reduces time from 0.3874 s to
0.3205 s (17.3%). It does not change which assignments are visited.

There are secondary costs:

- Two reference updates and one array update stage their pointer/value
  through temporary stack slots even though registers suffice.
- Nine checked-multiplication sites stage x16 through the stack while loading
  x17, despite that operand load leaving x16 untouched. The diagnostic removes
  that staging and adjusts stack-relative operand offsets; the signed
  high-product and overflow checks remain intact.
- The assignment loop has 32 bytes of locals, so the existing zero-local-frame
  self-loop optimization cannot cover it even after auditing more primitives.

In separate paired batches, removing multiplication staging alone improves
mpuz by about 4%; eliminating reference/array-store temporaries alone improves
it by about 3%; reusing the assignment-loop frame alone improves it by about
4%. Combining these with delayed branch-specific stores reaches 0.3046 s in
one batch and 0.3091 s in the final repeat, versus X64 at 0.2751–0.2763 s.

Two negative results narrow the recommendation:

- Reusing only the frames of the five string-to-integer loops does not help.
- Splitting the assignment loop's two heap `ldp` instructions makes the
  baseline slightly slower. Adding that change to the best other diagnostics
  also does not improve the result (0.3105 s versus 0.3091 s).

Recommendation: improve spill/store placement across branches, permit safe
frame reuse with locals, and avoid temporary staging for operands that can
remain in registers. For GC configurations, any generalized loop transform
must retain required safe points and valid root metadata. These measurements
are specifically for `-no_gc`.

## Profiling and reproducibility

macOS `sample` did not produce usable stack samples for these processes.
The usable profile comes from a separately linked diagnostic C sampler using
`ITIMER_PROF` and a signal-context program counter, over three executions per
benchmark. PCs are normalized by the executable's ASLR slide and mapped to
symbols; addresses outside its text segment are classified separately. These
samples guide function selection, not precise attribution to individual
instructions. Timed executables contain no sampler.

[Raw samples, instruction-sampling data, hashes, captured link commands and diagnostic scripts](arm64-performance/simple-mpuz-investigation/)
are retained. Scripts use this machine's temporary paths. Run `build.py`,
then `experiments.py`, `load-experiments.py`, `mpuz-experiments.py`,
`branch-experiment.py`, and `final-build.py` to construct the variants;
`measure-final.py` performs the final paired comparison. `sample-build.py`
builds and runs the separately instrumented executables.
