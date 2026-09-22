# Typed ARM64 instructions

`InstsArm64` now exposes an instruction datatype, following the X64 backend's
approach. Instructions have constructors with fixed operand counts, such as
`add`, `ldr`, `b_ne` and `tbz`. The old `Op of string * string list`
representation and string-based instruction builder are removed.

The shared declarations live in `Arm64Instructions.sml`, allowing the separate
`PrintArm64.sml` pretty-printer and `InstsArm64.sml` optimiser to consume the same
types without a dependency cycle. `INSTS_ARM64.sml` exposes the constructors.
The pretty-printer maps `b_ne` to `b.ne`; `and_` avoids the SML keyword `and`.

Operands are also typed: register widths, integer immediates, conditions,
shifts, memory addressing modes, labels and Mach-O relocations. For example:

```sml
A.ldr (R(X 0),M(X 1,8))
A.subs (R(X 2),R(X 2),I 2)
A.b_ne (L target)
```

Code generation constructs these values directly. Its numerical-operation
dispatch also uses a datatype rather than operation-name strings. Optimisation
matches constructors and operands, with no parsing of mnemonic, register or
address text. Label tables use label identity rather than printed names.

Directives used for branch-range calculations are typed as well: sections,
alignment, globals, data widths and reserved space. Data expressions retain
text payloads, as do floating constants. An opaque `Raw` directive is retained
for unknown assembler directives and remains a conservative range barrier;
ordinary generated code does not use it.

The suffix-passing construction strategy, register allocation, internal ABI,
inlining policy and optimisation rules are retained. Zero-offset memory
operands now have one representation, including addresses formerly printed
as either `[sp]` or `[sp, #0]`.

## Validation

MLKit, ReML and the emitter test are built using MLKit with `-gc`. The native,
parallel and dedicated switch suites pass. Native coverage includes GC,
generational GC, profiling, forced collection, exception unwinding, foreign
calls, stack results and large frames. Argobots is skipped because it is not
configured locally.

The emitter tests now construct typed instructions. Existing peephole
rejection cases and branch/address-range boundaries are retained, with
separate pretty-printer checks for register widths, signed immediates, shifted
operands, conditions and underscore-to-dot branch spelling.

## Nucleic compilation comparison

Three fresh compilations per compiler, alternating order, with the same warmed
`M12SelectFinal` Basis cache and no benchmark-local compilation caches. Both
compilers are X64-hosted MLKit `-gc` builds running under Rosetta2. The baseline
is `ec0c186`; the new compiler contains this representation refactor.

| Median | Before | After | Reduction |
| --- | ---: | ---: | ---: |
| CG user CPU time | 0.098 s | 0.058 s | 40.8% |
| Full compilation wall time | 3.839 s | 3.711 s | 3.3% |

CG sums the entries for `nucleic.sml`, `main0.sml` and `main.sml`; it includes
compiler GC time. Its timing boundary is `CodeGen.CG`: the peephole pass,
relaxation and printing run later in `CodeGen.emit`. Some formatting work has
therefore moved outside that timer. No separate optimiser speedup is claimed.
Full compilation includes assembly/linking and shows a smaller improvement;
three runs are a focused check, not a precise long-run performance estimate.
Validation and other builds had finished before these measurements.

All six executables matched expected output. The old and new assembly for
all three ML units and the generated link unit is identical after normalising
whitespace, hexadecimal immediates and explicit zero-offset address spelling.

[Raw timings, compiler hashes, assembly comparison and reproduction scripts](arm64-performance/typed-instructions/)
are retained. Script paths refer to the local build directories and must be
adapted for another checkout.
