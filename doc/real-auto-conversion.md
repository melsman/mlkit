# Real auto-conversion implementation plan

Tracking issue: https://github.com/melsman/mlkit/issues/161

Extend the C FFI auto-conversion described in manual section 19.9 so ML
`real` arguments and results correspond to C `double`. Support only 64-bit
reals; Real32 is outside this work. Implement Arm64 first, then X64.

The intended design uses `float64` at the foreign-call boundary, with
`real64` boxing and unboxing expressed through the existing conversion
primitives. Choose a lowering point early enough that region inference
accounts for result allocation, and preserve the unboxed representation
through LambdaExp, RegionExp, and MulExp.

## Milestones

- [x] **M1: Define the conversion contract and lowering point.** Specify
  real arguments/results and mixed signatures; trace the existing
  `__real_to_f64` and `__f64_to_real` primitives and identify where to insert
  conversions. Record the typing, evaluation-order, and allocation invariants.
- [x] **M2: Implement typed lowering through the shared compiler.** Insert
  conversions around auto-converted calls, preserve `float64` through closure
  conversion, and extend the foreign-type mapping. Audit downstream consumers.
  Verify IR dumps show unboxed call arguments/results and correct result boxing.
- [x] **M3: Implement Arm64 call generation.** Reuse the scalar ABI machinery
  for floating-point arguments and results. Cover mixed integer/floating-point
  signatures, register exhaustion, stack arguments, and live values across
  calls. Verify the floating-point registers and stack layout in generated code.
- [x] **M4: Add Arm64 end-to-end regression coverage.** Test real arguments,
  real results, mixed signatures, stack-passed arguments, negative zero,
  infinities, and NaNs. Exercise GC and profiling configurations, including
  allocation after a real result. Add focused assembly checks for direct
  `double` passage where runtime tests cannot establish the representation.
- [x] **M5: Support the X64 backend.** Extend X64 auto-converted calls to use
  the shared `float64` lowering and the target's floating-point C ABI. Cover
  mixed signatures, register exhaustion, stack alignment, result handling,
  and live values across calls. Run corresponding runtime and assembly
  regressions, including GC and profiling configurations, on X64.
- [x] **M6: Document and validate the completed implementation.** Update manual
  section 19.9 and add an ML/C example. Run the relevant Arm64 and X64 suites
  and check clear diagnostics on unsupported backends. Record validation in
  the PR and mark it ready for review when all implementation milestones pass.

## M1: Conversion contract and lowering decision

### Contract

An auto-converted call (`prim ("@name", ...)`) accepts ML `real` in each
individual argument position and as its single result type. Each corresponds
to a C `double`, including in signatures mixed with existing supported types.
For example, `(real, int) -> real` corresponds to `double name(double, long)`
with the existing integer conversion rules. Zero-argument calls returning
`real` and calls taking reals but returning an existing supported type also
follow this rule. Existing argument decomposition is retained: this does not
add recursive conversion of records, lists, arrays, or other containers.

The conversion copies the binary64 payload without arithmetic conversion or
narrowing. Signed zero, infinities, and NaNs follow the existing boxing/unboxing
semantics; no extra NaN canonicalization is introduced. No region pointer or
ML real-object pointer is passed for a real argument or result. This work adds
neither Real32, variadic-call syntax, nor floating-point callbacks/exports.
Ordinary calls without `@` retain their existing ML representation convention.

### Lower in CompileDec before region inference

Implement the rewrite in `compile_application_of_c_function` in
`src/Compiler/Lambda/CompileDec.sml`. It already decomposes the elaborated
argument type into `taus1`, compiles the result into `tau2`, and constructs the
typed `CCALLprim`. After that decomposition, identify concrete real positions
using `TyName.eq` with `tyName_REAL`, replace those call types with
`LambdaExp.f64Type`, and wrap the corresponding expressions in
`__real_to_f64`. If the result is real, change the inner call result to
`f64Type` and wrap the complete call in `__f64_to_real`. Keep the inner `@`
name so closure conversion still selects `CCALL_AUTO`.

Schematic lowering for a real result:

```text
prim("@f", (x : real, n : int)) : real
  => __f64_to_real(
       prim("@f", (__real_to_f64(x), n)) : float64)
```

Use the existing `ccall` helper for the conversion primitives. The original
`__blockf64` lowering in the same file already constructs `__real_to_f64`
this way. Build the inner arrow type from the converted argument and result
types before deriving its type variables and substitution. Preserve the
existing instantiation machinery for other positions; do not guess real
representations for unresolved type variables.

### Typing, order, and allocation invariants

- The outer expression has precisely the original ML result type. Each
  conversion is typed `real -> float64` or `float64 -> real`; the inner
  foreign-call arrow matches the converted expressions.
- Evaluate every original argument exactly once, in the same order as an
  ordinary primitive call, and invoke C exactly once. Wrap arguments in place
  without reordering them. If normalization needs temporary bindings, follow
  its existing primitive-argument order. Box the result only after C returns;
  exceptions during argument evaluation prevent the call as before.
- `TyName.sml` defines `tyName_REAL` as boxed with one region parameter and
  `tyName_F64` as unboxed with none. `SpreadExpression.sml` spreads each typed
  `CCALLprim` using `RType.sigma_for_c_function` and annotates result storage
  using `RType.c_function_effects`. Therefore the inner float64 result needs
  no real-result storage, while the outer conversion carries the real
  allocation through RegionExp and MulExp. Attach explicit result-region
  annotations to the outer boxing result type when a real result is rewritten;
  do not leave them on the unboxed inner call. Preserve existing annotations
  for calls whose result is unchanged.
- Reuse existing boxing allocation, tagging, GC, and profiling machinery.
  Keep the returned float64 live across any allocation needed for boxing.
  Backend code must preserve other live values across the foreign call and
  any later allocation; do not treat a float64 as a GC pointer.

### Downstream implementation boundary

`ClosExp.tn_to_foreign_type` currently rejects both real and float64. Add a
foreign `Float64` case for `tyName_F64` in M2, including its signatures,
printers, serialization, and downstream representations. Do not map boxed
`tyName_REAL` directly to that case: the early rewrite supplies the unboxed
representation. The type mapper is also used by exports, so audit that path
without silently adding floating-point export support.

At M1, Arm64 already implemented `Real_to_f64` by loading the real payload and
`F64_to_real` by storing it in allocated result storage. However,
`CodeGenArm64.autoCallInto` marshalled through the integer call path
and wrote its result from `X 0`. M3 uses the scalar floating-point ABI
for the new case, including a result in `D 0`, rather than only extending the
foreign-type enumeration. X64 requires the corresponding work in M5.

M1 is validated by tracing these existing construction, region-spreading,
type-definition, and backend conversion paths. M2 must verify the resulting
IR and preserve argument effects and explicit result-region behavior in
regressions; M3/M4 and M5 establish the actual ABI and GC behavior.

## M2: Shared lowering

`CompileDec` now rewrites concrete real argument positions to `float64` and
wraps real results in the existing boxing primitive. A single explicit
result-region argument becomes an annotation on the outer boxed result type;
it is not passed as an extra region pointer to either primitive. Multiple
explicit result regions are rejected for the single boxed real result.
Calls without `@` and all
other argument/result types retain their previous representation.

The closure and linear IR foreign-type enumerations and printers now carry
`Float64`, mapped only from `tyName_F64`. There is no separate foreign-type
serialization in these IR modules. Exports explicitly reject float64.

At M2, the auto-call register resolver only assigned integer registers and
rejected Float64 calls before assigning registers. M3 replaces that path for
Arm64; M5 also replaces it for X64. Both native backends now stage auto-call
operands in code generation.

The focused regression script is `test/real_auto_conversion/check-ir.sh`.
It exercises mixed signatures, real arguments with integer/unit results,
zero-argument real results, and an ordinary real call, in plain, GC, and
profiling modes. It also checks explicit result storage in ReML mode.
Arm64 ABI execution and special-value coverage are added in M4; M5 extends
the same fixtures to X64.

Validation for M2:

- Built the Arm64 compiler with MLton and rebuilt the final sources with the
  source-built compiler.
- Ran `check-ir.sh` successfully in plain, GC, profiling, and explicit-region
  modes. Checks cover conversion placement, unboxed call results without
  result storage, boxed-result storage, foreign types in linear IR, and the
  unchanged ordinary-call path.
- Ran existing integer auto-conversion and real arithmetic/boxing smoke
  checks in plain, GC, and profiling modes.
- Type-checked the final X64 compiler with MLton; checked shell syntax and
  `git diff --check`.

## M3: Arm64 ABI handling

Arm64 declares `REGISTER_INFO.staged_auto_ccall`, so register allocation
retains auto-call operands and their result in allocated registers or spill
slots instead of imposing integer C registers. Existing call liveness and
fetch/flush machinery preserves values across C; unboxed floats crossing C
calls are spilled by the current allocator. M5 applies the same strategy to X64.

`CodeGenArm64.autoCallInto` classifies `Float64` arguments as `AbiArm64.F64`
and other supported arguments as word-sized scalars. It uses the existing
scalar call emitter to stage every argument before changing ABI registers,
place floats in d0–d7 independently of x0–x7, and pass overflow arguments in
aligned stack slots. The real result is read from d0 into its allocated
destination; the shared lowering subsequently boxes it in ML result storage.
Boxed integer results still preserve their storage across C calls.

The scalar emitter retains foreign-call GC deferral and profiling context
save/restore. Result boxing and allocation use the existing region/GC code,
including preservation of live float results across allocation helpers.

## M4: Arm64 regression coverage

`test/real_auto_conversion/check-arm64.sh` compiles the ML fixtures against
C functions with ordinary `double` signatures. Its checks cover:

- Real arguments and results, zero-argument real results, integer/boolean/unit
  results, boxed Int64 results, and mixed bool/string/integer arguments.
- Nine float arguments and twenty interleaved integer/float arguments, using
  both ABI register banks and overflowing both onto the aligned outgoing stack.
- Exact binary64 payloads through result boxing and argument unboxing:
  negative zero, positive/negative infinity, a quiet NaN with payload, a
  subnormal, and an ordinary finite value.
- Argument effects evaluated once in left-to-right order, and argument
  exceptions preventing the C call.
- Nine optimised unboxed values live across a C function that deliberately
  clobbers all caller-saved FP registers.
- Five hundred boxed real results retained in a list while C requests
  collection before returning each result. GC builds assert that collection
  is deferred in C and actually occurs afterward; the test then checks every
  retained real. Profiling builds sample while the list is live and verify
  recorded allocation sites and occupancy for the real results.
- Explicit result-region storage in ReML mode.

The matrix includes plain, GC, generational GC, each corresponding profiling
mode, disabled register allocation, disabled optimisation in plain/GC modes,
and forced GC checks. Assembly checks verify double passage through FP
registers, mixed outgoing stack slots, and optimised FP arithmetic in the
live-value test. `make check-arm64` now runs both the shared IR script and
this execution suite.

Validation for M3/M4:

- Rebuilt the final Arm64 MLKit and ReML compilers from source.
- All four shared IR checks, eleven execution configurations, and the
  assembly checks passed with the final MLKit compiler.
- The existing native Arm64 MLKit/ReML suite passed, including scalar ABI
  generation, calls/callbacks, GC relocation/descriptors, regions, exceptions,
  floating-point operations, spills, large frames, and target guards.
- Existing boxed-integer auto-call profiling attribution and IR navigation
  checks passed.
- The final X64 compiler type-check, shell syntax checks, and
  `git diff --check` passed.

## M5: X64 ABI handling

X64 also retains allocated operands until code generation. Its auto-call
emitter stages all arguments before profiling/GC policy changes or ABI
register placement. Integer arguments use rdi, rsi, rdx, rcx, r8, and r9;
float64 arguments independently use xmm0–xmm7. Overflow arguments retain
source order in eight-byte outgoing stack slots, with the whole frame aligned
to sixteen bytes. Real results are read from xmm0. The emitter sets rax to
the number of FP argument registers used for the System V call convention.

The staging frame saves boxed integer result storage, the previous GC policy,
and profiling context as needed. Foreign auto-calls defer collection in GC
builds, restore the prior policy on return, and preserve result bits through
profiling restoration and subsequent ML boxing. Signed tagged integers use
an arithmetic shift when converted to C, including in mixed signatures.
Tagged words have a distinct foreign type and use a logical shift, preserving
their high bit. The execution tests cover both negative integers and maximal
default-word arguments and results.

The execution runner is now shared in `check-native.sh`, with `check-arm64.sh`
and `check-x64.sh` wrappers. The C live-value fixture clobbers all caller-saved
FP registers for each architecture. X64 assembly checks verify its independent
register banks, six overflowing stack arguments in the mixed fixture, and
optimised unboxed values across the clobbering C call.

Local X64 validation used Rosetta on Apple Silicon with isolated X64 runtime
archives built from the current sources. All eleven execution configurations,
the assembly checks, and four shared IR modes passed. Fourteen existing
`test_dev` cases (auto/auto2, C calls, f64_1–f64_4, real arithmetic/comparisons,
real references, and exceptions) passed in plain, generational-GC, and both
profiling modes: 56 tests total. The shared runner also passed all eleven
Arm64 configurations again. Final X64 type-check and shell syntax checks
passed. Native Linux execution on the ThinkPad also passed all eleven
configurations, the assembly checks, four shared IR modes, and the same
56 existing regression tests.

## M6: Documentation and final validation

Manual section 19.9 now documents ML real/Real64.real arguments and results
as C double on Arm64 and X64, mixed signatures, result boxing, the absence
of real-result region arguments in C, and the 64-bit-only scope. Its ML-to-C
type table lists all supported argument and result types and explains the
64-bit integral ABI slots used even for narrower ML types. The manual
and `kitdemo/mylib.mlb` now include `power_real_auto`, with an ordinary C
`double power_real_auto(double, long)` implementation and ML tests for its
result and zero exponent.

The manual compiled successfully with pdflatex. Its ML/C example was built
and executed on both Arm64 and native Linux X64 with GC enabled. A source-built
JavaScript compiler still rejects the unsupported foreign primitive with
`pToJs2.unimplemented: @m2_mixed`, rather than emitting a native FFI call.
The new `make check-real-auto-conversion` target runs shared IR and native
execution checks; the X64 `check-compiler` target includes it.

Final compiler builds used each checkout's configured installed MLKit seed
via `make mlkit`, on both Apple Silicon and the ThinkPad. On Linux, a
temporary writable copy of the seed's installed Basis/runtime and a fresh
cache avoided read-only installation directories and incompatible caches.
The original ThinkPad checkout and installed seed were preserved.

The final seed-built Arm64 MLKit/ReML compilers passed `make check-arm64`,
including scalar ABI generation, GC relocation/descriptors, native integration,
parallel execution, switches, shared IR, and the full real auto-conversion
matrix. Argobots was skipped because it was not configured. Native Linux
passed the shared matrix and the 56 existing regressions recorded above.
