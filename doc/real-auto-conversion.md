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

- [ ] **M1: Define the conversion contract and lowering point.** Specify
  real arguments/results and mixed signatures; trace the existing
  `__real_to_f64` and `__f64_to_real` primitives and identify where to insert
  conversions. Record the typing, evaluation-order, and allocation invariants.
- [ ] **M2: Implement typed lowering through the shared compiler.** Insert
  conversions around auto-converted calls, preserve `float64` through closure
  conversion, and extend the foreign-type mapping. Audit downstream consumers.
  Verify IR dumps show unboxed call arguments/results and correct result boxing.
- [ ] **M3: Implement Arm64 call generation.** Reuse the scalar ABI machinery
  for floating-point arguments and results. Cover mixed integer/floating-point
  signatures, register exhaustion, stack arguments, and live values across
  calls. Verify the floating-point registers and stack layout in generated code.
- [ ] **M4: Add Arm64 end-to-end regression coverage.** Test real arguments,
  real results, mixed signatures, stack-passed arguments, negative zero,
  infinities, and NaNs. Exercise GC and profiling configurations, including
  allocation after a real result. Add focused assembly checks for direct
  `double` passage where runtime tests cannot establish the representation.
- [ ] **M5: Support the X64 backend.** Extend X64 auto-converted calls to use
  the shared `float64` lowering and the target's floating-point C ABI. Cover
  mixed signatures, register exhaustion, stack alignment, result handling,
  and live values across calls. Run corresponding runtime and assembly
  regressions, including GC and profiling configurations, on X64.
- [ ] **M6: Document and validate the completed implementation.** Update manual
  section 19.9 and add an ML/C example. Run the relevant Arm64 and X64 suites
  and check clear diagnostics on unsupported backends. Record validation in
  the PR and mark it ready for review when all implementation milestones pass.

This document is a plan; real auto-conversion is not implemented by this commit.
