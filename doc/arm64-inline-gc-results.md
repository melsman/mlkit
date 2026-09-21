# ARM64 inline GC descriptors

Milestone 12 replaces the dynamic return-PC index with descriptors immediately
before return addresses, using the same lookup model as X64. GC-enabled ML
calls materialize the continuation in x30 and transfer with `b`/`br`. Returns
still use `ret`, and the callee still saves its incoming x29/x30 pair. No-GC
calls retain `bl`/`blr`, and tail calls preserve the original LR.

The emitter puts eight-byte-aligned descriptors between the transfer and its
continuation. Exception continuations and compilation-unit entry sentinels use
the same layout. The collector reads the frame size, saved-return-slot offset,
and liveset directly relative to the saved PC. It no longer copies or sorts
frame tables or searches them for each frame. Image registration remains for
static-data bounds and global root cells, including loaded REPL images.

The GC cache prefix changes to `ARM64_FD2_`, and the runtime registration API
becomes `mlkit_arm64_register_static_image`. Rebuild the ARM runtime together
with the compiler; older GC objects are excluded from the new cache, and mixed
compiler/runtime registration ABIs fail at link time.

## Validation

MLKit `-gc` builds of MLKit, ReML, and the emitter harness pass. The native
integration suite passes ordinary and tail calls, closures, spills, large
frames, long branches, allocation/reset, foreign callbacks, and exception
unwinding. It forces collection through register, stack, global, and handler
roots with GC, generational GC, tagged pairs, and profiling. REPL loading and
exception recovery pass. Assembly checks confirm explicit x30 materialization
and the absence of return-PC indexes in the GC fixtures.

The portable root walker test also passes with AddressSanitizer and
UndefinedBehaviorSanitizer. It walks inline descriptors without a registered
frame table, checks relocation across multiple bitmap words, retains global
root/static-range registration tests, and rejects reserved-register roots.

Fresh Basis compilation exposed host stack exhaustion at `wordtables.sml` in
both the unchanged baseline and the candidate, with native and X64 hosts.
The comparison therefore uses X64-hosted MLKit compilers linked with a 1 GiB
stack (`-ldexe 'gcc -arch x86_64 -Wl,-ld_classic,-stack_size,0x40000000'`).
Both were built with MLKit `-gc`; MLton was not used. This is a compilation
workaround, not a change to benchmark execution or the backend's default stack
setting. No full bootstrap or broad regression/performance suite was repeated.

## Small runtime comparison

Baseline: `f9ffdb4`, including the earlier register-preservation and peephole
changes. Both versions rebuild the Basis from source in separate caches;
the baseline links the saved pre-change ARM runtime and the candidate links the
rebuilt runtime. Programs run natively on the same M2 Max machine. Each version
gets one warmup and three timed runs, alternating order; all 24 executions
match the expected output. Values below are median wall-clock seconds.

| Benchmark, with GC | Dynamic index | Inline descriptor | Time change |
| --- | ---: | ---: | ---: |
| nucleic | 1.651 | 1.687 | +2.2% |
| mlyacc | 0.421 | 0.499 | +18.4% |
| professor | 0.420 | 0.469 | +11.8% |

The simpler frame lookup does **not** produce an overall speedup in these
samples: mlyacc and professor regress, while nucleic changes relatively little.
Explicit continuation-address setup and the changed call/return prediction and
instruction-cache layout are possible costs; this small comparison does not
isolate them. The requested inline-descriptor design is retained. Instruction
scheduling remains deferred.

[Raw samples, executable hashes, and compiler commands](arm64-performance/inline-gc-results/measurements.json)
are retained with the report.
