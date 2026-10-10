# T4: C origins and GC state

The macOS ARM64 profiler publishes attribution at ML/C boundaries rather than
at every ML function entry. Profiled single-thread code saves the previous
attribution word in its C-call workspace and publishes the address of its C-call
instruction. On normal return it restores the previous word, preserving the
C result registers. Nested C calls therefore restore their enclosing origins.

The export bridge saves the enclosing token, publishes ML/runtime state before
calling the ML closure, and restores the token on return. Samples in callbacks
use the ML PC actually executing. A callback's own foreign call publishes its
own initiating call site, rather than inheriting the enclosing C origin. GC
remains deferred across export bridges under the existing native GC policy.

Exception transfer publishes ML/runtime state before unwinding to an ML handler.
C-call save slots abandoned by nonlocal transfer do not need to be read. A
callback with a locally caught exception still returns through its bridge and
restores the enclosing C token. For profiled GC code, the handler's saved aligned SP also carries the prior
GC-deferral policy in its unused low bit. Unwinding restores that policy and
strips the bit before restoring SP. Thus a callback-local catch retains GC
deferral, while a C exception caught outside the C extent permits later GC.

The collector saves attribution after its optional pre-GC snapshot, publishes
GC state during collection, then restores it before post-GC snapshots and
pending exception delivery. GC is its own category; no initiating-function
charge is attached to GC samples.

## Atomic transitions and interpretation

`mlkit_tp_context` is one aligned atomic native word. Its low two bits encode
state; the remaining bits encode an aligned foreign-call PC. Generated code
uses aligned native loads/stores, which are indivisible on the supported ARM64
platform. C collector hooks use relaxed atomics. No separately published origin
and state can be mixed by an interrupted transition. No asynchronous stack
walking or metadata lookup is introduced.

Time-session version 2 uses the existing sample fields:

| State | Meaning | Origin PC |
| --- | --- | --- |
| 0 | ML or unclassified runtime; resolve raw PC | zero |
| 1 | Recorder drain; retain as its own category | zero |
| 2 | Foreign-call extent | initiating ML call instruction |
| 3 | GC | zero |

Offline attribution gives a recognized ML raw PC priority over a foreign origin.
This also makes instructions around publication boundaries interpretable: a
sample still executing the ML call sequence belongs to that ML function. For a
non-ML PC in state 2, resolve the origin PC. An unresolved origin or raw PC remains
explicitly unknown. Recorder and GC states are separate categories. The T5
report will apply this interpretation to totals and range selection.

The reader continues to accept version-1 T3 recordings and validates version-2
states and origins. New profiled caches use `_CODE1_TP1` to rebuild library C-call
boundaries. A linked capability marker prevents old executables from silently
claiming the new recording semantics. Native unit metadata version 2 also
ensures all linked ML libraries have the boundary instrumentation; mixing a
version-1 library into a time-sampled executable is rejected. Parallel and REPL time sampling remain
unsupported.

## Regression checks

`test/time_profile/check-attribution.sh` uses both GC and non-GC builds. It
combines long C phases, nested exported callbacks, callback ML computation,
callback-origin C calls, C-raised exceptions caught inside and outside callbacks, and allocation-heavy
ML. C assertions check exact token restoration around each exported callback;
recording assertions resolve foreign origins, require ML callback samples, and
check a separate GC category. T3 lifecycle and backpressure checks remain in
`check-recording.sh`.
