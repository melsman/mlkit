# T1: macOS ARM64 interrupted-PC feasibility

Issue [#241](https://github.com/melsman/mlkit/issues/241), experiment on
2026-10-09. Host: macOS 26.5.1 (25F80), Darwin 25.5.0 ARM64 T6020;
Apple clang 21.0.0; MLKit checkout `5bffdc30`.

## Decision

**Go for interrupted-PC capture and a single-thread wall-clock prototype.**
**No-go for presenting `ITIMER_VIRTUAL` or `ITIMER_PROF` as calibrated CPU-time
sampling on this host.** Delivery depends substantially on workload, even with
an empty buffer and unblocked signal. Keep CPU mode experimental until an
alternative or a defensible accounting model is validated. Do not substitute
wall-time samples while labeling them CPU time. T2 can proceed independently;
CPU sampling requires further investigation before product integration.

Wall mode needs shared ownership of `SIGALRM`/`ITIMER_REAL` with the existing
region profiler, or a separately validated timer mechanism. It must not replace
the existing timer. A shared timer handler would request region sampling on
its own schedule and capture time samples on theirs; a time sample must not
perform a region traversal. This is a future integration design, not implemented
by this experiment.

## Reproduce

From the repository root:

```sh
sh test/time_profile/run-feasibility.sh
```

Requires macOS ARM64, a built `bin/mlkit`, SDK headers, `cc`, `ar`, `nm`, and `rg`.
`MLKIT`, `CC`, `SML_LIB`, and `OUT` can override defaults. The script compiles
fresh sources in a temporary directory and uses `-no_par -gc`. It prints the
artifact directory and retains summaries, raw timestamp/PC/image/symbol rows,
GC reports, symbol tables, build logs, and profiler-conflict logs there.
`TP_MODE=wall|user|cpu` and `TP_INTERVAL_US` configure individual runs.

The standalone test covers a long C computation, `nanosleep`, pipe `read`
blocked until a helper process writes, deliberate signal masking, and a
`getppid` syscall-heavy loop. The helper is a separate process; the sampled
parent remains single-threaded and its CPU accounting excludes the child.
The ML executable covers a tail-recursive arithmetic loop, allocation with a
retained 200,000-element list to force meaningful GC scanning, a long C call,
and sleep. Each process runs one phase, then stops sampling before output.

## Measurements

[t1-results.txt](t1-results.txt) retains a complete run, with requested intervals
of 100 us, 1 ms, and 10 ms. These are observations, not resolution guarantees.
The approximately 300 ms standalone phases showed:

- Wall 100 us: about 3,000 deliveries for computation, sleep, and pipe read.
  At 1 ms there were about 300; at 10 ms about 30. Timestamp gaps include
  jitter and occasionally closely spaced deliveries. A requested interval
  is not a strict minimum spacing between handler entries.
- CPU/user timers: much lower and workload-dependent delivery rates in
  computation. At 1 ms the long C call received 177 user and 191 CPU samples
  over about 300 ms. The syscall loop received about 290 CPU samples, but
  **zero user-timer samples**, despite about 170 ms reported user CPU time.
  At 100 us, the CPU computation delivered 687 samples against approximately
  300 ms of CPU consumption, while the syscall loop delivered roughly 2,900.
  These discrepancies prohibit estimating CPU duration as count times interval.
- CPU/user timers produced no samples during sleep or pipe waiting at 1 ms
  and 10 ms. Wall samples continued. Handler cost itself contributes CPU time
  and can affect the timer; none of these runs subtract that cost.
- Masking the signal for roughly 300 ms yielded only one or two samples on
  unmasking, not the elapsed number of expirations. Standard timer signals
  coalesce. The experiment does not claim to count missing expirations.
- Every recorded PC was nonzero; timestamps never moved backwards; every
  interrupted SP fell in the main thread's stack bounds. This establishes
  routing for the tested single-thread process, not arbitrary worker threads.
- Raw rows resolve to generated `F.*` code during ML computation, `tp_busy`
  during the foreign call, and `gc`, `evacuate`, and `allocGen` during allocation.
  [t1-gc.txt](t1-gc.txt) confirms 45 collections and about 61 ms GC in the
  allocation phase. Resolution uses `dladdr` after disarming, only as an
  experimental sanity check; it is not the T2 offline metadata format.

Counts and gaps vary between runs. Both initial and subsequent runs showed the
same qualitative CPU-clock problems. This is feasibility evidence, not the
repeated overhead/accuracy study required by T6.

## Handler and timestamp audit

`SA_SIGINFO` supplies Darwin `ucontext_t`. The SDK
`__darwin_arm_thread_state64_get_pc` and SP accessor extract the interrupted
registers, rather than the handler's own address. Accessor macros also accommodate
the SDK's pointer-authentication representation. No stack walking is performed.

The handler calls only `mach_absolute_time` and the Darwin errno accessor,
saves/restores errno, and stores PC, raw ticks, and a stack-bound routing check
in a fixed 65,536-record buffer. Always-lock-free integer atomics are required
at compile time. Records are published after their fields with a release store.
The sampled signal is masked during the handler and during consumption; there
is no concurrent drain or wraparound. Overflow increments a saturating counter.
This protocol is deliberately limited to one writer and stop-then-read, not a
production streaming or parallel buffer (T3/T7).

Timestamp stubs are warmed before arming; timebase conversion, stack-bound
queries, symbol lookup, allocation, and output happen outside the handler.
Inspection with `otool -tv` confirms the handler has no allocation, lock, or I/O
calls and emits inline atomic instructions. The installed libsystem kernel's
`mach_absolute_time` reads a hardware counter and commpage offset, retrying if
the offset changes, with a kernel-trap fallback. It does not acquire a userspace
lock or allocate. Apple's [ARM64 implementation](https://github.com/apple-oss-distributions/xnu/blob/main/libsyscall/wrappers/mach_absolute_time.s)
agrees with this audit. This is a target-specific implementation audit, not a
portable POSIX async-signal-safety promise for the Mach API.

Apple documents [mach_absolute_time](https://developer.apple.com/documentation/kernel/1462446-mach_absolute_time)
as monotonic ticks excluding system sleep. The handler stores ticks unchanged;
conversion uses `mach_timebase_info` obtained outside signal context. A process
sleeping via `nanosleep` is different from the machine entering system sleep.
Aligning this coordinate with the region profiler's `CLOCK_MONOTONIC` origin is
still integration work; this experiment uses its own origin.

## Semantics and restrictions

The installed Darwin `setitimer(2)` manual specifies real time for `ITIMER_REAL`,
process execution/user time for `ITIMER_VIRTUAL`, and user plus system time for
`ITIMER_PROF`. Their signals are SIGALRM, SIGVTALRM, and SIGPROF respectively.
The manual's historical typical 10 ms resolution is not an observed lower bound
for wall timers here. CPU timer delivery should not be equated with fine-grained
CPU accounting merely because the API accepts microseconds.

Wall signals interrupt waiting syscalls: samples may contain a libsystem
syscall-return PC, not a runnable ML function. These are observations of waiting
locations, not reconstruction of all blocked time. Restarting `read`, `waitpid`,
and `nanosleep` after EINTR is explicit in the harness; signal effects on other
runtime I/O need auditing before integration. Delayed delivery can describe the
code executing on resumption. A timestamp records handler arrival, not the
unavailable exact expiration instant.

The actual region-profiler test rejects wall sampling when its timer is already
armed and verifies CPU sampling can coexist with region snapshots. CPU timers
have separate signal slots, but this does not fix their measurement limitations.
No signal handler is replaced when ownership is detected. Shutdown blocks the
signal and disarms the timer, leaving any pending delivery blocked until exit.
The harness is intentionally one-shot and does not restore/restart timers.

Unknown image/symbol rows stay explicit. Build UUIDs, stable function ranges,
ASLR resolution, C-origin attribution, GC categories, callbacks, and exception
state are T2/T4 work. No parallel, REPL, dynamic-image, alternative macOS timer,
or other-platform support is established by T1.
