# UF without GC: redundant modulo-call setup

The `-no_gc` UF slowdown comes from redundant argument staging around
`__mod_word64ub`. Removing that setup in an isolated assembly experiment
reduces ARM execution time by 42%, from 0.1535 s to 0.0891 s, making it 18%
faster than X64/Rosetta 2's 0.1084 s. No production compiler changes were made.

## Why the previous fix does not cover this configuration

The [earlier UF fix](arm64-uf-dlx-fixes.md) recognizes the tagged-word helpers
`__mod_word63` and `__mod_word31`. With `-no_gc`, the default word
representation is untagged 64-bit, so UF instead calls `__mod_word64ub`.
That name still takes the general C-call path.

All four arguments are already in x0–x3, but ARM reserves 48 bytes, copies
those registers through x16 into four stack slots, reloads x0–x3, calls the
helper, and releases the stack area. The helper call plus setup takes 13
instructions; the diagnostic replaces the sequence with the single existing
`bl ___mod_word64ub`. X64 already calls the helper without that staging.
There is no GC-state save/restore overhead in this non-GC configuration.

UF performs two random-element selections per union and one per find:
`20 * (2 * 200000 + 500000) = 18000000` modulo calls. Removing 12 setup
instructions per call eliminates 216 million dynamically executed
instructions. The three static call sites all use the constant divisor
100000.

Unlike the tagged path before its fix, untagged multiplication is already
short: materialize 16807, then multiply directly into the destination
register. There is no multiplication spill/masking problem to fix here.

## Measurements

Apple M2 Max, macOS 26.5.1, September 23, 2026. Compilers, non-GC runtimes and
rebuilt Basis libraries are those of the [20-program non-GC comparison](arm64-nogc20-current.md).
UF units are recompiled with retained assembly; each experiment reassembles
only the ARM UF unit and links the same remaining objects. All programs use
`-no_gc`. No MLton is used.

One warmup and seven measured runs per variant, shuffled order, with all
builds completed before timing. Each run exits successfully and matches
expected stdout. Times are medians; raw samples include CPU time, per-process
peak RSS and executable hashes.

| Configuration | Elapsed (s) | Relative to X64 |
|---|---:|---:|
| X64 / Rosetta 2 | 0.1084 | 1.000 |
| Current ARM | 0.1535 | 1.415 |
| ARM: direct modulo call | 0.0891 | 0.821 |
| ARM: direct call plus remove redundant result staging | 0.0885 | 0.816 |
| ARM: inline remainder instead of direct call | 0.0891 | 0.822 |

A second seven-run batch confirms the main result: 0.1533 s current ARM,
0.0894 s direct-call ARM, and 0.1080 s X64. Removing redundant result staging
has only a small additional measured effect. Inlining the remainder gives
no material further gain over a direct helper call in this experiment.

The inline diagnostic uses `udiv` and `msub` and is valid only because this
benchmark's divisor is known to be nonzero. It is not a general replacement
for the helper's `Div` exception handling. The direct-call diagnostic retains
the runtime helper and its exception semantics.

## Recommended fix

Extend the existing audited-helper fast path to `__mod_word64ub` and its
32-bit counterpart, `__mod_word32ub`. Both implementations in `Math.c` are
nonallocating, do not call back, and transfer to the exception handler on a
zero divisor. Keep the existing staging fallback for arguments that are not
already placed, and preserve protection for arbitrary foreign calls.

This is a small extension of the existing optimization. The measurements do
not justify a special inline-remainder implementation for UF. Validation of
the eventual compiler change should include untagged 32/64-bit remainders,
zero divisors, wrapping words, and fallback argument placement.

[Raw measurements and diagnostic scripts](arm64-performance/uf-nogc-investigation/)
retain machine-local temporary paths and captured link commands. The generated
assembly and executable files remain temporary artifacts.
