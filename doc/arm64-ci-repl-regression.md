# ARM64 CI REPL failure and missing diagnostics

Investigation of [run 35795467303](https://github.com/melsman/mlkit/actions/runs/35795467303),
which tested `c898ef8`, before the simple/mpuz/uf improvements in `77bea07`.

The four X64 jobs passed. The ARM job built its native compilers/tools and passed
native, parallel, dev, plain, GC, generational-GC, profiling, GC-profiling,
parallel-allocation and explicit-region checks. It stopped in the full-Basis
REPL suite at `simple-ri.out`; both no-Basis REPL tests had passed. The subsequent
artifact upload also failed, so the original detailed REPL output is unavailable.

## Reproduced REPL defect

Loading a full Basis library in the ARM REPL reproduces a runtime crash. The
macOS crash report identifies a `SIGBUS`/protection failure when generated
initialization code writes to the address of `mlkit_arm64_allocate_preserving`
in the runtime library's executable text segment.

The private allocation helper takes the region pointer in x16 and the allocation
size in x17. A direct `bl` to this helper in a dynamically loaded REPL library
is routed through a Mach-O symbol stub:

```asm
adrp x16, helper_got_page
ldr  x16, [x16, helper_got_offset]
br   x16
```

This destroys the region argument before the helper reads it. Static linking
usually resolves the call directly, explaining why the native executable tests
passed while the full-Basis REPL exposed the problem.

Allocation, untagged allocation and reset helper calls now load the GOT address
into x30 and use `blr x30`, bypassing the linker stub and preserving both private
argument registers. The caller's original return address is already saved in
its frame. Inline allocation and reset fast paths are unchanged.

## Lost diagnostics

The filesystem suite intentionally creates `testcycl -> testcycl`. The artifact
uploader followed this link while expanding recursive globs and failed with
`ELOOP`. A staging script now copies regular diagnostic files without following
symlinks; upload reads only the staged directory.

The REPL Makefile previously piped compiler output straight through `grep`,
which could mask the compiler's exit status and discarded intermediate output.
It now checks the compiler status before filtering and retains raw stdout,
stderr and filtered output. The chdir fixture intentionally reports a parse error
at EOF and has an explicit expected nonzero exit status; other fixtures expect 0.
These files are included in the staged diagnostics.

The local reproduction establishes a concrete defect in the failing REPL path.
Because CI failed to upload its detailed diagnostics, the original run's exact
crash cannot be independently confirmed from its console log alone.

## Validation

- Full-Basis REPL: eight tests passed with `-no_gc` and eight with `-gc`,
  using both the X64-hosted ARM emitter and a freshly built ARM-native host.
- The native MLKit/ReML suite passed, including GC/generational GC, profiling,
  callbacks, allocation, exceptions and frame/ABI checks.
- Log staging tested with both a self-referential symlink and a directory cycle.
- A synthetic failing compiler verifies that the REPL Makefile reports failure
  and retains stderr instead of hiding it in the output-filter pipeline.
