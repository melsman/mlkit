# Garbage collection in the X64 REPL

The X64 REPL now supports `-gc` (the default), `-gengc`, and
`-gc -tag_pairs`. It no longer switches garbage collection off at startup.
Rebuild the Basis library and other cached compilation units with the updated
compiler: GC units now export an additional per-unit root table.

## Implementation

* `StaticGC.c` owns the image bounds and global-root registry shared with ARM64.
  ARM64 retains its existing registration entry points for generated code and
  delegates them to the shared implementation. Its stack walker is unchanged.
* X64 GC compilation units emit a root table containing the addresses of their
  exported value slots. A REPL linker wrapper registers each newly loaded unit's
  bounds and roots before running its initialization code.
* The X64 collector visits registered global slots as well as its existing
  executable root table. The latter may be absent in a REPL runtime. This keeps
  values from earlier phrases alive and updates their slots after evacuation.
* Ordinary X64 executables keep their existing static-data bounds checks. A
  dynamic-image lookup is needed only when additional images are registered.
  The REPL runtime also initializes its own bounds for primitive exceptions.
* REPL entries already carry the end-of-stack frame-descriptor sentinel. Exported
  C callbacks, including pretty-printing hooks, temporarily defer collection
  because their entry frames have no ML descriptors. They save and restore the
  caller's GC policy, leaving pending collection requests for later ML safe points.

The runtime keeps loaded REPL libraries resident, so their registrations remain
valid for the session. As with ARM64, unloading a library would require removing
its registration before its code or root slots become invalid.

## Regression test

`test/repl/check-gc-images.sh` requests collection explicitly and checks that the
collection counter advances. It exercises references updated across phrases,
retained closures, allocated strings, static strings, exceptions, pretty-printing,
and an exported callback that allocates while collection is deferred. It checks
that collection resumes after the callback returns.

For example, with an updated X64 compiler and runtime on Apple Silicon:

```sh
SML_LIB=/path/to/mlkit MLKIT=/path/to/updated/mlkit \
  CC='gcc -arch x86_64' test/repl/check-gc-images.sh
```

`GC_FLAGS=-gengc` or `GC_FLAGS='-gc -tag_pairs'` selects other variants.
`GC_CACHE` can reuse a Basis cache already rebuilt with this compiler; otherwise
an isolated cache suffix is selected. Test outputs are retained in the printed
temporary directory. `make -C test/repl test_gc` runs GC and genGC checks and is
included in the X64 CI jobs.

## Local validation

On Apple Silicon, with X64 programs running under Rosetta 2:

* Built the updated X64 compiler using MLKit with `-gc`.
* Rebuilt all ten runtime variants for both X64 and ARM64.
* Passed all eight existing REPL checks in each of `-gc`, `-gengc`,
  `-gc -tag_pairs`, and `-no_gc` modes (32 checks), using fresh Basis caches.
* Passed all 181 ordinary-program tests with `-gc` and all 181 with `-gengc`.
* Passed the forced-collection image/callback test in all three X64 GC modes.
  The callback test includes nested C-to-ML entries and verifies both deferred
  collection and restoration of the caller's GC setting.
* Passed the same forced-collection test on ARM64 with `-gc`, using its rebuilt
  runtime and an existing native compiler.
* Passed the register/stack/global relocation and image-boundary metadata test
  compiled for both architectures.

Linux execution and hosted CI results remain to be checked by CI.


## Installed-distribution regression

[The Ubuntu MLKit job](https://github.com/melsman/mlkit/actions/runs/35834098068/job/107093304925?pr=224)
passed its source-tree tests but failed its installed-distribution REPL smoke test.
`mlkit_basislibs` compiled `repl.mlb` only without GC; the GC variant compiled only
`basis.mlb`. With GC now enabled in the REPL, the installed compiler tried to
compile the missing `repl.sml` cache in the root-owned library directory. The
resulting `Failed to write dependencies ... repl.sml.d` error prevented loading
the Basis. The subsequent arithmetic type error came from missing infix
information, not from generated arithmetic code or the collector.

The build now precompiles `repl.mlb` with GC, which also prepares `basis.mlb`.
The installed-distribution CI check runs from a fresh temporary directory and
requires correct arithmetic, `List.tabulate`, and list pretty-printing. It rejects
Basis-loading diagnostics even if the REPL subsequently exits successfully.

Local verification reproduced both the dependency-write error and the arithmetic
type error with a read-only staged X64 installation. Adding the GC REPL cache
made the same check pass. A second check passed after copying the prepared
installation to a new read-only location and temporarily hiding the original,
verifying that the cache is relocatable. Hosted Linux validation awaits CI.
