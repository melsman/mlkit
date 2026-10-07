# Runtime arguments

MLKit executables use GHC-style `+RTS ... -RTS` blocks. All runtime options,
including profiling, garbage-collector controls, and runtime help, belong inside
such a block. Ordinary arguments outside a block are passed unchanged to
`CommandLine.arguments()`; the runtime does not consume application options.

```sh
mlkit -no_gc -rp -o app app.mlb
./app +RTS -rp -rp_region all -rp_interval 10ms -RTS input.txt
./app +RTS -help
./gc-app +RTS -report_gc -RTS input.txt
```

The compiler's `-rp` option remains a compiler option. A block on the compiler's
own command line configures the runtime executing the compiler, not the runtime
of the generated program. Interactive compiler options such as `-rp_file` still
configure the child REPL runtime; MLKit adds the child's runtime block itself.

| Marker | Meaning |
| --- | --- |
| `+RTS` | Begin runtime options. Removed from application arguments. |
| `-RTS` | End the block. Removed; another block may follow. |
| `--RTS` | Stop all runtime parsing. Removed; everything following is literal. |
| `--` | Stop all runtime parsing. Retained along with everything following. |

The closing `-RTS` can be omitted at the end of the command line. Blocks may
appear before, between, or after application arguments. Application argument
order, empty arguments, and arguments containing spaces are preserved.

```sh
./app before +RTS -rp -RTS after       # application receives before, after
./app -help                          # application receives -help
./app --RTS +RTS -help                # application receives +RTS, -help
./app -- +RTS -help                   # application receives --, +RTS, -help
```

Unknown or unavailable options inside a block are errors. A delimiter cannot
serve as the value of a runtime option. Profiling options require an executable
compiled with `-rp`; runtime help lists only options supported by its runtime.
Use `-RTS` or `--RTS`, rather than `--`, when the separator must not reach the
application.

This replaces implicit parsing of leading runtime flags. In old command lines,
wrap runtime options in a block, leaving application arguments outside it.
Historical benchmark artifacts under `doc/arm64-performance` retain the commands
used for their recorded compiler revisions; use blocks when rerunning workloads
with the current compiler. The old profiler's `-notimer`, `-microsec`, and rp2ps
interface are not restored by adding a block; see the current
[region profiler](region-profiler.md) and [site occupancy](allocation-profiler.md).

The delimiter semantics follow the
[GHC runtime-options convention](https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#setting-rts-options-on-the-command-line).
