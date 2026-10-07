# Archived ARM64 performance experiments

These measurements, scripts, and reports record the compiler revisions named in
each experiment. Their original commands are preserved so the measurements
remain reproducible with those revisions.

Current MLKit executables require runtime options in `+RTS ... -RTS` blocks.
For example, replace `./program -report_gc` with
`./program +RTS -report_gc -RTS` when rerunning a workload with a current compiler.
Application arguments go outside the block. See
[runtime arguments](../runtime-arguments.md) for the complete delimiter rules.
