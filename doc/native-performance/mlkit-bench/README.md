# Manual fifteen-program PR #253 assessment

See [the full assessment](../../mlkit-bench-pr253-results.md) for methodology, all fifteen results and limitations. These results supersede the earlier ten-program test-tree screen.

`arm64/` and `thinkpad/` each contain 90 sample rows: fifteen programs, two compiler variants, three executions. `run.log` records successful reference-output comparisons for all programs. Basis build logs, prepared-library reuse logs, hashes and sizes document the separate prepared libraries and executable identities. X64 Fibonacci assembly/address records support the code-placement caveat. All samples are retained. The reproduction script is manual only; no performance measurements run in CI.
