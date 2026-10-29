# Standard ML style

Follow these rules when adding or modifying Standard ML code in this repository:

- Always separate `=>` from surrounding code with spaces, for example
  `fn x => x + 1`.
- Always separate `=` in `val` and `fun` declarations with spaces, for example
  `val a = 5`.
- A `let ... in ... end` expression may occupy a single line. If it spans
  multiple lines, put `let`, `in`, and `end` at the start of separate lines,
  aligned in the same column. Indent declarations and the body within them.
  Declarations may occur aligned to the right of `let`. An expression may occur
  immediately after `in`. These two squeezing rules may save vertical space.
- Do not put multiple `val` declarations on the same line.

For example:

```sml
let val x = computeX ()
    val y = computeY x
in List.map (fn z => z + y) xs
end
```

# Test and utility scripts

Do not commit Python code, including embedded Python in shell scripts. Prefer
POSIX shell and standard command-line tools for test and benchmark orchestration.

# Documentation, tests, and experimental material

Keep committed material useful to someone maintaining the code a year from now.

- Document durable design decisions, safety assumptions, invariants, and public
  behavior. Put concise explanations near the relevant code or in existing
  documentation; avoid development diaries and overlapping reports.
- Keep focused tests for meaningful behavior and plausible regressions.
  Consolidate overlapping cases. Avoid tests of incidental source structure or
  symbol names unless those details enforce an explicit requirement.
- Put investigation history, alternatives, benchmark methodology and summaries,
  and implementation milestones in the pull request or issue. Do not retain
  completed milestone plans as permanent design documentation.
- Store raw experiment data, generated binaries, disassembly, timing samples,
  and build logs outside the source tree, in an accessible experiment archive
  when preservation is needed. Record a concise reproduction command and archive
  link in the pull request or issue. Do not remove the only copy of evidence
  that should be preserved.
- Keep manual benchmark tools only when they are likely to be reused. Do not add
  performance measurements to CI; use CI for correctness and selection checks.

Before adding a document, test, or generated artifact, ask whether it explains a
lasting decision or catches a plausible regression. Reproducibility does not
require committing the entire investigation.
