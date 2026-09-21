# ARM64 code-builder composition and Standard ML style

Milestone 10 applies the root `AGENTS.md` rules to the ARM64 backend and the
Standard ML code added or modified by this PR. Runtime performance comparison
is now milestone 11.

`CodeGenArm64.sml` defines a local, right-associative `++` operator with type:

```sml
(inst list -> inst list) * (inst list -> inst list) -> inst list -> inst list
```

Its implementation is function composition, `fun (f ++ g) x = f(g x)`, with
annotations restricting both operands to instruction-list builders. `one`
prepends an instruction or directive, and `instruction` builds a single machine
instruction. For example, a binary primitive now reads in execution order:

```sml
(readInto fsz a (X 16)
 ++ readInto fsz b (X 17)
 ++ instruction opn ["x16","x16","x17"]
 ++ writeInto fsz d (X 16)) code
```

Applying the composition builds the suffix first, preserving the existing
backwards construction. No instruction-list append is introduced. The rewrite
covers primitive arithmetic/conversions, calls, records, allocation/reset,
statements and switches, GC entry, shared runtime helpers, and REPL setup.

The style cleanup spaces `=>` and declaration `=`, aligns multiline
`let`/`in`/`end`, and puts `val` declarations on separate lines. Declarations
may start after `let`, and the expression may start after `in`, as documented
in `AGENTS.md`. Existing shared files are cleaned within the PR's changed
regions; the ARM64 files are checked throughout.

## Validation

The whitespace-only pass preserves the lexical token stream in all 59 SML
files touched by the PR, including the already-rewritten code generator. All
original code-generator comments are retained. A separate audit checks spacing,
multiline keyword alignment, and declaration separation.

Both old and new compilers run `nucleic` successfully. The two generated source
assembly files and the link assembly file are byte-for-byte identical, including
labels, instructions, and metadata.
The emitter harness also produces 26 byte-identical assembly files before and
after the rewrite, covering return areas, tail calls, nested scopes, scalar C
calls, long branches, and plain/GC/generational allocation/reset probes.

MLKit with `-gc` builds the ARM64 MLKit and ReML compilers, the X64 backend
compiler, the emitter harness, kittester, and the ABI harness. The standalone
ABI harness uses its own cache because its mock `Lvars` representation differs
from the production compiler's representation. Its layout checks pass. The X64
compiler also builds and runs a GC-enabled smoke program through Rosetta, using
explicit X64 assembler/linker options when hosted by the native ARM compiler.

Native MLKit/ReML integration passes, including forced collection,
generational GC, profiling, foreign callbacks, exceptions, spills, and REPL
recovery. Pthread and Argobots checks pass. The final compiler passes all 130
developer tests, and the final ReML compiler passes all 79 explicit-region tests.
All 181 GC regression tests also pass with the final compiler.
