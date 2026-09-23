# Passing code continuations in the ARM64 emitter

The changes proposed here are now implemented, including suffix passing through
the remaining primitive and runtime emitters. See the
[implementation results](arm64-continuation-results.md) for measurements and the
validation scope of each step.

Passing the remaining instruction list into each emitter is a good fit for
this backend. Use the same approach as X64: an explicit `inst list` argument,
with instructions prepended using `::`. It avoids copying completed instruction
lists without allocating a function closure for every instruction.

The original investigation below records the rationale for the change. The [compiler timing comparison](arm64-compiler-timings.md) motivates
it but does not attribute the observed slowdown to list concatenation alone.

## Existing precedent and proposed interface

`CodeGenX64.sml` already passes a code suffix `C` through `CG_lss`, `CG_ls`,
and its utility functions. For example, it emits jumps with `I.jmp ... :: C`
and passes `C` to `move_aty_to_aty`. The shared `CodeGenUtil.sml` also prepends
new static-data fragments to the accumulated data instead of copying the
accumulated data on every insertion.

For ARM64, change helpers from returning a fresh fragment to accepting the
code that follows it. For example, the equivalent of the current `move` is:

```sml
fun move (src, dst, code) =
  if src = dst then code
  else
    ins (case (src, dst) of
           (D _, _) => "fmov"
         | (_, D _) => "fmov"
         | _ => "mov") [r dst, r src] :: code
```

A sequence then has the form `read (..., write (..., code))`, rather than
`read ... @ write ...`. Each helper constructs its instructions directly
onto the supplied suffix. The migration must reach helpers such as `load`,
`store`, `constant`, and `stack`: wrapping the existing implementation as
`oldHelper args @ code` still constructs and copies its fragment.

There are two different meanings of continuation here:

- An instruction-list suffix, as above. This is the recommended representation.
- A function of type `inst list -> inst list` (a difference list). Composing
  such functions can also avoid copying, but may allocate closures and retain
  captured operands. It adds uncertainty about inlining, register pressure,
  and region allocation in this self-hosted compiler. There is no need to
  introduce a closure per instruction to obtain the benefit.

Neither changes the generated ML calling convention or the register-based
return-address design. The existing ARM64 function named `continuation bv`
creates an actual return-PC label and GC metadata; that is a separate concept.

## Where copying occurs

| Pattern in `CodeGenArm64.sml` | Cost and proposed treatment |
| --- | --- |
| `staticData := !staticData @ fragment` in `number`, `continuation`, `static`, worker entry, and export generation | Copies all preceding static data at every insertion. Accumulate chunks and flatten once. |
| `stmts = List.concat (map stmt ...)`, with nested `SCOPE`, `LETREGION`, handlers, and switches | Flattens each statement fragment and copies nested bodies again as enclosing statements are assembled. Pass the code suffix through recursive statement emission. |
| Helper chains in `internalCall`, `recordWithUntag`, `mlcall`, and `entryGC` | Construct temporary instruction lists and copy their spines during concatenation. Make the helpers accept a suffix and prepend directly. |
| Final concatenation in `top` and `CG` | Copies completed function/program lists. Assemble onto a suffix at these boundaries too. |

The static-data pattern is the clearest asymptotic problem. For `n` fragments
of `k` instructions each, appending to the accumulated prefix copies
`k * n * (n - 1) / 2` existing list cells. A chunk accumulator needs `n`
chunk-list cells and a single linear flattening of the `n * k` instructions.
It also handles the out-of-line worker/export code stored in `staticData`.
To preserve the current fragment order:

```sml
val staticChunks : A.inst list list ref = ref []

fun addStatic fragment =
  staticChunks := fragment :: !staticChunks

fun finishStatic () =
  List.concat (rev (!staticChunks))
```

Reset the accumulator at the existing reset points and materialize it only
when finishing a compilation unit or link-code result. This is a small,
independent change worth measuring before the broader emitter conversion.

Not every `@` is quadratic. SML's `@` is right-associative, so a flat
`a @ b @ c` copies `a` and `b`, not an ever-growing combined prefix.
`List.concat (map ...)` is likewise linear at one level. The additional
cost comes from repeated construction/copying across helper and nesting
boundaries; deeply nested bodies can be copied once per enclosing level.
Appending bounded argument lists or a small fixed prefix is a lower priority.

## Register usage and pressure

The timing reports show `RegAlloc` at 2.74 seconds on ARM64 versus 1.48
seconds on X64, and `FetchFlush` at 0.99 versus 0.65 seconds. These figures
show additional costs in those phases; they do not measure spill counts or
prove their cause. `NativeCompile.sml` runs allocation, fetch/flush insertion,
frame layout, and substitution before invoking `CodeGen.CG`.

The source also shows code-volume costs that amplify instruction-list work:

- `internalCall` preserves 24 integer and 30 floating-point registers: 54
  stores and 54 loads for each invocation, before argument staging and other
  call instructions. These saves are conservative because allocation helpers
  are invisible to register allocation; they are not evidence of 54 live
  values or 54 allocator spills.
- `mlcall`, `arguments`, and `results` stage values through temporary stack
  areas. This handles moves safely but emits memory traffic even when the
  allocated values fit in registers.
- The ARM64 register description retains conservative flushing across C calls
  (`is_callee_save_ccall` returns false). The allocator has 24 general-purpose
  registers and 28 allocatable floating-point registers, with two additional
  floating-point spill temporaries. A larger palette alone does not establish
  lower allocation cost or fewer spills.

Passing an instruction-list suffix should leave the emitted registers,
stack slots, and instruction sequence unchanged. Consequently it cannot
remove these saves or reduce target-program register pressure. It reduces
how often their instruction-list cells are allocated and copied. In a native
self-hosted compiler, allocating those extra cells is itself executed by ARM
code, so conservative allocation-helper preservation can compound that cost.

For the compiler implementation itself, an explicit suffix adds a live
argument but removes intermediate fragments. The balance must be measured;
it is not a guarantee of fewer host spills. A chain of function-valued
continuations could instead increase captured state and allocation, which is
another reason to follow the existing X64 convention.

## Evaluation order and correctness

A mechanical replacement of `a () @ b ()` with `a (b code)` changes the order
in which the emitters run. Instruction order can remain correct while label
allocation and other compiler side effects change order.

ARM64 emission mutates `staticData`, `frameIndex`, and `dataLabels`, allocates
fresh labels, and uses `currentArgs`/`currentResults` to generate frames and
tail calls. A safe conversion should:

1. Pass function context explicitly where needed, rather than letting deferred
   emitters read mutable state belonging to another function.
2. Allocate labels and register data/metadata deliberately, preserving their
   associations and the ordering requirements of data boundaries. If statement
   traversal becomes right-to-left, audit these effects rather than assuming
   that matching instruction order is sufficient.
3. Keep effectful preparation separate from pure suffix assembly where it
   makes ordering explicit. Avoid deferred closures that capture global state.
4. Compare generated assembly before and after, allowing only explained label
   renaming, and exercise GC return-PC maps, exceptions, tail calls, exports,
   worker entries, and REPL image registration.

## Recommended sequence

First replace accumulated static-data append with ordered chunks and compare
its isolated effect. Then convert the utility helpers and statement/control-
flow emitters to explicit code suffixes, following X64. Keep the register
palette, calling convention, and save/restore policy unchanged during this
experiment so any timing improvement can be attributed to list construction.

Use the existing timed programs, particularly `nucleic`, `kitsimple`, and
`kitmolgard`, to compare `CG` time. Include an input with many constants/call
sites and one with deeply nested control flow to exercise the two copying
patterns. The phase timings include compiler GC time despite the old HTML description
claiming otherwise; they do not provide a separate collector-time breakdown.

Reducing allocation-helper saves and argument/result staging is a separate
optimization requiring liveness and ABI correctness work. It should follow,
with its own measurements, rather than being mixed into the continuation
conversion. The subsequent implementation changes list construction; register-allocation
and save/restore policies remain unchanged.
