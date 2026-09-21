# Initial MLKit/ReML ARM64 ABI

This specifies the first macOS arm64 backend. It is a compiler/runtime
contract, not a claim that ARM instruction generation already exists.
`FrameLayout.sml` supplies target-dependent frame sizes to CallConv,
RegAlloc, and CalcOffset. `Arm64/AbiArm64.sml` records the register contract
and implements scalar C argument/result placement independently of emission.
The X64 parameters retain its existing layout and allocation palette.

## Register roles

| Registers | ML convention |
| --- | --- |
| x0-x7 | ML arguments, in closure/value/region order |
| x0-x2 | ML results; excess results use stack slots |
| x0-x15, x19-x26 | Initial general-purpose allocator palette |
| x16, x17 | Instruction, address, and transfer temporaries; also linker scratch |
| x18 | Platform-reserved; never allocate, use as a temporary, or restore |
| x27 | Exception value during the raise bridge |
| x28 | Pointer to the runtime `context` |
| x29 | Frame pointer to a saved-FP/saved-return-PC pair |
| x30 | Link/continuation register |
| sp | Hardware stack pointer, always 16-byte aligned |
| d0-d7 | Unboxed floating-point arguments |
| d0-d27 | Initial floating-point allocator palette |
| d28, d29 | Floating-point spill/move scratch |
| d30, d31 | Floating-point instruction temporaries |

The ML convention is internal: it does not promise to preserve the C
callee-saved value registers across ML calls. Initially, live ranges crossing
ML calls are flushed; preserve the existing allocator's conservative policy.
C bridges must preserve x19-x29 and the low 64 bits of d8-d15 as required by
AAPCS64. C-facing entry stubs save the incoming values before installing x28
or using those registers for ML. Restore them on normal return. x16/x17 may
be destroyed by call/branch veneers and cannot carry persistent state.

As in X64, closure arguments consume the first integer argument register,
then ordinary values, then region arguments. The independent FP bank takes
unboxed doubles. Overflow arguments keep the shared CallConv ordering;
closure/ordinary arguments are roots when live, region pointers and unboxed
floating-point values are not. Result registers follow result-list order.

## ML frames and control transfer

All logical offsets in CallConv/CalcOffset are 64-bit words. Low to high
addresses in a complete activation are:

1. Local slots, including any alignment padding.
2. Spilled incoming arguments in the existing descending-stack order.
3. Saved caller x29.
4. Saved return PC.
5. Spilled result slots.

The two header words occupy the position formerly occupied by the single
X64 return-address word. Within the header, saved FP is word 0 and return PC
is word 1. `FrameLayout.returnOffsetFromTop` measures the return-PC offset
from the low-address end of that header. If F is the local-word count and A
is the spilled-argument count, the return-PC offset from the frame base is
F + A + 1. Frame size is F + A + 2 + R, where R is spilled-result count.
`alignFrame` pads F so this total is a multiple of two words.

`resolve_act_cc` offsets describe the existing argument/result reservation
order, not direct byte offsets from the hardware SP. The ARM emitter must
translate these logical offsets consistently with formal argument offsets.
Reserve complete aligned blocks and store slots at offsets: do not implement
an eight-byte abstract push by temporarily misaligning hardware SP.

Calls explicitly materialize their continuation address and install the
header before transfer. x29 points at the header once the activation is
established. Normal return reloads the saved FP and continuation, releases
the local/argument/header area as specified by the caller/callee convention,
and makes spilled results available to the caller. No implicit x86-style
hardware push is assumed. The emitter must retain any caller-side alignment
padding in its accounting until results have been fetched.

Continuations used by the collector have metadata immediately before their
labels. ARM can explicitly form the continuation address, branch over the
metadata into the callee, and return to the continuation label. It must not
let an ordinary BL return address point into descriptor data. Large distances
require the emitter's general address materialization, not an assumption
that every continuation is in ADR range.

A tail transfer restores the enclosing FP and preserves the original return
PC, relocates overlapping arguments safely, and installs the target's
compatible activation. Start with the existing conservative tail-call
eligibility; use ordinary call/return when stack layouts cannot be reused.
Do not silently drop tail calls needed for bounded-space loops: add coverage
when the ARM emitter implements this path.

## Exceptions and regions

Keep `context.topregion`, `context.exnptr`, and `context.uncaught_exnname`
at byte offsets 0, 8, and 16. Region representations and tagging remain
those validated by Runtime/Layout.c; ReML does not require a separate ABI.

An ARM handler record has six words, at increasing addresses:
continuation PC, handler closure, previous handler pointer, saved SP,
saved FP, and a reserved zero word. The first four preserve the existing
handler contract; the extra FP and padding make restoration explicit and
keep the record 16-byte sized. The closure slot is the only potential heap
root in this record; the other fields must not enter the heap root bitmap.

The public raise bridge receives context and exception via the C ABI,
installs x28, and protects the exception in x27 while deallocating intervening
regions. It then restores the previous handler, SP and FP, places the handler
closure/exception in x0/x1, and transfers to the handler with the recorded
continuation. There must be no GC safepoint between protecting the exception
in x27 and delivering it to the handler; deallocation must not introduce one.
Crossing a foreign callback boundary by a nonlocal raise is not implicitly
supported: wrappers must retain the existing exported-call exception policy
and restore the foreign frame/preserved registers before returning to C.

## GC contract

This section describes the required ARM format. The current C collector
still consumes the X64 format; wire the new format only with the ARM GC stub
and validate both together in the runtime-integration milestone.

Use a 32-slot integer save block and an eight-slot floating save block. Root
mask bit i names xi and maps to save word 31-i, matching the existing reversed
integer-save convention. Slot 0 contains the original ML SP. The x18 slot is
zero; the stub must not restore it. Save d7 through d0 in increasing addresses
after the integer block. Only live value argument registers are marked at
entry safepoints; context, SP/FP, return addresses, regions, reserved registers,
and FP values are excluded. Other live values must already have stack homes.

Follow the save blocks with four words: spilled region/FP argument count,
spilled result count, total spilled argument count, and a pointer to the
spilled-argument area. This 44-word snapshot is 16-byte sized. The explicit
argument-area pointer keeps any bridge/alignment padding outside the root
scan. Use the C ABI to call the collector with context, snapshot pointer,
and register mask. The collector updates roots in the saved slots; the
bridge reloads those updated values before continuing ML execution.

Frame metadata retains the existing reverse-emission convention: immediately
before a continuation are function number, return-PC offset, frame size,
and then bitmap words going toward lower addresses. Bitmap words contain
32 meaningful bits, stored in 64-bit slots. For bit k, the corresponding word
is `frameBase + 8*(frameWords-1-k)`. Both saved FP and return PC have zero bits.
The return-PC offset is the formula above; the frame size includes both
header words and all alignment padding. Terminate at the existing sentinel
frame-size value. Tests must cover roots around each header, spilled results,
multiple bitmap words, and restoration of relocated register roots.

## C calls and callbacks

`AbiArm64.arguments` returns argument locations, promoted types, required
narrow-integer extension, and the total aligned outgoing stack area. Apply
the same layout in reverse for incoming callback arguments. Integer/pointer
results use x0, scalar FP results use v0, and void has no result location.

Named scalar integer/pointer arguments use x0-x7; named FP arguments use
v0-v7 independently. Overflow arguments use naturally aligned stack slots
of their actual size, so adjacent chars can occupy consecutive bytes.
For register arguments the caller sign/zero-extends values narrower than
32 bits; stack stores retain their natural width. The outgoing stack
allocation is rounded to 16 bytes.

Unnamed arguments are default-promoted (narrow integers to int, float to
double) and placed in eight-byte stack slots even when argument registers
remain unused. They follow any named stack arguments, aligned to eight.
The current MLKit automatic FFI describes integer/boolean/pointer-like
values; the layout helper additionally models scalar floats for runtime
bridges. It does not introduce new source-level FFI syntax or silently map
aggregates, vectors, int128, or arbitrary C++ types to scalar registers.

Emission still needs ML tagging/unboxing, C-width conversion, parallel moves,
stack placement, indirect/direct calls, callback preservation, and result
boxing/tagging. The scalar layout helper is not a working C-call emitter.
Keep the milestone's C-boundary implementation checkbox open until calls and
callbacks execute through the ARM backend.

## Validation and next integration points

Run `sh src/Compiler/Backend/tests/check-abi.sh`. The runner uses `mlkit`
from PATH; set `MLKIT=/path/to/mlkit` to select another MLKit build. It tests the production
CallConv module with X64 and ARM frame parameters, scalar C placement and
promotions, and register exclusions. On Apple Silicon it also calls small
assembly probes from C and a C callback from assembly to independently check
Apple's packed-stack, variadic, register-extension, and frame-record rules.
The probes validate the ABI assumptions, not generated ML code.

The refactored MLKit and ReML compiler sources typecheck with MLton.
An X64 MLKit build of the refactored compiler passes all 130 `test_dev`
checks (65 without GC and 65 with generational GC). A fresh MLton build
of the final source also passes all 130 checks from a clean test directory
and eight profiling cases: `int_first`, `exn1`, `f64_1`, and `fib`, each
with no-GC profiling and generational-GC profiling. This is X64 regression
coverage, not ARM execution or a native bootstrap fixed-point check.

A bootstrap limitation remains: using the installed X64 MLKit to build
this compiler with `-no_gc` produces a compiler that crashes when compiling
`int_first.sml` with `--no_basislib -no_gc -prof`. A clean build of the
pre-refactor source at `0a85302` reproduces the same crash. Do not count
successful source compilation alone as a validated bootstrap.

The ARM instruction module must expose these register roles through
REGISTER_INFO (including the explicit FP allocator palette) and select the
ARM FrameLayout. The emitter and collector must implement the contract above
as a coordinated change. X64 remains wired to FrameLayout.x64 throughout.

References: [Apple ARM64 ABI](https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms)
and [AAPCS64](https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst).
The ML register assignment, handler record, and GC snapshot are MLKit design
decisions, not requirements imposed by Apple's C ABI.
