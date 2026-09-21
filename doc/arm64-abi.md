# Initial MLKit/ReML ARM64 ABI

This specifies the first macOS arm64 backend. It is a compiler/runtime
contract. Implemented coverage and remaining language limits are described in
[arm64-compiler.md](arm64-compiler.md).
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
| x30 (LR) | Incoming return address; restored before `ret` or a tail transfer |
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

ARM ML calls pass the return address in x30 (LR): direct calls use `bl`,
indirect calls use `blr`, and normal returns use `ret`. The caller does not
write a return PC into the callee's frame. The callee owns preservation of
its incoming LR before any instruction or call that would overwrite it,
including C/runtime calls and GC slow paths. Initially every ML function
saves incoming x29/LR and establishes x29 before its first safepoint.

`FrameLayout.returnDelivery` separates entry delivery from saved storage:
X64 uses `StackHeader`, ARM uses `LinkRegister 30`. The existing ARM
`headerWords=2` describes the conservative saved frame, not how the incoming
return address is passed. The emitter implements direct and indirect calls, stack arguments/results,
and callee-owned FP/LR saves; the layout helper itself emits no instructions.

All logical offsets in CallConv/CalcOffset are 64-bit words. Low to high
addresses in a complete activation are:

1. Local slots, including any alignment padding.
2. Spilled incoming arguments in the existing descending-stack order.
3. Argument padding: one word when the spilled-argument count is odd.
4. Saved caller x29.
5. Saved return PC.
6. Spilled results in descending result-list order.
7. Result padding: one word when the spilled-result count is odd.

The two header words occupy the position formerly occupied by the single
X64 return-address word. Within the header, saved FP is word 0 and return PC
is word 1. `FrameLayout.returnOffsetFromTop` measures the return-PC offset
from the low-address end of that header. If F is the local-word count and A
is the spilled-argument count, the return-PC offset from the frame base is
F + even(A) + 1, where even(n) rounds n up to an even number.
Frame size is F + even(A) + 2 + even(R), where R is the spilled-result
count. `alignFrame` rounds F up independently to an even number. Separate
argument/result padding keeps SP aligned after the callee releases its
argument/header area and leaves only the result block for its caller.

`resolve_act_cc` offsets describe the existing argument/result reservation
order, not direct byte offsets from the hardware SP. The ARM emitter must
translate these logical offsets consistently with formal argument offsets.
Reserve complete aligned blocks and store slots at offsets: do not implement
an eight-byte abstract push by temporarily misaligning hardware SP.

For the initial layout, the caller reserves the complete aligned outgoing
area, including space for the two header words, but leaves that header
uninitialized. The callee stores its incoming x29 and LR into those slots
and sets x29 to the pair. Reserving storage does not transfer the return
address through memory. No collection or stack walk may observe a partially
initialized activation. Prologue and epilogue unwind information must
account for the transition between LR and its saved slot.

Normal return restores x29 and LR, releases the local/argument/header area
according to the existing logical slot convention, and executes `ret`.
Spilled results remain available to the caller, which stages register and
stack results before writing their destinations and releasing even(R) words.
Tail transfers stage operands below the active frame, reuse the original
result block, and reposition the argument/header block for the target. This
supports changing argument counts without accumulating stack space.
The ARM emitter must not copy X64's implicit hardware return-address push.

### Return PCs and frame descriptors

The ARM collector looks up descriptors through a return-PC-to-descriptor
index. Each collecting ML call site has an entry keyed by the exact address
immediately following its `bl`/`blr`. That address contains executable
continuation code, never inline descriptor data. Descriptor payloads reside
out of line. The index also covers runtime/GC continuations used for stack
walking and explicit entry/exit sentinels.

The emitter and linker must produce relocatable code/descriptor references;
the runtime must register each image's index before executing its ML code,
including dynamically loaded REPL code. A missing entry while walking an
ML frame is an error, not an implicit end of stack. Foreign entry bridges
for shared images provide an explicit boundary descriptor. Index lookup and
registration are implemented; X64 keeps its existing PC-relative metadata.

This allows ordinary `bl`/`blr` without manually constructing a continuation
address. Linker veneers may change x16/x17; they must not change which return
PC the caller's index entry describes.

### Tail calls and leaf functions

A tail transfer restores the enclosing FP and original incoming LR from
its saved frame (or retains LR when it was never saved), relocates overlapping
arguments safely, and tears down the current activation. Transfer with `b`
or `br`, not `bl`/`blr`: the target must inherit the original caller's return
address. The target establishes its own frame. Retain the existing tail-call
eligibility and bounded-space guarantees; argument/result layouts must agree.

The initial emitter conservatively saves FP/LR in every ML function. A later
leaf optimization may omit the save/restore and leave LR live throughout a
function that makes no calls, has no GC safepoint or allocating slow path,
installs no exception handler, and meets platform unwind requirements.
A source-level leaf that calls a runtime helper is not eligible. A leaf
returns directly with `ret`; x29 continues to describe its caller's frame.

Initially an optimized leaf still retains the reserved logical header space
and established argument/result offsets. Its unsaved slots must never be
scanned. Removing that space requires a separate compatible layout decision
for direct and indirect calls; it is not implied by passing LR in a register.
Functions requiring collection or nonlocal exception restoration retain a
materialized frame until those paths explicitly support a register-held LR.

## Exceptions and regions

Keep `context.topregion`, `context.exnptr`, and `context.uncaught_exnname`
at byte offsets 0, 8, and 16. Region representations and tagging remain
those validated by Runtime/Layout.c; ReML does not require a separate ABI.

An ARM handler record has six words, at increasing addresses:
continuation PC, handler closure, previous handler pointer, saved SP,
saved FP, and the saved `context.topregion` pointer. The first four preserve
the existing handler contract; the FP and region-chain snapshot make
restoration explicit and keep the record 16-byte sized. The closure slot is the only potential heap
root in this record; the other fields must not enter the heap root bitmap.

The public raise bridge receives context and exception via the C ABI,
installs x28, and protects the exception in x27 while deallocating intervening
regions until `context.topregion` equals the saved chain snapshot. It then
restores the previous handler, SP and FP, places the handler
closure/exception in x0/x1, installs the recorded continuation in LR,
reserves the handler's two-word FP/LR header, and branches to the handler
without overwriting LR. There must be no GC safepoint between protecting the exception in x27 and delivering it to the
handler; deallocation must not introduce one.
Crossing a foreign callback boundary by a nonlocal raise is not implicitly
supported: wrappers must retain the existing exported-call exception policy
and restore the foreign frame/preserved registers before returning to C.

## GC contract

The C collector selects this ARM format under `DARWIN_NATIVE`. X64 continues
to consume its original format. The entry check and collector are validated
together by the runtime-integration tests.

Use a 32-slot integer save block and an eight-slot floating save block. Root
mask bit i names xi and maps to save word 31-i, matching the existing reversed
integer-save convention. Slot 0 contains the original ML SP. The x18 slot is
zero; the entry check must not restore it. Save d7 through d0 in increasing addresses
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

The implemented entry check is inline: the ML prologue saves incoming LR
before collection and before allocating locals. The snapshot's argument-area
pointer refers to the incoming call block. The walker relocates register and
spilled argument roots, then starts the caller walk from that block's saved
incoming LR. The snapshot's x30 is restored before execution continues; both
LR copies and saved FP are non-roots. There is no separate assembly GC stub
continuation to confuse with the incoming ML return address.

The ARM return-PC index maps to an out-of-line descriptor anchor. Relative
to that anchor, function number is at word -1, return-PC offset at -2, frame
size at -3, and bitmap words at -4 and below. This preserves the existing
payload ordering without interpreting instructions before LR as metadata.
Bitmap words contain 32 meaningful bits, stored in 64-bit slots. For bit k, the corresponding word
is `frameBase + 8*(frameWords-1-k)`. Both saved FP and return PC have zero bits.
The return-PC offset is the formula above; the frame size includes both
header words and all alignment padding. Terminate at the existing sentinel
frame-size value. Tests must cover roots around each header, spilled results,
multiple bitmap words, and restoration of relocated register roots.
Emitter/runtime acceptance tests must also cover nested direct/indirect ML
calls, LR survival across C and GC calls, tail recursion with bounded stack
use, leaf `ret`, exception-handler entry, and descriptor lookup in linked and
dynamically loaded images. The native integration suite covers these paths.
`Arm64GC.c` registers each image's return-PC table, static-data range, and
addresses of global root cells before executing that image. The table is copied
and sorted for lookup; the image owns its descriptors and root cells. The REPL
retains loaded images. Any future unloading path must unregister the table
before `dlclose`; an unregister operation is provided and tested. X64 keeps its
existing stack walk and contiguous static-data classification.

## C calls and callbacks

`AbiArm64.arguments` returns argument locations, promoted types, required
narrow-integer extension, and the total aligned outgoing stack area. The general emitter must apply
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

`CodeGenUtilArm64.scalarCall` now implements staging, scalar placement,
narrow-integer extension, packed stack stores, and variadic promotion. Raw and
automatic source FFI calls use its word-sized path, with ML conversion in
`CodeGenArm64`. Automatic boxed integer results preserve their destination
across C. Returning bridges preserve x19–x30 and d8–d15; x18 remains reserved.
The exported source interface remains `int -> int`, using ML integer
representation as on X64.

C-call IR currently has no root descriptors. Foreign calls and exported hooks
therefore defer collection and restore the previous GC policy on return.
Allocations can request a pending GC, which runs at the next ordinary ML entry.
Exceptions must be caught within callbacks instead of escaping C frames.
The REPL's returning bridge supplies a sentinel descriptor and registers loaded
images, so ordinary ML execution within a shared image can collect normally.

## Validation and next integration points

Run `sh src/Compiler/Backend/tests/check-abi.sh`. The runner uses `mlkit`
from PATH; set `MLKIT=/path/to/mlkit` to select another MLKit build. It tests the production
CallConv module with X64 and ARM frame parameters, scalar C placement and
promotions, and register exclusions. On Apple Silicon it also calls small
assembly probes from C and a C callback from assembly to independently check
Apple's packed-stack, variadic, register-extension, and frame-record rules.
The probes validate the ABI assumptions, not generated ML code.

Both compiler entry points build with MLKit through `Makefile.arm64`. The
GC-enabled X64 compiler built with MLKit passes all 130 default `test_dev`
checks (65 without GC and 65 with generational GC). Native ARM64 execution
checks are described in [arm64-compiler.md](arm64-compiler.md). These results
establish the tested ARM GC paths, but not a native bootstrap fixed point.

A bootstrap limitation remains: using the installed X64 MLKit to build
this compiler with `-no_gc` produces a compiler that crashes when compiling
`int_first.sml` with `--no_basislib -no_gc -prof`. A clean build of the
pre-refactor source at `0a85302` reproduces the same crash. Do not count
successful source compilation alone as a validated bootstrap. Building the
X64 compiler with `-gc` instead of `-no_gc` was subsequently verified to
compile and run this profiling reproduction successfully. Tracked in
[issue #225](https://github.com/melsman/mlkit/issues/225).

The ARM instruction module must expose these register roles through
REGISTER_INFO (including the explicit FP allocator palette) and select the
ARM FrameLayout. The emitter and collector implement the contract above
as a coordinated change. X64 remains wired to FrameLayout.x64 throughout.

References: [Apple ARM64 ABI](https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms)
and [AAPCS64](https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst).
The ML register assignment, handler record, and GC snapshot are MLKit design
decisions, not requirements imposed by Apple's C ABI.

## Parallel worker boundaries

With `-par`, a C-ABI worker entry receives `ThreadInfo *` in x0. It calls
`thread_init`, sets x28 to the context at byte offset 8, and reads the closure
from offset 0. It invokes the untagged ML closure with x0 = closure and
x1 = unit, reserving the normal 16-byte FP/LR header. The result in x0 goes
to the nonreturning `thread_exit`. Both pthreads and Argobots use this bridge.
Runtime layout assertions protect the offsets used here.

Parallel exported callbacks recover x28 from `thread_info`, so a closure
registered by another thread uses the calling runtime worker's region and exception
chains. Exceptions must still be handled before returning across a C boundary.
ML's `Thread` wrapper catches worker exceptions and transports them as results.

Dynamic exception names increment the shared counter with `ldaxr`/`stlxr`.
The exclusive-store status uses w30 only after the incoming LR has been saved
in the ML frame. Protected allocation goes through the C allocator's acquire,
compare/exchange, and release operations; `-par0` explicitly bypasses protection.
Parallel GC/tagging/profiling combinations are unsupported by the existing
runtime and rejected by the ARM driver.
