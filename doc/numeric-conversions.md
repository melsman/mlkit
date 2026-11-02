# Numeric conversion optimization

Design for issue #264. This document describes the intended implementation;
the conversion optimization is not implemented yet.

## Scope and semantics

Preserve conversions through IntInf and LargeWord as semantic operations until
the Lambda optimizer can compose them. Cover Int31, Int32, Int63, Int64 and
Word8, Word31, Word32, Word63, Word64. Resolve default Int and Word according
to the target configuration. Existing API aliases share the same operations.
Direct treatment of Int8, Int16 and Word16 is outside this scope, as are
toLarge(fromLarge x) compositions.

A common conversion description records source and destination types, signed
or unsigned source interpretation, and checked or modulo conversion semantics.
Signed word interpretation (the X variants) must remain distinguishable from
unsigned interpretation. Mathematical identity does not imply representation
identity: tagging, boxing, extension and normalization must remain correct.

There are 126 forward composition cases through IntInf: 14 source operations
(four signed integers and two interpretations of five words) times nine
destination operations. Removing 14 same-type identities and ten duplicate
signed/unsigned word truncations leaves 102 distinct typed non-self-identity
results. Some of these can still lower to a typed identity. The 50 forward
LargeWord cases add no distinct result kinds. These are semantic matrix sizes,
not counts of new backend primitives.

## Optimization and lowering boundary

Compile Basis conversion wrappers to explicit semantic conversion primitives,
including fixed-width integer/word conversions to and from IntInf. Compose
operations after inlining, including opportunities exposed through let bindings.
Preserve evaluation order and Overflow behavior.

Run normal optimization with the imported optimizer environment and save the
exported environment before lowering. Exported function bodies retain semantic
conversions so later compilation units can optimize compositions across module
boundaries.

Lower surviving IntInf conversions to the existing IntInfRep implementation
functions. Extend the existing conservative IntInfRep dependency mechanism to
these conversions. Then optimize executable code internally without importing
or exporting an optimizer environment. Lowering is mandatory even when
optimization is disabled; additional optimization respects the selected mode.
No semantic IntInf conversion primitives may reach region inference/backends.

## Validation requirements

Exercise the complete forward composition matrix using runtime inputs, with
boundary values, signed X variants and Overflow behavior. Verify generated code
as well as results: eliminated intermediate IntInf conversions must not allocate
digit lists. Check cross-compilation-unit composition, surviving conversion
lowering, and optimization-disabled compilation. Validate native tagged and
untagged configurations and the JavaScript backend where supported.
