# Numeric conversion optimization

Numeric conversion wrappers retain semantic operations until the Lambda
optimizer has composed them. The native scalar model covers Int31, Int32,
Int63, Int64 and Word8, Word31, Word32, Word63, Word64; default Int and Word
resolve according to the target configuration. Modern and legacy Basis names
share implementations. Int8, Int16 and Word16 retain their existing wrappers;
there are no special rules for these types or for IntInf-to-finite-to-IntInf
round trips. SMLtoJs retains its separate Basis conversion implementations.

## Semantic representation

`NumericConversions` decodes string-named `CCALLprim` operations into source,
destination and signed/unsigned interpretation. An integer destination checks
its range; a word destination converts modulo its precision. X operations
interpret the source word's high bit as a sign bit. These distinctions must
survive composition even when physical representations coincide.

Primitive names retain logical precisions independently of their Lambda types.
In particular, CompileDec erases Word8 to the default word type, so its 8-bit
precision cannot be recovered from the primitive's type alone. Generated
primitive types describe physical representations, while their names describe
logical conversions. Representation lowering selects existing backend
operations, including their legacy aliases, or implements mixed cases with a
full-width bit carrier, extension, masks and range checks.

The forward matrix contains 126 IntInf compositions: fourteen source operations
(four signed integers and two interpretations of five words) times nine
destinations. Removing fourteen same-type identities and ten duplicate signed
and unsigned word truncations leaves 102 distinct typed non-self-identity maps.
Some maps still lower to a representation identity. Fifty LargeWord cases add
no result kinds; these counts do not describe new backend primitives.

## Let bindings and exported information

The contraction environment records conversion provenance only for an already
evaluated source variable. An outer conversion can compose with this fact
without duplicating or moving the original source expression. Independent
consumers simplify independently; ordinary dead-binding elimination removes an
intermediate only when it has no remaining uses and is safe to discard.
Provenance participates in scope removal, substitution, joins and serialization.
Conversions that can check or lose information are not treated as generally
invertible operations.

Normal contraction exports semantic function bodies. Flattening also completes
before lowering, because its exported metadata describes external calling
conventions. These semantic exports remain available to later compilation units.

## Executable lowering

After normal optimization, surviving finite-to-IntInf operations construct the
existing signed, little-endian base-2^30 representation directly. At most three
digits suffice for a native scalar, including the magnitude of the minimum
signed integer. This bounded construction exposes ordinary constructors and
branches to subsequent local optimization. Surviving IntInf-to-finite operations
call the existing IntInfRep implementations. The compiler's conservative
IntInfRep dependency keeps these functions available; internal Basis scopes
must also make the module available when imported semantic bodies require it.

The post-lowering rounds import no contraction environment, export no optimizer
information and cannot create semantic conversion primitives. Lowering remains
mandatory with optimization disabled. No semantic conversion may reach region
inference or a backend. Typed identities involving boxed values are not used:
sharing their regions requires more than equality of their machine layouts.

## Validation

`test/numeric-conversions.sml` exercises the forward matrix with runtime boundary
inputs and independent mathematical range/modulo expectations. The companion
`test/numeric-conversions/check.sh` runs tagged, untagged and optimization-disabled
configurations, checks cross-unit imports, shared intermediates, evaluation and
exception ordering, fallback conversions, and the optimized selection unit.
It preserves generated artifacts in a temporary directory printed at startup.
