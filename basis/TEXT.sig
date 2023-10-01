(** Text structures related to a shared character type.

The TEXT signature is matched by the required Text structure,
which implements strings based on the extended ASCII 8-bit characters.
*)

signature TEXT =
  sig
    structure Char : CHAR
    structure String : STRING
    structure Substring : SUBSTRING
    structure CharVector : MONO_VECTOR
    structure CharArray : MONO_ARRAY
    structure CharVectorSlice : MONO_VECTOR_SLICE
    structure CharArraySlice : MONO_ARRAY_SLICE
    sharing type Char.char = String.char = Substring.char
      = CharVector.elem = CharArray.elem = CharVectorSlice.elem
      = CharArraySlice.elem
    sharing type Char.string = String.string = Substring.string
      = CharVector.vector = CharArray.vector
      = CharVectorSlice.vector = CharArraySlice.vector
    sharing type CharArray.array = CharArraySlice.array
    sharing type CharVectorSlice.slice
      = CharArraySlice.vector_slice
  end

(**

[structure Char] The underlyng Char structure.

[structure String] The String structure associated with the Char
structure.

[structure Substring] The Substring structure associated with the Char
structure.

[structure CharVector] The CharVector structure associated with the
Char structure.

[structure CharArray] The CharArray structure associated with the
Char structure.

[structure CharVectorSlice] The CharVectorSlice structure associated
with the Char structure.

[structure CharArraySlice] The CharArraySlice structure associated
with the Char structure.

*)
