
(* Applies the functors in wordtable-functors.sml *)

(*
structure Word8Vector : MONO_VECTOR =
  let structure V = WordTable(TableArgWord8(type table = string))
  in struct open V
	    val update = updatev
     end
  end

structure Word8Array : MONO_ARRAY =
  WordTable(TableArgWord8(type table = chararray))
*)

(*---------------------------------------------*)
(*     Word31 Vectors, Arrays, and Slices      *)
(*---------------------------------------------*)

local
  structure Arg = TableArgWord31(type table = string)
in
  (** SigDoc *)
  structure Word31Vector : MONO_VECTOR =
    let structure V = WordTable(Arg)
    in struct open V
              val update = updatev
       end
    end

  (** SigDoc *)
  structure Word31VectorSlice : MONO_VECTOR_SLICE =
    WordSlice(Arg)
end

local
  structure Arg = TableArgWord31(type table = chararray)
in
  (** SigDoc *)
  structure Word31Array : MONO_ARRAY =
    WordTable(Arg)

  (** SigDoc *)
  structure Word31ArraySlice : MONO_ARRAY_SLICE =
    WordSlice(Arg)
end

(*---------------------------------------------*)
(*     Word32 Vectors, Arrays, and Slices      *)
(*---------------------------------------------*)

local
  structure Arg = TableArgWord32(type table = string)
in
  (** SigDoc *)
  structure Word32Vector : MONO_VECTOR =
    let structure V = WordTable(Arg)
    in struct open V
              val update = updatev
       end
    end

  (** SigDoc *)
  structure Word32VectorSlice : MONO_VECTOR_SLICE =
    WordSlice(Arg)
end

local
  structure Arg = TableArgWord32(type table = chararray)
in
  (** SigDoc *)
  structure Word32Array : MONO_ARRAY =
    WordTable(Arg)

  (** SigDoc *)
  structure Word32ArraySlice : MONO_ARRAY_SLICE =
    WordSlice(Arg)
end

(*---------------------------------------------*)
(*     Word64 Vectors, Arrays, and Slices      *)
(*---------------------------------------------*)

local
  structure Arg = TableArgWord64(type table = string)
in
  (** SigDoc *)
  structure Word64Vector : MONO_VECTOR =
    let structure V = WordTable(Arg)
    in struct open V
              val update = updatev
       end
    end

  (** SigDoc *)
  structure Word64VectorSlice : MONO_VECTOR_SLICE =
    WordSlice(Arg)
end

local
  structure Arg = TableArgWord64(type table = chararray)
in
  (** SigDoc *)
  structure Word64Array : MONO_ARRAY =
    WordTable(Arg)

  (** SigDoc *)
  structure Word64ArraySlice : MONO_ARRAY_SLICE =
    WordSlice(Arg)
end

(*---------------------------------------------*)
(*     Word Vectors, Arrays, and Slices      *)
(*---------------------------------------------*)

local
  structure Arg = TableArgWord(type table = string)
in
  (** SigDoc *)
  structure WordVector : MONO_VECTOR =
    let structure V = WordTable(Arg)
    in struct open V
              val update = updatev
       end
    end

  (** SigDoc *)
  structure WordVectorSlice : MONO_VECTOR_SLICE =
    WordSlice(Arg)
end

local
  structure Arg = TableArgWord(type table = chararray)
in
  (** SigDoc *)
  structure WordArray : MONO_ARRAY =
    WordTable(Arg)

  (** SigDoc *)
  structure WordArraySlice : MONO_ARRAY_SLICE =
    WordSlice(Arg)
end
