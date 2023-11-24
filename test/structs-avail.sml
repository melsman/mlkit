(* Required structures - see https://smlfamily.github.io/Basis/overview.html *)

structure X = Array : ARRAY
structure X = ArraySlice : ARRAY_SLICE
structure X = BinIO : BIN_IO
structure X = BinPrimIO : PRIM_IO
structure X = Bool : BOOL
structure X = Byte : BYTE
structure X = Char : CHAR
structure X = CharArray : MONO_ARRAY
structure X = CharArraySlice : MONO_ARRAY_SLICE
structure X = CharVector : MONO_VECTOR
structure X = CharVectorSlice : MONO_VECTOR_SLICE
structure X = CommandLine : COMMAND_LINE
structure X = Date : DATE
structure X = General : GENERAL
structure X = IEEEReal : IEEE_REAL
structure X = Int : INTEGER
structure X = IO : IO
structure X = LargeInt : INTEGER
structure X = LargeReal : REAL
structure X = LargeWord : WORD
structure X = List : LIST
structure X = ListPair : LIST_PAIR
structure X = Math : MATH
structure X = Option : OPTION
structure X = OS : OS
structure X = Position : INTEGER
structure X = Real : REAL
structure X = StringCvt : STRING_CVT
structure X = String : STRING
structure X = Substring : SUBSTRING
structure X = TextIO : TEXT_IO
structure X = TextPrimIO : PRIM_IO
structure X = Text : TEXT
structure X = Timer : TIMER
structure X = Time : TIME
structure X = VectorSlice : VECTOR_SLICE
structure X = Vector : VECTOR
structure X = Word : WORD
structure X = Word8 : WORD
structure X = Word8Array : MONO_ARRAY
(*structure X = Word8Array2 : MONO_ARRAY2 *)
structure X = Word8ArraySlice : MONO_ARRAY_SLICE
structure X = Word8Vector : MONO_VECTOR
structure X = Word8VectorSlice : MONO_VECTOR_SLICE

(* Optional structures - see https://smlfamily.github.io/Basis/overview.html *)

structure X = Array2 : ARRAY2
structure X = BoolArray : MONO_ARRAY
structure X = BoolArray2 : MONO_ARRAY2
structure X = BoolArraySlice : MONO_ARRAY_SLICE
structure X = BoolVector : MONO_VECTOR
structure X = BoolVectorSlice : MONO_VECTOR_SLICE
(*structure X = CharArray2 : MONO_ARRAY2*)
structure X = FixedInt : INTEGER
(*structure X = GenericSock : GENERIC_SOCK*)
structure X = INetSock : INET_SOCK

structure X = IntArray : MONO_ARRAY
structure X = IntArray2 : MONO_ARRAY2
structure X = IntArraySlice : MONO_ARRAY_SLICE
structure X = IntVector : MONO_VECTOR
structure X = IntVectorSlice : MONO_VECTOR_SLICE

structure X = Int31 : INTEGER
structure X = Int31Array : MONO_ARRAY
structure X = Int31Array2 : MONO_ARRAY2
structure X = Int31ArraySlice : MONO_ARRAY_SLICE
structure X = Int31Vector : MONO_VECTOR
structure X = Int31VectorSlice : MONO_VECTOR_SLICE

structure X = Int32 : INTEGER
structure X = Int32Array : MONO_ARRAY
structure X = Int32Array2 : MONO_ARRAY2
structure X = Int32ArraySlice : MONO_ARRAY_SLICE
structure X = Int32Vector : MONO_VECTOR
structure X = Int32VectorSlice : MONO_VECTOR_SLICE

structure X = Int63 : INTEGER
structure X = Int63Array : MONO_ARRAY
structure X = Int63Array2 : MONO_ARRAY2
structure X = Int63ArraySlice : MONO_ARRAY_SLICE
structure X = Int63Vector : MONO_VECTOR
structure X = Int63VectorSlice : MONO_VECTOR_SLICE

structure X = Int64 : INTEGER
structure X = Int64Array : MONO_ARRAY
structure X = Int64Array2 : MONO_ARRAY2
structure X = Int64ArraySlice : MONO_ARRAY_SLICE
structure X = Int64Vector : MONO_VECTOR
structure X = Int64VectorSlice : MONO_VECTOR_SLICE

structure X = IntInf : INT_INF

structure X = LargeReal : REAL
structure X = LargeRealArray : MONO_ARRAY
structure X = LargeRealArray2 : MONO_ARRAY2
structure X = LargeRealArraySlice : MONO_ARRAY_SLICE
structure X = LargeRealVector : MONO_VECTOR
structure X = LargeRealVectorSlice : MONO_VECTOR_SLICE

structure X = NetHostDB : NET_HOST_DB
(*
structure X = NetProtDB : NET_PROT_DB
structure X = NetServDB : NET_SERV_DB
structure X = PackWord<N>Big : PACK_WORD
structure X = PackWord<N>Little : PACK_WORD
*)
structure X = PackWord32Big : PACK_WORD
structure X = PackWord32Little : PACK_WORD

structure X = PackRealBig : PACK_REAL
structure X = PackRealLittle : PACK_REAL
structure X = PackReal64Big : PACK_REAL
structure X = PackReal64Little : PACK_REAL

structure X = Posix : POSIX

structure X = RealArray : MONO_ARRAY
structure X = RealArray2 : MONO_ARRAY2
structure X = RealArraySlice : MONO_ARRAY_SLICE
structure X = RealVector : MONO_VECTOR
structure X = RealVectorSlice : MONO_VECTOR_SLICE

structure X = Real64 : REAL
structure X = Real64Array : MONO_ARRAY
structure X = Real64Array2 : MONO_ARRAY2
structure X = Real64ArraySlice : MONO_ARRAY_SLICE
structure X = Real64Vector : MONO_VECTOR
structure X = Real64VectorSlice : MONO_VECTOR_SLICE

structure X = Socket : SOCKET
structure X = SysWord : WORD
(*structure X = UnixSock : UNIX_SOCK*)
structure X = Unix : UNIX

(*
structure X = WideCharArray : MONO_ARRAY
structure X = WideCharArray2 : MONO_ARRAY2
structure X = WideCharArraySlice : MONO_ARRAY_SLICE
structure X = WideChar : CHAR
structure X = WideCharVector : MONO_VECTOR
structure X = WideCharVectorSlice : MONO_VECTOR_SLICE
structure X = WideString : STRING
structure X = WideSubstring : SUBSTRING
structure X = WideTextPrimIO : PRIM_IO
structure X = WideText : TEXT
structure X = Windows : WINDOWS
*)

structure X = Word16 : WORD
structure X = Word16Array : MONO_ARRAY
structure X = Word16Array2 : MONO_ARRAY2
structure X = Word16ArraySlice : MONO_ARRAY_SLICE
structure X = Word16Vector : MONO_VECTOR
structure X = Word16VectorSlice : MONO_VECTOR_SLICE

structure X = Word31 : WORD
structure X = Word31Array : MONO_ARRAY
structure X = Word31Array2 : MONO_ARRAY2
structure X = Word31ArraySlice : MONO_ARRAY_SLICE
structure X = Word31Vector : MONO_VECTOR
structure X = Word31VectorSlice : MONO_VECTOR_SLICE

structure X = Word32 : WORD
structure X = Word32Array : MONO_ARRAY
structure X = Word32Array2 : MONO_ARRAY2
structure X = Word32ArraySlice : MONO_ARRAY_SLICE
structure X = Word32Vector : MONO_VECTOR
structure X = Word32VectorSlice : MONO_VECTOR_SLICE

structure X = Word63 : WORD
structure X = Word63Array : MONO_ARRAY
structure X = Word63Array2 : MONO_ARRAY2
structure X = Word63ArraySlice : MONO_ARRAY_SLICE
structure X = Word63Vector : MONO_VECTOR
structure X = Word63VectorSlice : MONO_VECTOR_SLICE

structure X = Word64 : WORD
structure X = Word64Array : MONO_ARRAY
structure X = Word64Array2 : MONO_ARRAY2
structure X = Word64ArraySlice : MONO_ARRAY_SLICE
structure X = Word64Vector : MONO_VECTOR
structure X = Word64VectorSlice : MONO_VECTOR_SLICE

val () = print "All ok\n"
