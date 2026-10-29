structure TV32 = Test(structure W = Word32
                      structure V = Word32Vector
                      structure A = Word32Array
                      structure VS = Word32VectorSlice
                      structure AS = Word32ArraySlice)

structure TV63 = Test(structure W = Word63
                      structure V = Word63Vector
                      structure A = Word63Array
                      structure VS = Word63VectorSlice
                      structure AS = Word63ArraySlice)

structure TV64 = Test(structure W = Word64
                      structure V = Word64Vector
                      structure A = Word64Array
                      structure VS = Word64VectorSlice
                      structure AS = Word64ArraySlice)

val () = pr_n := false

structure TV = Test(structure W = Word
                    structure V = WordVector
                    structure A = WordArray
                    structure VS = WordVectorSlice
                    structure AS = WordArraySlice)
