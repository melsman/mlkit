structure TV32 = Test(structure I = Int32
                      structure V = Int32Vector
                      structure A = Int32Array
                      structure VS = Int32VectorSlice
                      structure AS = Int32ArraySlice)

structure TV63 = Test(structure I = Int63
                      structure V = Int63Vector
                      structure A = Int63Array
                      structure VS = Int63VectorSlice
                      structure AS = Int63ArraySlice)

structure TV64 = Test(structure I = Int64
                      structure V = Int64Vector
                      structure A = Int64Array
                      structure VS = Int64VectorSlice
                      structure AS = Int64ArraySlice)

val () = pr_n := false

structure TV = Test(structure I = Int
                    structure V = IntVector
                    structure A = IntArray
                    structure VS = IntVectorSlice
                    structure AS = IntArraySlice)

val () = report ()
