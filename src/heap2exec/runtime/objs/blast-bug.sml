fun iter 0 = []
  | iter n = n :: (iter (n-1)); 

val blastWrite : 'a -> Word8Vector.vector =
      (fn x => Unsafe.CInterface.c_function "SMLNJ-RunT" "blastOut" x);

blastWrite (iter 100000);

