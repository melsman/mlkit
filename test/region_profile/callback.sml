fun sample () : unit = prim ("mlkit_rp_sample", ())
fun callback (n:int) : int = (sample (); n)
val _ = _export ("rp_test_callback", callback)
val n : int = prim ("rp_call_callback", ())
