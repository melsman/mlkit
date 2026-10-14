structure TimeProfile : TIME_PROFILE =
struct
  fun start () = prim ("mlkit_tp_start", ())
  fun pause () = prim ("mlkit_tp_pause", ())
  fun flush () = prim ("mlkit_tp_flush", ())
end
