structure RegionProfile : REGION_PROFILE =
struct
  fun start () = prim ("mlkit_rp_start", ())
  fun pause () = prim ("mlkit_rp_pause", ())
  fun sample () = prim ("mlkit_rp_sample", ())
  fun mark s = prim ("mlkit_rp_mark", s)
  fun flush () = prim ("mlkit_rp_flush", ())
end
