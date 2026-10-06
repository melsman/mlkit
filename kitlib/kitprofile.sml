structure KitProfile : KIT_PROFILE =
  struct
    fun tellTime (s: string) : unit = prim ("mlkit_rp_mark", s)
  end
