structure KitProfile : KIT_PROFILE =
  struct
    fun tellTime(s: string) : unit = prim("queueMark","queueMarkProf", s)
      
     (* queueMark is a no-op *)
      
  end