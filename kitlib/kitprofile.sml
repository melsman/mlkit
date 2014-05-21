structure KitProfile : KIT_PROFILE =
  struct
    fun tellTime(s: string) : unit = prim("queueMark", s)
      
     (* queueMark is a no-op; queueMarkProf does stuff *)
      
  end