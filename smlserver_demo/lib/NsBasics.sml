structure NsBasics =
  struct
    type status = int     (* see nsthread.h *)
    val OK = 0 and ERROR = ~1 and END_DATA = 4
    fun ppStatus 0 = "OK"
      | ppStatus ~1 = "ERROR"
      | ppStatus 4 = "END_DATA"
      | ppStatus s = "Un supported status code: " ^ Int.toString s
  end
