structure NsBasics =
  struct
    type status = int     (* see nsthread.h *)
    val OK = 0 and ERROR = ~1 and END_DATA = 4
  end
