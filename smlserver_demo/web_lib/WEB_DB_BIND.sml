signature WEB_DB_BIND = 
  sig
    type sql = string
    structure Handle : WEB_DB_HANDLE
    val dml : sql -> Handle.db -> string list -> unit
    val fold : sql -> Handle.db -> string list -> ((string -> string) * 'a -> 'a) -> 'a -> 'a
  end
