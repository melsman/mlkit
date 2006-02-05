signature MLB_FILESYS =
  sig
    val fromFile : string -> string
    val change_dir : string -> {cd_old : unit -> unit, file : string}
    val getCurrentDir : unit -> string
  end

