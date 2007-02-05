signature MLB_FILESYS =
  sig
    eqtype unique
    val fromFile : string -> string
    val change_dir : string -> {cd_old : unit -> unit, file : string}
    val getCurrentDir : unit -> string
    val cmp : unique * unique -> order
    val unique : bool -> string -> unique
  end

(*

unique b file

get a unique id on the file file. If b then follow links instead of getting the
unique id of the link. The id is the pair dev_t * ino_t from the stat of the
file.

*)
