(* To buffer bytecode during emission *)
(* Taken from the Moscow ML compiler  *)

signature BUFF_CODE =
  sig
    val out_position : int ref
    val init_out_code : unit -> unit

    val out_i : int -> unit
    val out_short_i : int -> unit
    val out_long_i : int -> unit 

    val out_w : Word.word -> unit
    val out_short_w : Word.word -> unit
    val out_long_w : Word.word -> unit

    val out_long_w32 : Word32.word -> unit

    val dump_buffer : string -> unit
  end
