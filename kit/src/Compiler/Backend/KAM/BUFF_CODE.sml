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

    val dump_buffer : {filename : string,
		       main_lab_opt : int option,
		       map_import_code : (int * int) list,
		       map_import_data : (int * int) list,
		       map_export_code : (int * int) list,
		       map_export_data : (int * int) list} -> unit
  end
