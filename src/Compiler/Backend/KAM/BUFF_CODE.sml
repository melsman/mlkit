(* To buffer bytecode during emission *)
(* Taken from the Moscow ML compiler  *)

signature BUFF_CODE =
  sig
    val out_position : int ref
    val init_out_code : unit -> unit

    val out_i : int -> unit
    val out_long_i32 : Int32.int -> unit
    val out_short_i : int -> unit
    val out_long_i : int -> unit 

    val out_w : Word.word -> unit
    val out_short_w : Word.word -> unit
    val out_long_w : Word.word -> unit

    val out_long_w32 : Word32.word -> unit

    val out_real : real -> unit

    type key = int * string (*Label key; the string is the base (i.e., the program unit) *)

    val dump_buffer : {filename : string,
		       main_lab_opt : key option,
		       map_import_code : (int * key) list,
		       map_import_data : (int * key) list,
		       map_export_code : (key * int) list,
		       map_export_data : (key * int) list} -> unit
  end
