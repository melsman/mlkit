signature TEST_REPORT =
  sig
    
    val reset : unit -> unit
    val export : {errors:int, testfile_string:string, kitexe:string} -> unit

    val add_compout_line : {name: string, match: bool option,     (* NONE if match test not requested *) 
			    success_as_expected: bool} -> unit

    val add_comptime_line : {name: string, entries: (string *
			     Time.time) list} -> unit

    val add_runtime_line : {name: string, ok: bool, exesize: string,
			    exesize_stripped: string, size: int, 
			    rss: int, stk: int, data: int, exe: int, 
			    real: Time.time, user: Time.time, sys: Time.time} -> unit

    val add_runtime_bare_line : string * bool -> unit   (* `name' and `ok' *)

    val add_profile_line : string * bool -> unit   (* `name' and `ok' *)

    val add_gc_line : string * bool -> unit   (* `name' and `ok' *)

    val add_gengc_line : string * bool -> unit   (* `name' and `ok' *)

    val add_gc_profile_line : string * bool -> unit   (* `name' and `ok' *)

    val add_gengc_profile_line : string * bool -> unit   (* `name' and `ok' *)

    val add_tags_line : string * bool -> unit (* `name' and `ok' *)

    val add_tags_profile_line : string * bool -> unit (* `name' and `ok' *)

    val add_log_line : string -> unit
  end
