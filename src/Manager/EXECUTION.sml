
(* The signature EXECUTION describes the argument to the KitCompiler functor which
 * builds the Manager modules... 
 *)

signature EXECUTION =
  sig
    structure CompileBasis: COMPILE_BASIS    
    type CompileBasis = CompileBasis.CompileBasis
    type CEnv = CompilerEnv.CEnv

    type strdec = PostElabTopdecGrammar.strdec
    type strexp = PostElabTopdecGrammar.strexp
    type funid = FunId.funid
    type strid = StrId.strid
    type Env = CompilerEnv.ElabEnv
    type lab
    type target 
    type linkinfo 

    val pr_lab : lab -> string
    val code_label_of_linkinfo : linkinfo -> lab
    val imports_of_linkinfo : linkinfo -> lab list * lab list
    val exports_of_linkinfo : linkinfo -> lab list * lab list
    val unsafe_linkinfo : linkinfo -> bool

    (* Hook to be run before any compilation *)
    val preHook : unit -> unit
	
    (* Hook to be run after all compilations (for one compilation unit) *)
    val postHook : {unitname:string} -> unit

    datatype res = CodeRes of CEnv * CompileBasis * target * linkinfo
                 | CEnvOnlyRes of CEnv

    val compile : 
	('a * ('a -> funid -> strid * Env * strexp * CEnv * 'a))
	-> CEnv * CompileBasis * strdec list * string -> res

    val generate_link_code : (lab list * (lab list * lab list) -> target) option
    val emit: {target: target, filename:string} -> string   (* returns the filename for the .o file
							     * or the .uo file, dependent on which
							     * backend is used *)

    (* -------------------------------------------------------------
     * link_files_with_runtime_system files run : Link 
     * a list `files' of partially linked files (.o files) to the 
     * runtime system (also partially linked) and produce an executable 
     * called `run'. 
     * ------------------------------------------------------------- *)
      
    val link_files_with_runtime_system : string list -> string -> unit

    val backend_name : string (* e.g., X86, KAM, Barry, JS *)

    val mlbdir : unit -> string

    val pu_linkinfo : linkinfo Pickle.pu
  end
