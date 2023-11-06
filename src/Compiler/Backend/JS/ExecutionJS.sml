
structure ExecutionJS : EXECUTION =
  struct
    structure Compile = CompileJS
    structure TopdecGrammar = PostElabTopdecGrammar
    structure PP = PrettyPrint
    structure Labels = AddressLabels

    structure DecGrammar = TopdecGrammar.DecGrammar

    structure CompileBasis = CompileBasisJS

    val backend_name = "SmlToJs"
    val backend_longname = "SmlToJs - Standard ML to JavaScript Compiler"

    type CompileBasis = CompileBasis.CompileBasis
    type CEnv = CompilerEnv.CEnv
    type Env = CompilerEnv.ElabEnv
    type strdec = TopdecGrammar.strdec
    type strexp = TopdecGrammar.strexp
    type funid = TopdecGrammar.funid
    type strid = TopdecGrammar.strid
    type lab = string
    fun pr_lab s = s
    type linkinfo = {unsafe:bool,imports:lab list,exports:lab list}
    type target = Compile.target

    fun die s = (print ("ExecutionJS.Die: " ^ s); raise Fail s)

    val dummy_label = "__DUMMYDUMMY"
    val code_label_of_linkinfo : linkinfo -> lab = fn _ => dummy_label

    fun imports_of_linkinfo (li: linkinfo) : lab list * lab list =
        (#imports li,nil)
    fun exports_of_linkinfo (li: linkinfo) : lab list * lab list =
        (#exports li,nil)

    fun unsafe_linkinfo (li: linkinfo) : bool =  #unsafe li

    (* Hook to be run before any compilation *)
    val preHook = Compile.preHook

    (* Hook to be run after all compilations (for one compilation unit) *)
    val postHook = Compile.postHook

    datatype res = CodeRes of CEnv * CompileBasis * target * linkinfo
                 | CEnvOnlyRes of CEnv
    fun compile fe (ce,CB,strdecs,vcg_file) =
      let val (cb,()) = CompileBasis.de_CompileBasis CB
      in
	case Compile.compile fe (ce, cb, strdecs)
	  of Compile.CEnvOnlyRes ce => CEnvOnlyRes ce
	   | Compile.CodeRes(ce,cb,target,safe) =>
	    let
                val {imports,exports} = #2 target
		val linkinfo : linkinfo = {unsafe=not(safe),imports=imports,exports=exports}
		val CB = CompileBasis.mk_CompileBasis(cb,())
	    in CodeRes(ce,CB,target,linkinfo)
	    end
      end

    val generate_link_code = NONE
    val generate_repl_init_code = NONE

    fun emit a = Compile.emit a

    val be_rigid = true

    val op ## = OS.Path.concat infix ##

    val js_dom_mode = Flags.add_bool_entry
	{long="js_dom_mode", short=NONE,
	 menu=["File","js dom mode"],
	 item=ref false, neg=false,
         desc= "Generate a full HTML document including\n\
               \a proper HTML DOCTYPE specification."}

    val get_jslibs = Flags.add_stringlist_entry
      {long="javascript_library_paths",
       short=SOME "jslibs",
       menu=["Control","JavaScript library paths"],
       item=ref nil,
       desc="Use this option to add javascript library paths\n\
            \to the generated html and thereby allow for SML\n\
            \code to reference existing JavaScript libraries."}

    val js_path_compress = Flags.add_bool_entry
	{long="js_path_compress", short=NONE,
	 menu=["File","js path compress"],
	 item=ref false, neg=false,
         desc= "Compress (make canonical) Javascript file path\n\
               \references in the resulting run.html file."}

    val js_path_prefix = Flags.add_string_entry
        {long="js_path_prefix", short=NONE, menu=["File", "js path prefix"],
         item=ref "",
         desc= "Prefix to add to each non-absolute Javascript\n\
               \file path in the resulting run.html file."}

    val js_path_relative_to = Flags.add_string_entry
        {long="js_path_relative_to", short=NONE, menu=["File", "js path relative to"],
         item=ref "",
         desc= "Absolute directory for which each absolute\n\
               \Javascript file path is made relative to in\n\
               \the resulting run.html file."}

    val () = Flags.turn_off "values_64bit"

    fun link_files_with_runtime_system files run =
	let val html_file = run ^ ".html"
	    val os = TextIO.openOut html_file
            fun out s = TextIO.output(os,s)
            fun maybe_out_DOCTYPE () =
                if js_dom_mode() then
                  (out "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n";
                   out "<html id='html_id'>\n";
                   out "<head id='head_id'>\n")
                else ()
            fun maybe_out_body () =
                if js_dom_mode() then
                  (out "</head>\n";
                   out "<body id='body_id'></body>\n";
                   out "</html>\n")
                else ()
            fun outJsFile f =
                let val f =
                        if OS.Path.isAbsolute f then
                          if js_path_relative_to() <> "" then
                            (OS.Path.mkRelative{path=f,relativeTo=js_path_relative_to()}
                             handle OS.Path.Path => die "link: outJsFile")
                          else f
                        else js_path_prefix() ## f
                    val f = if js_path_compress() then OS.Path.mkCanonical f
                            else f
                in out ("<script type=\"text/javascript\" src=\"" ^ f ^ "\"></script>\n")
                end
            val jslibs = get_jslibs()
            val files = jslibs @ files
	in
	    (out ("<!-- JavaScript generated by " ^ backend_longname ^ " -->\n");
             out ("<!-- See http://www.smlserver.org/smltojs -->\n");
             maybe_out_DOCTYPE();
             outJsFile (!Flags.install_dir ## "prims.js");
	     app outJsFile files;
             maybe_out_body();
	     TextIO.closeOut os;
	     print("[Created file " ^ html_file ^ "]\n"))
	    handle X => (TextIO.closeOut os; raise X)
	end

    fun create_repl_runtime _ _ = die "create_repl_runtime: not implemented"

    fun mlbdir () = "MLB" ## "Js"

    val pu_linkinfo =
        let val pu_sList = Pickle.listGen Pickle.string
        in
	  Pickle.convert (fn (is,es,b) => {imports=is,exports=es,unsafe=b},
			  fn {imports=is,exports=es,unsafe=b} => (is,es,b))
	                 (Pickle.tup3Gen(pu_sList,pu_sList,Pickle.bool))
        end

    fun mk_sharedlib _ =
        die "mk_sharedlib.unimplemented"

    datatype cval = datatype Compile.cval
    fun retrieve_longid (CE: CEnv) CBE (longid:CompilerEnv.longid) : string cval =
        die "retrieve_longid.unimplemented"

    datatype conkind = UNB | ENU | BOX
    type tyvar = CompilerEnv.tyvar
    type Type = CompilerEnv.Type
    type coninfo = string * (tyvar list * Type)
    fun tyname_reps (CB: CompileBasis) (tn: TyName.TyName) : coninfo list option =
        die "tyname_reps.unimplemented"

    val toJSString = SOME (Compile.mlToJsString)

    val export_basis_js =
        Flags.add_bool_entry
            {long="export_basis_js", short=SOME "ebjs", neg=false,
             item=ref false,
             menu=["General Control", "export basis in js file (SmlToJs)"],
             desc="When this flag is enabled, SmlToJs writes\n\
                  \pickled bases to file.eb.js files to be read by\n\
                  \js-client."}

  end
