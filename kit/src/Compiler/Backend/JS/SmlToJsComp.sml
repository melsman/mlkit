signature ENV = sig
  type t
  val empty  : t
  val initial : t
  val plus : t * t -> t
  val pp : bool -> t -> string   (* bool specifies whether values are printed *)
  val pu : t Pickle.pu
end
                
signature SMLTOJS_COMP = sig
  structure Env : ENV
  type modcode
  val compile  : Env.t * string -> Env.t * modcode  (* may raise Fail *)
  val execute  : modcode -> unit                    (* may raise exn *)
  val load_url : string -> Env.t * modcode
  val link     : modcode list -> modcode
  val pp       : modcode -> string
end


structure SmlToJsComp : SMLTOJS_COMP = struct

  val dummyExn = IO.ClosedStream  (* force loading of IO.js file *)
  fun chat(s: string) = if !Flags.chat then print (s^"\n") else ()

  structure E : EXECUTION 
                    where type target = ExpToJs.Js * {exports: string list,
                                                      imports: string list}
    = ExecutionJS

  structure MO : MANAGER_OBJECTS0 =
    ManagerObjects0(structure Execution = E)

  structure Basis = MO.Basis

  structure Env : ENV = struct
    type t = MO.Basis
    val empty = Basis.empty
    val initial = Basis.initial()
    val plus = Basis.plus
    fun pp _ b = PrettyPrint.flatten(PrettyPrint.format(100,Basis.layout b))
    val pu = Basis.pu
  end

  datatype modcode = Seq_mc of modcode list
                   | Single_mc of E.target * E.linkinfo

  val link = Seq_mc

  structure ModCodeMini : MODCODE_MINI = struct
    type modcode = modcode
    type linkinfo = E.linkinfo
    type target = E.target
    fun seq (mc1,mc2) = Seq_mc[mc1,mc2]
    val empty = Seq_mc[]
    fun mk_modcode (t,li,_) = Single_mc(t,li)
    fun emit (_,mc) = mc
  end

  fun pp_mc mc =
      let fun p (Seq_mc mcs) acc = List.foldl (fn (mc,a) => p mc a) acc mcs
            | p (Single_mc((t,_),li)) acc = ExpToJs.toString t :: acc
      in String.concatWith ";\n" (rev(p mc nil))
      end

  fun exec_mc mc =
      let val s = pp_mc mc
          val f : unit -> unit =
            JsCore.exec0 {stmt=s,res=JsCore.unit}
      in f()
      end

  fun mlbdir() = "MLB/Js0"

  structure IntModules = 
    IntModules(structure ManagerObjects = MO
               structure Execution = E
               structure ModCodeMini = ModCodeMini
               val mlbdir = mlbdir)

  val mk_name =
      let val c = ref 0
      in fn () =>
            ("top" ^ Int.toString(!c)
             before (c := !c + 1))
      end

  fun print_error_report report = Report.print' report (!Flags.log)

  exception PARSE_ELAB_ERROR of ParseElab.ErrorCode.ErrorCode list

  fun compile (e:Env.t, src:string) : Env.t * modcode =
      let 
        val _ = Flags.reset_warnings ()
        val mlbfile = mk_name()
        val abs_mlbfile = ModuleEnvironments.mk_absprjid mlbfile
        val _ = Name.bucket := []
        val _ = Name.baseSet mlbfile
        val (infB,elabB,oe,intB) = Basis.un e
        val res = ParseElab.parse_elab 
                      {absprjid=abs_mlbfile,
                       src=ParseElab.SrcString src,
                       infB=infB, elabB=elabB} 
      in case res of 
           ParseElab.FAILURE (report, error_codes) => 
           (  print "\n"
            ; print_error_report report
            ; raise PARSE_ELAB_ERROR error_codes)
         | ParseElab.SUCCESS {report,infB=infB',elabB=elabB',topdec} =>
           let 
             val _ = chat "[opacity elimination begin...]"
             val (topdec', oe') = OpacityElim.opacity_elimination(oe, topdec)
             val _ = chat "[opacity elimination end...]"
             val _ = chat "[interpretation begin...]"
             val functor_inline = false
             val smlfile = mk_name()
             val (intB', modc) = 
                 IntModules.interp(functor_inline, abs_mlbfile, 
                                   intB, topdec', smlfile)
             val _ = List.app Name.mk_rigid (!Name.bucket)
             val _ = Name.bucket := []
             val _ = chat "[interpretation end...]"                      
             val B' = Basis.mk(infB',elabB',oe',intB')
           in (B',modc)
           end
      end

  fun execute mc = exec_mc mc

  fun load_url _ = raise Fail "SmlToJsComp.load_url not implemented" 
  fun link _ = raise Fail "SmlToJsComp.link not implemented" 

  val pp = pp_mc
end
