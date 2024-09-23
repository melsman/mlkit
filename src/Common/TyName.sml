(* Type names *)

structure TyName :> TYNAME =
  struct

    val print_type_name_stamps = Flags.add_bool_entry
        {long="print_type_name_stamps", short=SOME "Ptypestamps", item=ref false, neg=false,
         menu=["Layout", "print type name stamps"], desc=
         "Print type name stamps and their attributes in types\n\
          \and expressions."}

    fun die s = Crash.impossible ("TyName." ^ s)
    val tag_values = Flags.is_on0 "tag_values"
    val values_64bit = Flags.is_on0 "values_64bit"

    (* Type names are based on names, which may be `matched'. In
     * particular, if two type names, n1 and n2, are successfully
     * matched, eq(n1,n2) = true. This may affect the canonical
     * ordering of type names. *)

    type tycon = TyCon.tycon
    type name = Name.name
    type rank = int

    datatype boxity = SINGLE of boxity
                    | UNB_LOW | UNB_ALL
                    | ENUM | BOXED

    type TyName = {tycon: tycon,
                   name: name,
                   arity: int,
                   rank: rank ref,
                   equality: bool,
                   boxity: boxity ref}

    fun boxity (tn:TyName) : boxity =
        !(#boxity tn)

    structure Rank =
      struct
        type rank = rank
        local val current_rank = ref 0
              val rank_bucket : rank ref list ref = ref []
        in
          fun current () = !current_rank
          fun new () = (current_rank := !current_rank + 1;
                        let val rr = ref (!current_rank)
                        in rank_bucket := rr :: ( !rank_bucket ) ;
                          rr
                        end)
          fun min (r1,r2) = Int.min (r1, r2)
          fun reset () =
            (List.app (fn rr => rr := 0) (!rank_bucket);
             rank_bucket := [];
             current_rank := 0)
          val op <= : rank * rank -> bool = op <=
          fun from_TyName ({rank=ref rank,...}:TyName) = rank
          val pu = Pickle.int
          val pu_rankrefOne = Pickle.refOneGen pu
          val pp = Int.toString
        end
      end

    fun fresh boxity {tycon: tycon, arity: int, equality: bool} =
        let val name = Name.new()
      in (* if tycon = TyCon.tycon_EXN then print ("generating fresh type name exn(" ^
                                                   Int.toString(Name.key name) ^ ")\n") else (); *)
        {tycon=tycon, name=name, rank=Rank.new(), arity=arity,
         equality=equality, boxity=ref boxity}
      end

    fun freshTyName r = fresh BOXED r

    fun arity ({arity, ...} : TyName) : int = arity

    fun equality ({equality, ...} : TyName) : bool = equality

    fun tycon ({tycon, ...} : TyName) : tycon = tycon

    fun name ({name, ...} : TyName) : name = name

    val id = Name.key o name

    (* We should only allow matching of type names when their attributes
     * are equal; otherwise changes in attributes are not caught by
     * the system.. *)
    fun match (tn1,tn2) =
        if (equality tn1 = equality tn2
            andalso arity tn1 = arity tn2
            andalso !(#boxity tn1) = !(#boxity tn2)) then
          Name.match(name tn1, name tn2)
        else ()

    val op eq = fn (tn1,tn2) => Name.eq(name tn1, name tn2)

    local
        val bucket = ref nil
        fun predef b r =
            let val tn = fresh b r
            in bucket := tn :: !bucket
             ; tn
            end
    in
        val tyName_BOOL       = predef UNB_LOW {tycon=TyCon.tycon_BOOL,       arity=0, equality=true}
        val tyName_INT31      = predef UNB_LOW {tycon=TyCon.tycon_INT31,      arity=0, equality=true}
        val tyName_INT32      = predef BOXED   {tycon=TyCon.tycon_INT32,      arity=0, equality=true}
        val tyName_INT63      = predef UNB_ALL {tycon=TyCon.tycon_INT63,      arity=0, equality=true}
        val tyName_INT64      = predef BOXED   {tycon=TyCon.tycon_INT64,      arity=0, equality=true}
        val tyName_INTINF     = predef UNB_LOW {tycon=TyCon.tycon_INTINF,     arity=0, equality=true}
        val tyName_WORD8      = predef UNB_LOW {tycon=TyCon.tycon_WORD8,      arity=0, equality=true}
        val tyName_WORD31     = predef UNB_LOW {tycon=TyCon.tycon_WORD31,     arity=0, equality=true}
        val tyName_WORD32     = predef BOXED   {tycon=TyCon.tycon_WORD32,     arity=0, equality=true}
        val tyName_WORD63     = predef UNB_ALL {tycon=TyCon.tycon_WORD63,     arity=0, equality=true}
        val tyName_WORD64     = predef BOXED   {tycon=TyCon.tycon_WORD64,     arity=0, equality=true}
        val tyName_REAL       = predef BOXED   {tycon=TyCon.tycon_REAL,       arity=0, equality=false}
        val tyName_F64        = predef UNB_ALL {tycon=TyCon.tycon_F64,        arity=0, equality=false}
        val tyName_STRING     = predef BOXED   {tycon=TyCon.tycon_STRING,     arity=0, equality=true}
        val tyName_CHAR       = predef UNB_LOW {tycon=TyCon.tycon_CHAR,       arity=0, equality=true}
        val tyName_LIST       = predef UNB_LOW {tycon=TyCon.tycon_LIST,       arity=1, equality=true}
        val tyName_FRAG       = predef BOXED   {tycon=TyCon.tycon_FRAG,       arity=1, equality=true}
        val tyName_REF        = predef BOXED   {tycon=TyCon.tycon_REF,        arity=1, equality=true}
        val tyName_ARRAY      = predef BOXED   {tycon=TyCon.tycon_ARRAY,      arity=1, equality=true}
        val tyName_VECTOR     = predef BOXED   {tycon=TyCon.tycon_VECTOR,     arity=1, equality=true}
        val tyName_CHARARRAY  = predef BOXED   {tycon=TyCon.tycon_CHARARRAY,  arity=0, equality=true}
        val tyName_FOREIGNPTR = predef UNB_ALL {tycon=TyCon.tycon_FOREIGNPTR, arity=0, equality=true}
        val tyName_EXN        = predef BOXED   {tycon=TyCon.tycon_EXN,        arity=0, equality=false}
        val _ = Rank.reset()
        val tynamesPredefined = !bucket
    end

    fun tyName_IntDefault () =
        case (tag_values(), values_64bit()) of
            (true,  true)  => tyName_INT63
          | (true,  false) => tyName_INT31
          | (false, true)  => tyName_INT64
          | (false, false) => tyName_INT32

    fun tyName_WordDefault () =
        case (tag_values(), values_64bit()) of
            (true,  true)  => tyName_WORD63
          | (true,  false) => tyName_WORD31
          | (false, true)  => tyName_WORD64
          | (false, false) => tyName_WORD32

    local
      fun boxity_num32 () =
          if tag_values() then BOXED else UNB_LOW
      fun boxity_num64 () =
          if tag_values() then BOXED else UNB_ALL
    in
      fun boxity tn =
          if eq(tn,tyName_INT32) orelse eq(tn,tyName_WORD32) then
            boxity_num32 ()
          else if eq(tn,tyName_INT64) orelse eq(tn,tyName_WORD64) then
            boxity_num64 ()
          else !(#boxity tn)

      fun is_unboxed tn =
          case boxity tn of
              BOXED => false
            | _ => true
    end

    fun pr_boxity b =
        case b of
            SINGLE b => "single " ^ pr_boxity b
          | UNB_LOW => "lub"
          | UNB_ALL => "hub"
          | ENUM => "enum"
          | BOXED => "box"

    fun pr_TyName (tn: TyName) : string =
      let val str = TyCon.pr_TyCon (tycon tn)
      in
        if print_type_name_stamps() then
          let val eq = if equality tn then "E " else ""
              val (i,b) = Name.key (name tn)
              val id = Int.toString i
              val box = pr_boxity (boxity tn)
          in str ^ "(" ^ eq ^ id ^ b ^ ":" ^ box ^ ")"
          end
        else
          (if tag_values() then
             (if eq(tn, tyName_INT63) then "int"
              else if eq(tn, tyName_WORD63) then "word"
                   else str)
           else
             (if eq(tn, tyName_INT64) then "int"
              else if eq(tn, tyName_WORD64) then "word"
                   else str))
      end

    fun pr_TyName' ({tycon,name,...}:TyName) : string =
	let val str = TyCon.pr_TyCon tycon
            val (i,s) = Name.key name
            val s = if Name.baseGet() = s then "" else s
	in str ^ ":" ^ Int.toString i ^ ":" ^ s
	end


    fun setBoxity (tn: TyName, b:boxity) : unit =
        case boxity tn of
            BOXED => #boxity tn := b
          | _ => die ("setBoxity.boxity already set for tyname " ^ pr_TyName tn)

    val pu_boxity =
        let val toInt = fn SINGLE _ => 0
                         | UNB_LOW => 1
                         | UNB_ALL => 2
                         | ENUM => 3
                         | BOXED => 4
        in Pickle.dataGen ("boxity", toInt,
                           [Pickle.con1 SINGLE (fn SINGLE b => b | _ => die "pu_boxity"),
                            Pickle.con0 UNB_LOW,
                            Pickle.con0 UNB_ALL,
                            Pickle.con0 ENUM,
                            Pickle.con0 BOXED])
        end

    val pu =
        Pickle.hashConsEq eq
        (Pickle.register "TyName" tynamesPredefined
         let fun to ((t,n,a),r,e,b) : TyName =
                 {tycon=t, name=n, arity=a, rank=r,
                  equality=e, boxity=b}
             fun from ({tycon=t, name=n, arity=a, rank=r,
                        equality=e, boxity=b} : TyName) = ((t,n,a),r,e,b)
         in Pickle.newHash (#1 o Name.key o name)
             (Pickle.convert (to,from)
              (Pickle.tup4Gen0(Pickle.tup3Gen0(TyCon.pu,Name.pu,Pickle.int),
                               Pickle.refOneGen Pickle.int,Pickle.bool,Pickle.refOneGen pu_boxity)))
         end)

    structure QD : QUASI_DOM =
      struct
        type dom = TyName
        val name = name
        val pp = pr_TyName
      end

    structure Map = QuasiMap(QD)

    structure Set = QuasiSet(QD)

    val predefined = Set.fromList tynamesPredefined

    fun pr_TyName_repl (tn: TyName) : string =
      let val str = TyCon.pr_TyCon (tycon tn)
      in if Set.member tn predefined then str
         else let val (i,b) = Name.key(name tn)
              in str ^ "_" ^ Int.toString i
              end
      end

    type StringTree = PrettyPrint.StringTree
    val layout = PrettyPrint.LEAF o pr_TyName

(*
    structure TestMap =
      struct
        val _ = print "[test begin]\n"
        fun error s = print ("error: " ^ s ^ "\n")
        fun assert s false = error s
          | assert _ _ = ()

        fun new s = freshTyName{tycon=TyCon.mk_TyCon s, arity=0, equality=false}

        val t = new "t"

        val l = ["t1", "t2", "t3", "t4", "t5"]

        val ts as [t1,t2,t3,t4,t5] = map new l

        val m = Map.fromList (map (fn t => (t, pr_TyName t)) ts)

        val _ = case Map.lookup m t2
                  of SOME s => assert "test1" (s=pr_TyName t2)
                   | _ => error "test2"
        val m' = Map.restrict (m,[t3,t4])

        val _ = case Map.lookup m' t2
                  of NONE => ()
                   | _ => error "test3"

        val s4 = pr_TyName t4
        val _ = case Map.lookup m' t4
                  of SOME s => assert "test4" (s=s4)
                   | _ => error "test5"

        val _ = Name.mark_gen (name t4)
        val _ = Name.mark_gen (name t)
        val _ = match(t4,t)
        val _ = Name.unmark_gen (name t4)
        val _ = Name.unmark_gen (name t)
        val _ = case Map.lookup m' t
                  of SOME s => assert "test6" (s=s4)
                   | _ => error "test7"
        val _ = case Map.lookup m' t4
                  of SOME s => assert "test8" (s=s4)
                   | _ => error "test9"

        val _ = print "[end of test]\n"
      end
  *)
  end
