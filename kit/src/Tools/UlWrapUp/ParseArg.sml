(* Author: Carsten Varming 2006 *)

functor QuickSort(Arg:
                    sig
                      type A
                      type V
                      val < : (A * A) -> bool
                      val sub : V * int -> A
                      val update : V * int * A -> V
                      val length : V -> int
                    end)
                    :>
                    sig
                      val sort : Arg.V -> Arg.V
                    end=
  struct
    fun swap a i j = (fn (ja,ia) => Arg.update(Arg.update(a,i,ja),j,ia))
                     (Arg.sub (a,j),Arg.sub(a,i))

    fun split a i j k =
      let 
        val p = Arg.sub(a,j)
        fun left i j = if i < j andalso (not (Arg.<(p,Arg.sub(a,i))))
                     then left (i+1) j
                     else i
        fun right k j = if k > j andalso (not (Arg.<(Arg.sub(a,k),p)))
                      then right (k-1) j
                      else k
        fun go a i j k = let 
                         val i' = left i j 
                         val k' = right k j
                       in
                         if i' = j andalso k' = j then (a,j)
                         else (let val a = swap a i' k' 
                               in if i' = j then go a i' k' k'
                                  else if k' = j then go a i' i' k'
                                  else go a i' j k'
                               end)
                       end
      in 
        go a i j k
      end 

    fun sort a i k = if i < k
                     then 
                       let
                         val j = i + (k - i) div 2
                         val (a,p) = split a i j k
                         val a = sort a i (p - 1)
                         val a = sort a (p+1) k
                       in a
                       end
                     else a

    val sort = fn a => sort a 0 (Arg.length a - 1)
  end

functor ArgType(Out : 
  sig
    type A
  end) :>
  sig
    datatype ArgKind = 
        Int of (int -> Out.A)
      | String of (string -> Out.A)
      | Binary of (bool -> Out.A)
  end =
  struct
    datatype ArgKind = 
        Int of (int -> Out.A)
      | String of (string -> Out.A)
      | Binary of (bool -> Out.A)
  end

functor Argument(Data :
  sig
    type A
    structure Arg :
      sig
        datatype ArgKind = 
          Int of (int -> A)
        | String of (string -> A)
        | Binary of (bool -> A)
      end
    val Default : string -> A
    exception BadArg of string
    val constructors : (string * Arg.ArgKind) list
  end) :>
  sig
    val parse : string list -> Data.A list
  end =
  struct
    structure Sort = QuickSort(struct 
                                 type A = (string * Data.Arg.ArgKind)
                                 fun op< ((s1,_),(s2,_)) = String.<(s1,s2)
                                 type V = A Array.array
                                 val sub = Array.sub
                                 fun update (a,i,e) = (Array.update (a,i,e); a)
                                 val length = Array.length
                               end)

    val constructors = Array.fromList Data.constructors
    val _ = Sort.sort constructors
    fun find (a : (string * Data.Arg.ArgKind) Array.array) s i j = if i < j 
                       then
                         let val p = i + (j - i) div 2
                         in
                           case String.compare(s,#1 (Array.sub(a,p)))
                           of EQUAL => SOME (#2 (Array.sub(a,p)))
                            | LESS => find a s i p
                            | GREATER => find a s (p+1) j
                         end
                       else NONE
    val find = fn (a,s) => find a s 0 (Array.length a)
    local
      val state = ref false
    in
      datatype Kind = Eq of Data.A
                    | Int of int -> Data.A
                    | String of string -> Data.A
                    | Binary of bool -> Data.A
                    | Unknown
      fun toArgKind (Int f) = Data.Arg.Int f
        | toArgKind (String f) = Data.Arg.String f
        | toArgKind (Binary f) = Data.Arg.Binary f
      fun getBinary "true" = SOME true
        | getBinary "false" = SOME false
        | getBinary x = case Int.fromString x
                        of SOME 0 => SOME false
                         | SOME x => SOME true
                         | NONE => NONE

      fun data k (Data.Arg.Int f) s = f (case Int.fromString s 
                                      of NONE => raise Data.BadArg (s ^ " not a proper argument to " ^ k)
                                       | SOME i => i)
        | data _ (Data.Arg.String f) s = f s
        | data k (Data.Arg.Binary f) s = f (case getBinary s
                                         of SOME b => b
                                          | NONE => raise Data.BadArg (s ^ " not a proper argument to " ^ k))

      fun keyword s a =
           if s
           then SOME Unknown
           else
             case find (constructors,a)
             of SOME (Data.Arg.Int v) => SOME (Int v)
              | SOME (Data.Arg.String v) => SOME (String v)
              | SOME (Data.Arg.Binary v) => SOME (Binary v)
              | NONE =>
           if a = "--"
           then (state := true ; NONE)
           else
               let
                 val (k,v) = Substring.splitl (fn x => x <> #"=") (Substring.full a)
                 fun strip (l,n) = Substring.string (#2 (Substring.splitAt (l,n)))
                 val k' = Substring.string k
               in
                 if Substring.size k = 0 
                 then SOME Unknown
                 else if Substring.isPrefix "-" k
                 then
                   case find (constructors,k') 
                   of NONE => raise Data.BadArg ("Unknown option : " ^ k')
                    | SOME d => SOME (Eq (data k' d (strip (v,1))))
                 else SOME Unknown
               end
      val keyword = fn x => keyword (!state) x

      fun next k x = case keyword x 
                     of SOME Unknown => x
                      | _ => raise Data.BadArg (x ^ " not a proper argument to " ^ k)
    
      fun parseArgs [] acc = List.rev acc
        | parseArgs (x::xr) acc = 
              if not (!state)
              then
                case keyword x
                of NONE => parseArgs xr acc
                 | SOME Unknown => parseArgs xr (Data.Default x :: acc)
                 | SOME (Eq a) => parseArgs xr (a::acc)
                 | SOME f => case xr of [] => raise Data.BadArg (x ^ " takes one argument")
                              | (x'::xr') => parseArgs xr' ((data x (toArgKind f) (next x x')) :: acc)
               else parseArgs xr (Data.Default x :: acc)

      val parse = fn x => (state:=false ; parseArgs x []) 
    end
  end;

structure Arg :> 
  sig
    val parse : string list -> (string * string list * string)
  end =
  struct
    structure D = 
      struct
        datatype A = Out of string
                   | In of string
                   | Pres of string
                   | Undef of string
        structure Arg = ArgType(struct
                                  type A = A
                                end)
        val Default = Undef
        exception BadArg of string
        val constructors = [("-p",Arg.String Pres),("--preserve",Arg.String Pres),
                            ("-i",Arg.String In),("--infile",Arg.String In),
                            ("-o",Arg.String Out),("--outfile",Arg.String Out)]
      end
    structure A = Argument(D)
    fun getPres l = List.foldr (fn (D.Pres i,acc) => i::acc | (_,acc) => acc) [] l
    fun getIn l = List.foldr (fn (D.In i,acc) => i::acc | (_,acc) => acc) [] l
    fun getOut l = List.foldr (fn (D.Out i,acc) => i::acc | (_,acc) => acc) [] l
    fun getUndef l = List.foldr (fn (D.Undef i,acc) => i::acc | (_,acc) => acc) [] l
    fun parse l = 
      let
        val tmp = A.parse l
        val undef = getUndef tmp
        val (infile,undef) = case getIn tmp
                     of [i] => (i,undef)
                      | [] => (case undef
                              of [] => raise Fail "No input file ?"
                               | (x::xr) => (x,xr))
                      | _ => raise Fail "Multiple input files ?"
        val (outfile,undef) = case getOut tmp
                     of [i] => (i,undef)
                      | [] => (case undef
                              of [] => raise Fail "No output directory ?"
                               | (x::xr) => (x,xr))
                      | _ => raise Fail "Multiple output directories ?"
        val _ = case undef of [] => () | _ => raise Fail "Undefined parameters ?"
      in
        (infile,getPres tmp, outfile)
      end
  end


