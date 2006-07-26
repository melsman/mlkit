(* $Id:$ *)
(* Author: Carsten Varming 2006 *)

structure Arg :>
  sig
    val parse : string list -> (string list * string list * string)
                           (*   preserve      infiles       outfile *)
  end = 
  struct
    datatype Actions = Pres of string
                     | In of string
                     | Out of string
    
    local 
      datatype Keywords = Preserve of string option
                        | Infile of string option
                        | Outfile of string option
                        | UnKnown
      
      local
        val state = ref false
      in
        fun keyword _ "--preserve" = SOME (Preserve NONE)
          | keyword _ "-p" = SOME (Preserve NONE)
          | keyword _ "--infile" = SOME(Infile NONE)
          | keyword _ "-i" = SOME (Infile NONE)
          | keyword _ "--outfile" = SOME(Outfile NONE)
          | keyword _ "-o" = SOME(Outfile NONE)
          | keyword _ "--" = (state:=true;NONE)
          | keyword s x = 
             if s 
             then 
               let
                 val (k,v) = Substring.splitl (fn x => x <> #"=") (Substring.full x)
                 fun strip (l,n) = Substring.string (#2 (Substring.splitAt (l,n)))
               in
                 if Substring.size k = 0 
                 then SOME UnKnown
                 else if Substring.isPrefix "-" k
                 then case keyword false (Substring.string k)
                      of NONE => NONE
                       | SOME UnKnown => raise Fail ("Unknown option : " ^ (Substring.string k))
                       | SOME (Preserve _) => SOME (Preserve (SOME (strip (v,1))))
                       | SOME (Infile _) => SOME (Infile (SOME (strip (v,1))))
                       | SOME (Outfile _) => SOME (Outfile (SOME (strip (v,1))))
                 else SOME UnKnown
               end
             else SOME UnKnown
        val keyword = fn x => keyword true x
        
        fun next k [] = raise Fail (k ^ " takes one argument")
          | next k (x::xr) = case keyword x 
                             of SOME UnKnown => x
                              | _ => raise Fail (x ^ " not a proper argument to " ^ k)
        
        datatype Actions = Pres' of string
                         | In' of string
                         | Out' of string
                         | File' of string
        
        fun parseArgs [] = (state := false ; [])
          | parseArgs (x::xr) = 
                if not (!state) then case keyword x
                                of SOME (Preserve NONE) => Pres' (next x xr):: (parseArgs (tl xr))
                                 | SOME (Preserve (SOME v)) => Pres' v :: (parseArgs xr)
                                 | SOME (Infile NONE) => In' (next x xr) :: (parseArgs (tl xr))
                                 | SOME (Infile (SOME v)) => In' v :: (parseArgs xr)
                                 | SOME (Outfile NONE) => Out' (next x xr) :: (parseArgs (tl xr))
                                 | SOME (Outfile (SOME v)) => Out' v :: (parseArgs xr)
                                 | SOME UnKnown => (File' x) :: (parseArgs xr)
                                 | NONE => parseArgs xr
                else (state := false ; List.map File' (x::xr))
      end
      
      fun checkArgs false [] = raise Fail "No output file ?"
        | checkArgs true [] = []
        | checkArgs out (File' x :: xr) = if out orelse List.exists (fn (File' _) => true
                                                                     | (Out' _) => true
                                                                     | _ => false) xr
                                         then
                                           (In x) :: (checkArgs out xr)
                                         else
                                           (Out x) :: (checkArgs true xr)
        | checkArgs out (Pres' x :: xr) = (Pres x) :: (checkArgs out xr)
        | checkArgs true (Out' _ :: _) = raise Fail "Multiple output files"
        | checkArgs false (Out' x :: xr) = (Out x) :: (checkArgs true xr)
        | checkArgs out (In' x :: xr) = (In x) :: (checkArgs out xr)
    in
      val checkArgs = fn a => checkArgs false (parseArgs a)
    end
    local
      fun getOut l = case List.mapPartial (fn (Out x) => SOME x | _ => NONE) l
                     of [x] => x
                      | [] => raise Fail "No output file ?"
                      | _ => raise Fail "Multiple output files"
      fun getIn l = List.mapPartial (fn (In x) => SOME x | _ => NONE) l
      fun getPres l = List.mapPartial (fn (Pres x) => SOME x | _ => NONE) l
    in
      fun split l = (getPres l, getIn l, getOut l)
    end
    fun parse l = split (checkArgs l)
  end

