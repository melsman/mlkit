structure WebSerialize :> WEB_SERIALIZE =
struct

  type 'a Type = {name: string,
      to_string: 'a -> string,
      from_string: string -> 'a}

  fun Pair (t1 : 'a Type) (t2: 'b Type) =
    let
      (* Type pair is printed: (type1,type2) *)
      val name = "(" ^ (#name t1) ^ "," ^ (#name t2) ^ ")"
      fun to_string (a,b) = 
        let
          val a_s = (#to_string t1) a
          val a_sz = Int.toString (String.size a_s)
          val b_s = (#to_string t2) b
        in
          a_sz ^ ":" ^ a_s ^ b_s
        end
      fun from_string s =
        let
          val s' = Substring.full s
          val (a_sz,rest) = 
            Option.valOf (Int.scan StringCvt.DEC Substring.getc s')
          val rest = #2(Option.valOf (Substring.getc rest)) (* skip ":" *)
          val (a_s,b_s) = (Substring.slice(rest,0,SOME a_sz),Substring.slice(rest,a_sz,NONE))
          val a = (#from_string t1) (Substring.string a_s)
          val b = (#from_string t2) (Substring.string b_s)
        in
          (a,b)
        end
    in
      {name=name,
       to_string=to_string,
       from_string=from_string}
    end

  fun Option (t : 'a Type) =
    let
      (* Option type is printed: O(type) *)
      val name = "O(" ^ (#name t) ^ ")" 
      fun to_string a = 
        case a of
          NONE => "0:N()"
        | SOME v => 
      let
        val v_s = (#to_string t) v
        val v_sz = Int.toString (String.size v_s)
      in
        v_sz ^ ":S(" ^ v_s ^ ")"
      end
      fun from_string s =
        let
          val s' = Substring.full s
          val (v_sz,rest) = 
            Option.valOf (Int.scan StringCvt.DEC Substring.getc s')
          val rest = #2(Option.valOf (Substring.getc rest)) (* skip ":" *)
          val (N_S,rest) = Option.valOf (Substring.getc rest) (* read N og S *)
          val rest = #2(Option.valOf (Substring.getc rest)) (* skip "(" *)
        in
          if N_S = #"S" then
            SOME ((#from_string t) (Substring.string (Substring.slice(rest,0,SOME v_sz))))
          else
            NONE
         end
    in
      {name=name,
       to_string=to_string,
       from_string=from_string}
    end

  fun List (t : 'a Type ) =
    let
      (* List type is printed: L(type) *)
      val name = "L(" ^ (#name t) ^ ")"
      (* Format: [x1_sz:x1...xN_sz:xN] *)
      fun to_string xs = 
        let
    fun to_string_x x =
      let
        val v_x = (#to_string t) x
      in
        Int.toString (String.size v_x) ^ ":" ^ v_x
      end
    val xs' = List.map to_string_x xs
        in
    "[" ^ (String.concat xs') ^ "]"
        end
      fun from_string s =
        let
    fun read_x (rest,acc) = 
      if Substring.size rest = 1 (* "]" *) then
        List.rev acc
      else
        let
          val (x_sz,rest) = Option.valOf (Int.scan StringCvt.DEC Substring.getc rest)
          val rest = #2(Option.valOf (Substring.getc rest)) (* skip ":" *)
          val (x_s,rest) = (Substring.slice(rest,0,SOME x_sz),Substring.slice(rest,x_sz,NONE))
        in
          read_x (rest,((#from_string t) (Substring.string x_s)) :: acc)
        end
    val s' = Substring.full s
    val rest = #2(Option.valOf (Substring.getc s')) (* skip "[" *)
        in
    read_x (rest,[])
        end
    in
      {name=name,
       to_string=to_string,
       from_string=from_string}
    end

  fun Triple (t1 : 'a Type) (t2: 'b Type) (t3: 'c Type) =
    let
      (* Type triple is printed (type1,type2,type3) *)
      val name = "(" ^ (#name t1) ^ "," ^ (#name t2) ^ "," ^ (#name t3) ^ ")"
      fun to_string (a,b,c) = 
        let
    val a_s = (#to_string t1) a
    val a_sz = Int.toString (String.size a_s)
    val b_s = (#to_string t2) b
    val b_sz = Int.toString (String.size b_s)
    val c_s = (#to_string t3) c
        in
    a_sz ^ ":" ^ a_s ^ b_sz ^ ":" ^ b_s ^ c_s
        end
      fun from_string s =
        let
    val s' = Substring.full s
    val (a_sz,rest) = 
      Option.valOf (Int.scan StringCvt.DEC Substring.getc s')
    val rest = #2(Option.valOf (Substring.getc rest)) (* skip ":" *)
    val (a_s,rest) = (Substring.slice(rest,0,SOME a_sz),Substring.slice(rest,a_sz,NONE))
    val (b_sz,rest) = 
      Option.valOf (Int.scan StringCvt.DEC Substring.getc rest)
    val rest = #2(Option.valOf (Substring.getc rest)) (* skip ":" *)      
    val (b_s,c_s) = (Substring.slice(rest,0,SOME b_sz),Substring.slice(rest,b_sz,NONE))
    val a = (#from_string t1) (Substring.string a_s)
    val b = (#from_string t2) (Substring.string b_s)
    val c = (#from_string t3) (Substring.string c_s)
        in
    (a,b,c)
        end
    in
      {name=name,
       to_string=to_string,
       from_string=from_string}
    end

  (* Pre defined types *)
  val Unit   = {name="U",to_string=fn () => "unit", from_string=(fn "unit" => () 
                                                                  | _ => raise Option.Option)}
  val Int    = {name="I",to_string=Int.toString,from_string=Option.valOf o Int.fromString}
  val Real   = {name="R",to_string=Real.toString,from_string=Option.valOf o Real.fromString}
  val Bool   = {name="B",to_string=Bool.toString,from_string=Option.valOf o Bool.fromString}
  val Char   = {name="C",to_string=Char.toString,from_string=Option.valOf o Char.fromString}
  val String = {name="S",to_string=(fn s => s),from_string=(fn s => s)}

end
