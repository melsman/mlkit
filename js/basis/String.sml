
structure String =   (* Depends on StrBase and Char *)
  struct 

    type string = string (*CharVector.vector*)
    type char = char
    structure Char = Char

    (* Primitives *)
    fun sub_unsafe (s:string,i:int) : char = prim ("__bytetable_sub", (s,i))
    fun size (s : string) : int = prim ("__bytetable_size", s)
    fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))

    (* Arrays, implemented as JavaScript Arrays with number elements *)
    type array = chararray
    fun sub_array_unsafe(a:array,i:int) : char = prim ("word_sub0", (a,i))
    fun update_array_unsafe(a:array,i:int,c:char) : unit = prim("word_update0", (a, i, c))
    fun alloc_array_unsafe (i:int) : array = prim("word_table0", i)
    fun length_array(a:array) : int = prim("table_size",a)
    fun array_to_string (a: array) : string = prim("chararray_to_string", a)

    (* Body *)

    val maxSize = CharVector.maxLen

    fun sub(s, i) = if i<0 orelse i >= size s then raise Subscript
		    else sub_unsafe(s, i)

    fun extract (tab:string, i:int, slicelen:int option) : string =
      let val n = case slicelen of NONE => size tab - i | SOME n => n
	  val newvec : array = 
	    if i < 0 orelse n < 0 orelse n > maxSize
	      orelse i > size tab orelse i+n > size tab then raise Subscript
	    else alloc_array_unsafe n
	  fun blit (i1, i2) =
	    if i2 >= n then ()
	    else let val e : char = sub_unsafe(tab:string, i1:int)
		 in update_array_unsafe(newvec, i2, e);
		    blit (i1+1, i2+1)
		 end
      in blit(i,0); array_to_string newvec 
      end

    fun substring (s, i, j) = extract(s, i, SOME j)

    fun concat x = CharVector.concat x

    fun concatWith sep [] = ""
      | concatWith sep l = concat (tl (foldr (fn (s,acc)=>sep::s::acc) [] l))

    fun implode l = CharVector.fromList l
    fun explode s = StrBase.explode s

    fun str (c : char) : string = implode [c]

    fun translate f s = StrBase.translate f (s, 0, size s);
    fun tokens p s = map substring (StrBase.tokens p (s, 0, size s));
    fun fields p s = map substring (StrBase.fields p (s, 0, size s));

    fun map x = CharVector.map x

    fun isSuffix s1 s2 = 
	let val n1 = size s1 
	    and n2 = size s2
	    val offset = n2-n1
	    fun h j = 
		(* At this point s1[0..j-1] = s2[offset..offset+j-1] *)
		j = n1 orelse sub_unsafe(s1,j) = sub_unsafe(s2,j+offset) andalso h (j+1)
	in n1 <= n2 andalso h 0 
	end

    fun isSubstring "" s2 = true
      | isSubstring s1 s2 =
	let val n1 = size s1 
	    and n2 = size s2
	    val stop1 = n1-1
	    val stop2 = n2-n1
	    fun cmp offset =
		let fun h j = 
		    (* At this point s1[0..j-1] = s2[offset..j+offset-1] *)
		    j >= stop1 
		    orelse sub_unsafe(s1,j) = sub_unsafe(s2,j+offset) andalso h (j+1)
		in h 0 
		end
	(* Comparison at end of s1 good if s1 begins with identical chars: *)
	    fun issub offset = 
		offset <= stop2 andalso 
		(sub_unsafe(s1,stop1) = sub_unsafe(s2,stop1+offset) andalso cmp offset 
		 orelse issub (offset+1))
	in issub 0 
	end

    fun isPrefix s1 s2 = 
      let val n1 = size s1 
	  and n2 = size s2
	  val stop = if n1 < n2 then n1 else n2
	  fun h j = (* At this point s1[0..j-1] = s2[0..j-1] *)
	    j = stop orelse sub_unsafe(s1, j) = sub_unsafe(s2, j) andalso h (j+1)
      in n1 <= n2 andalso h 0 
      end
	
    fun compare (s1:string, s2) = 
      if s1 < s2 then LESS
      else if s1 > s2 then GREATER
	   else EQUAL

    fun collate cmp (s1, s2) =
      let val n1 = size s1 
	  and n2 = size s2
	  val stop = if n1 < n2 then n1 else n2
	  fun h j = (* At this point s1[0..j-1] = s2[0..j-1] *)
	    if j = stop then if      n1 < n2 then LESS
			     else if n1 > n2 then GREATER
				  else                 EQUAL
	    else case cmp(sub_unsafe(s1, j), sub_unsafe(s2, j))
		   of LESS    => LESS
		    | GREATER => GREATER
		    | EQUAL   => h (j+1)
      in h 0 
      end

    fun fromString s =
      let fun getc i = if i < size s then SOME (sub_unsafe(s, i), i+1) else NONE
	  fun h src res = case getc src
			    of NONE => SOME (implode(rev res))
			     | SOME(#"\\", src1) => 
			      (case StrBase.fromMLescape getc src1 
				 of NONE => NONE
				  | SOME(c, src2) => h src2 (c :: res))
			     | SOME(c, src1) => h src1 (c :: res)
      in h 0 []
      end
	
    fun toString s = StrBase.translate StrBase.toMLescape (s, 0, size s)
    fun fromCString s = StrBase.fromCString s
    fun toCString s = StrBase.translate StrBase.toCescape (s, 0, size s)

    val op <  = op <  : string * string -> bool
    val op <= = op <= : string * string -> bool
    val op >  = op >  : string * string -> bool
    val op >= = op >= : string * string -> bool

  end (*structure String*)

fun substring x = String.substring x
