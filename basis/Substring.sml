(* Substring -- 1995-06-15, 1997-06-03 *)

structure Substring : SUBSTRING =    (* Depends on StrBase and String *)
  struct

    (* Primitives *)
    fun sub_unsafe (s:string,i:int) : Char.char = prim ("__bytetable_sub", (s,i))

    (* Body *)

    structure String = String
    type substring = string * int * int
    (* Invariant on values (s, i, n) of type substring:
     *                  0 <= i <= i+n <= size s, 
     * or equivalently, 0 <= i and 0 <= n and i+n <= size s.  
     *)

    fun base arg = arg
    
    fun string x = String.substring x
  
    fun extract (s, i, NONE) =
      if 0 <= i andalso i <= size s then (s, i, size s - i)
      else raise General.Subscript
      | extract (s, i, SOME n) =
	if 0 <= i andalso 0 <= n andalso n <= size s - i then (s, i, n)
	else raise General.Subscript

    fun substring (s, i, n) = extract(s, i, SOME n)
      
    fun all s = (s, 0, size s)

    fun getc (s, i, 0) = NONE
      | getc (s, i, n) = SOME(sub_unsafe(s,i), (s, i+1, n-1))

    fun first (s, i, n) = if n = 0 then NONE else SOME (sub_unsafe(s,i));

    fun isEmpty (s, i, n) = n=0

    fun triml k (s, i, n) = 
      if k < 0 then raise Subscript
      else if k > n then (s, i+n, 0) 
	   else (s, i+k, n-k)

    fun trimr k (s, i, n) = 
      if k < 0 then raise Subscript
      else if k > n then (s, i, 0) 
	   else (s, i, n-k)

    fun sub((s', i', n'), i) = 
      if i<0 orelse i >= n' then raise Subscript
      else sub_unsafe(s', (i'+i));

    fun size (_, _, n) = n

    fun slice ((s', i', n'), i, NONE) =
      if 0 <= i andalso i <= n' then (s', i'+i, n'-i)
      (* If the argument is valid, then so is the result:
       *  0 <= i' <= i'+i <= i'+i + (n'-i) = i'+n' <= size s' *)
      else raise Subscript
      | slice ((s', i', n'), i, SOME n) =    
	if 0 <= i andalso 0 <= n andalso i+n <= n' then (s', i'+i, n)
	(* If the argument is valid, then so is the result:
	 *  0 <= i' <= i'+i <= i'+i + n <= i'+n' <= size s' *)
	else raise Subscript

    fun splitAt ((s, i, n), k) =
      if k < 0 orelse k > n then raise Subscript
      else ((s, i, k), (s, i+k, n-k));

    fun concat strs =
      let val strs' = map string strs
      in String.concat strs'
      end

    fun compare ((s1, i1, n1), (s2, i2, n2)) =
      let val stop = if n1 < n2 then n1 else n2
	  fun h j = (* At this point (s1, i1, j) = (s2, i2, j) *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
			     else                 EQUAL
	    else
	      let val c1 = sub_unsafe(s1, i1+j)
		  val c2 = sub_unsafe(s2, i2+j)
	      in if c1 < c2 then LESS
		 else if c1 > c2 then GREATER
	         else h (j+1)
	      end
      in h 0 
      end

    fun isPrefix s1 (s2, i2, n2) =
      let val stop = if n2 < String.size s1 then n2 else String.size s1
	  fun h j = (* At this point (s1, 0, j) = (s2, i2, j) *)
	    j = stop orelse sub_unsafe(s1,j) = sub_unsafe(s2,i2+j) andalso h (j+1)
      in String.size s1 <= n2 andalso h 0 
      end

    fun collate cmp ((s1, i1, n1), (s2, i2, n2)) =
      let val stop = if n1 < n2 then n1 else n2
	  fun h j = (* At this point (s1, i1, j) = (s2, i2, j) *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                             else                 EQUAL
	    else
	      case cmp(sub_unsafe(s1,i1+j), sub_unsafe(s2,i2+j)) 
		of LESS => LESS
		 | GREATER => GREATER
		 | EQUAL => h (j+1)
      in h 0 
      end

    fun foldl f e sus = StrBase.foldl f e sus;

    fun foldr f e (s,i,n) = 
      let fun h j res = if j<i then res 
			else h (j-1) (f (sub_unsafe(s,j), res))
      in h (i+n-1) e 
      end

    fun explode (s, i, n) =
      let fun h j res = if j<i then res
			else h (j-1) (sub_unsafe(s,j) :: res)
      in h (i+n-1) [] 
      end

    fun app f ss = foldl (fn (x, _) => f x) () ss

    exception Span
    fun span ((s, i, n), (s', i', n')) = 
      if i > i'+n' orelse s<>s' then raise Span
      else (s, i, i'+n'-i)

    local open StrBase 
    in
      fun splitl x = StrBase.splitl x
      fun splitr x = StrBase.splitr x
      fun dropl x = StrBase.dropl x
      fun dropr x = StrBase.dropr x
      fun takel x = StrBase.takel x
      fun taker x = StrBase.taker x
      fun translate x = StrBase.translate x
      fun tokens x = StrBase.tokens x
      fun fields x = StrBase.fields x
    end

    (* An early test at the end of s may save much work if s begins with
     * many identical characters.  *)

    fun position "" (ss as (s', i, n)) = ((s', i, 0), ss)
      | position s (ss as (s', i, n)) =             
      let val len1 = String.size s - 1	(* will be >= 0 *)
	  fun eq j k = j >= len1 orelse 
	    sub_unsafe(s,j) = sub_unsafe(s',k) andalso eq (j+1) (k+1)
	  val stop = i+n-len1-1
	  fun cmp k = 
	    if k>stop then (ss, (s', i+n, 0))				(* failure *)
	    else if sub_unsafe(s,len1) = sub_unsafe(s',k+len1) andalso eq 0 k then
	      ((s', i, k-i), (s', k, n-(k-i)))		(* success *)
		 else cmp(k+1)
      in cmp i 
      end
	
    (* Above, (eq j k)  means that  (s,j,len1-j) = (s',k,len1-j), 
           so s[len1] = s'[k+len1] and (eq 0 k) implies s = (s', k, len).
       At successful termination, i <= k <= i+n-len, so 0 <= k-i <= n-len, 
       and therefore n >= n-(k-i) >= len >= 0.  It follows that 
       0 <= i <= i + (k-i) = k <= size s'     
           and 
       0 <= k <= k + n-(k-i) = n+i <= size s' (by (s', i, n) being valid),
       so the resulting substrings are valid.
    *)

  end
