signature SCS_LIST =
  sig
    val allDifferent : ('a * 'a -> bool) -> 'a list -> bool
  end

structure ScsList =
  struct
    (* Given a list of strings, checks that they are mutually different. *)
    fun allDifferent fn_eq ls =
      let
	fun check(c,ls1,ls2) = not (List.exists (fn_eq c) (ls1@ls2))
	fun check_all ([],ls2) = true
	  | check_all (x::xs,ls2) = check(x,xs,ls2) andalso check_all(xs,x::ls2)
      in
        check_all (ls,[])
      end
  end