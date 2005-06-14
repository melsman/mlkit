(* $Id$ *)

signature SCS_QA = 
  sig

    (* [mk_qa_box (query,title,desc)] returns a box with the results of the 
       qa check: if the query returns no rows, then (NONE,0) is returned,
       else (SOME q, num_of_errors) is returned  *)
    val mk_qa_box : quot * string * quot -> quot option * int

  end (* of signature *)


structure ScsQa :> SCS_QA =
  struct

  (* Functions to show the result of a query. This should be abstracted
     and implemented in another lib-file. Can also be used by the
     Query-package. *)
  fun f (s:Ns.Set.set,acc) = 
    let 
      val ls = List.rev (Ns.Set.list s)
    in
      (case acc of
	 NONE => 
	   SOME ((`<tr><td>` ^^ (Quot.concatWith "</td><td>" 
				 (List.map (Quot.fromString o #2) ls)) ^^ `</td></tr>^("\n")`) :: 
		 [`<tr bgcolor="#999999"><td class="headercell">` ^^ (Quot.concatWith (Quot.toString `</td><td class="headercell">`) 
				 (List.map (Quot.fromString o #1) ls)) ^^ `</td></tr>^("\n")`], 1)
       | SOME (acc,n) => 
	   SOME ((`<tr><td>` ^^ (Quot.concatWith "</td><td>" 
				 (List.map (Quot.fromString o #2) ls)) ^^ `</td></tr>^("\n")`) ::
		 acc,n+1)) 
    end

  fun do_query q =
    if q = "" then 
      ([`No query specified`],0)
    else
      (case Db.foldSet f NONE `^q` of
	 NONE => ([`No rows returned`],0)
       | SOME (res,n) => ((List.rev res),n))
	 handle Fail e => ([`Database error: returned error:<p>
			    <pre>^e</pre>`],1)


    fun mk_qa_box (q,title,desc) = 
      let
	val (r,n) = do_query (Quot.toString q)

	fun add_table_tags b = 
	  `<table width="100%" align="" bgcolor="white" cellpadding="2" 
		  cellspacing="0" border="0">` ^^ b ^^ `</table>`
	val r = add_table_tags (Quot.concat r)
	fun errors_dict num = [
	  (ScsLang.da, `Der er ^(Int.toString num) fejl:`),
	  (ScsLang.en, `There ^(if num>0 then "are" else "is") ^(Int.toString num) error^(if num>0 then "s" else ""):`)
	]
	val pre_text = desc ^^ `<p>^(ScsDict.s (errors_dict n))<p>`
	val box_opt =     
	  if n > 0 then
	    SOME ( UcsWidget.innerBox 
	      title
	      (pre_text ^^ r)
	    )
	  else NONE
      in
	(box_opt, n)
      end


  end (* of struct *)
