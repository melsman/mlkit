(* Memo: File changed from original. If the flag kitstop is set to true,
         then we only print 25 lines and ends. Otherwise it runs forever. 
   2001-02-17, Niels 
*)


(* Experiment done 2001-02-17, Niels:

   HP OmniBook 4150 with 128MB ram and 350 MHz processor.
 
   All results are best of 3 runs.

   We measure memory usage using top -d 1 and reports the largest number in column SIZE.

   Moscow ML version 2.00 (June 2000):

      Compilation: mosmlc kitmolgard.sml generates file a.out

      Execution: time a.out

      Execution time: 75 sek. user time

      Memory usage: 1.080 Kb.

      Output:

        [13:50-all]# time a.out
        init_stateSim.createCreating instances...
        Initializing markings...
        Iter: 0 - Start
        Iter: 1 - (0,0,0,8)      counts/sekund= 12500     average= 12500
        Iter: 2 - (0,0,0,16)      counts/sekund= 12500     average= 12500
        Iter: 3 - (0,0,0,25)      counts/sekund= 11111     average= 12000
        Iter: 4 - (0,0,0,33)      counts/sekund= 12500     average= 12121
        Iter: 5 - (0,0,0,41)      counts/sekund= 12500     average= 12195
        Iter: 6 - (0,0,0,50)      counts/sekund= 11111     average= 12000
        Iter: 7 - (0,0,0,58)      counts/sekund= 12500     average= 12068
        Iter: 8 - (0,0,1,6)      counts/sekund= 12500     average= 12121
        Iter: 9 - (0,0,1,14)      counts/sekund= 12500     average= 12162
        Iter: 10 - (0,0,1,23)      counts/sekund= 11111     average= 12048
        75.000u 0.070s 1:23.08 90.3%	0+0k 0+0io 221pf+0w

   ML Kit version 4.0a, Feb 17, 2001 [X86 Backend]:

      Compilation: mlkit -gc -chat kitmolgard.sml generates file run

      Execution: time run -silent_gc

      Execution time: 51.5 sek user time

      Memory usage: 878 Kb.

      Output:

        [13:58-all]# time run -silent_gc
        init_stateSim.createCreating instances...
        Initializing markings...
        Iter: 0 - Start
        Iter: 1 - (0,0,0,6)      counts/sekund= 16666     average= 16666
        Iter: 2 - (0,0,0,12)      counts/sekund= 16666     average= 16666
        Iter: 3 - (0,0,0,17)      counts/sekund= 20000     average= 17647
        Iter: 4 - (0,0,0,23)      counts/sekund= 16666     average= 17391
        Iter: 5 - (0,0,0,29)      counts/sekund= 16666     average= 17241
        Iter: 6 - (0,0,0,35)      counts/sekund= 16666     average= 17142
        Iter: 7 - (0,0,0,40)      counts/sekund= 20000     average= 17500
        Iter: 8 - (0,0,0,46)      counts/sekund= 16666     average= 17391
        Iter: 9 - (0,0,0,52)      counts/sekund= 16666     average= 17307
        Iter: 10 - (0,0,0,57)      counts/sekund= 20000     average= 17543
        51.500u 0.000s 0:57.08 90.2%	0+0k 0+0io 296pf+0w        

  Fast test on SML/NJ shows they are much faster - but also uses lots of memory (approx 37Mb.)!
  Memory includes the compiled program, however.

  I did not make an executable image!

        [14:13-all]# sml
        Standard ML of New Jersey, Version 110.0.6, October 31, 1999 [CM; autoload enabled]
        - use "kitmolgard.sml";
        [opening kitmolgard.sml]
        init_stateSim.createCreating instances...
        Initializing markings...
        Iter: 0 - Start
        Iter: 1 - (0,0,0,2)      counts/sekund= 50000     average= 50000
        Iter: 2 - (0,0,0,4)      counts/sekund= 50000     average= 50000
        Iter: 3 - (0,0,0,7)      counts/sekund= 33333     average= 42857
        Iter: 4 - (0,0,0,9)      counts/sekund= 50000     average= 44444
        Iter: 5 - (0,0,0,11)      counts/sekund= 50000     average= 45454
        Iter: 6 - (0,0,0,13)      counts/sekund= 50000     average= 46153
        Iter: 7 - (0,0,0,15)      counts/sekund= 50000     average= 46666
        Iter: 8 - (0,0,0,18)      counts/sekund= 33333     average= 44444
        Iter: 9 - (0,0,0,20)      counts/sekund= 50000     average= 45000
        Iter: 10 - (0,0,0,22)      counts/sekund= 50000     average= 45454

  Executing on ML Kit without garbage collection also increases speed,
  but it memory leaks heavily (204 Mb.)!

  I think the program uses references at the top level which is very
  bad for region inference.

        [14:18-all]# time run
        init_stateSim.createCreating instances...
        Initializing markings...
        Iter: 0 - Start
        Iter: 1 - (0,0,0,3)      counts/sekund= 33333     average= 33333   
        Iter: 2 - (0,0,0,7)      counts/sekund= 25000     average= 28571    <--- already > 30Mb
        Iter: 3 - (0,0,0,10)      counts/sekund= 33333     average= 30000
        Iter: 4 - (0,0,0,18)      counts/sekund= 12500     average= 22222   <--- from now on swaps heavily
        Iter: 5 - (0,0,0,29)      counts/sekund= 9090     average= 17241
        Iter: 6 - (0,0,0,41)      counts/sekund= 8333     average= 14634
        Iter: 7 - (0,0,0,51)      counts/sekund= 10000     average= 13725
        Iter: 8 - (0,0,1,0)      counts/sekund= 11111     average= 13333
        Iter: 9 - (0,0,1,14)      counts/sekund= 7142     average= 12162
        Iter: 10 - (0,0,1,31)      counts/sekund= 5882     average= 10989
        39.310u 12.900s 1:31.63 56.9%	0+0k 0+0io 5566pf+28358w
        
*)

local 
  (* Added 2001-02-17, Niels *)
  local
    val kitnoiter = ref 0
    fun incr() = (kitnoiter := !kitnoiter + 1)
    val kitstop_flag = true
  in
    fun kitstop s =
      ((*print ("in kitstop(" ^ Int.toString(!kitnoiter) ^ ")\n");*)
       (*TextIO.output (TextIO.stdOut, "Iter: " ^ (Int.toString(!kitnoiter)) ^" - " ^ s);*)
       TextIO.output (TextIO.stdOut, "Iter: " ^ (Int.toString(!kitnoiter)) ^ "\n");
       TextIO.flushOut (TextIO.stdOut);
       (* Don't go higher than 5 if you are compiling without gc. approx 102Mb. *)
       if kitstop_flag andalso !kitnoiter = 5 then OS.Process.exit(OS.Process.success) else incr())
  end

  val useStream_0 =
	  fn s_105 => (raise (Fail "Compiler.Interact.useStream unavailable"))
      fun fromString_17 s_106 = NONE
      fun setHandler_2 _ = ()
      datatype Signal_0 = HANDLER_1 of ((int * int * int) -> int)
      val exportML_2 =
	  fn s_107 => (raise (Fail "SMLofNJ.exportML unavailable"))
      val openIn_2 = TextIO.openIn
      val closeIn_4 = TextIO.openIn
      val input_5 = TextIO.input
      val inputAll_4 = TextIO.inputAll
      val input1_4 = TextIO.input1
      val inputN_4 = TextIO.inputN
      val inputLine_2 = TextIO.inputLine
      val endOfStream_4 = TextIO.endOfStream
      val lookahead_3 = TextIO.lookahead
      val openOut_2 = TextIO.openOut
      val openAppend_2 = TextIO.openAppend
      val closeOut_4 = TextIO.closeOut
      val output_5 = TextIO.output
      val output1_4 = TextIO.output1
      val outputSubstr_2 = TextIO.outputSubstr
      val flushOut_4 = TextIO.flushOut
      val stdIn_2 = TextIO.stdIn
      val stdOut_2 = TextIO.stdOut
      val stdErr_2 = TextIO.stdErr
      val print_3 = TextIO.print
      val openString_1 =
	  fn s_108 => (raise (Fail "TextIO.openString unavailable"))
      val use_0 = fn f_226 => (raise (Fail "use unavailable"))
      val exnName_1 = fn e_27 => (raise (Fail "exnName unavailable>"))
      val substring_4 = String.substring
      val sub_28 = Word8.toInt o Word8Array.sub
      fun array_6 (n_204, c_100) =
	  (Word8Array.array (n_204, Word8.fromInt c_100))
      fun update_23 (a_87, i_304, v_39) =
	  (Word8Array.update (a_87, i_304, Word8.fromInt v_39))
      fun extract_9 (ba_0, s_109, n_205) =
	  let fun tab_5 i_305 = (Word8Array.sub (ba_0, i_305 + s_109))
	  in Byte.unpackString (Word8Array.tabulate (n_205, tab_5), 0, NONE)
	  end
      exception InternalError_0  of string
      fun response_0 (blist_0 : bool list,
		      ilist_0 : int list,
		      slist_0 : string list) =
	  (blist_0, ilist_0, slist_0)
      val NSBootstrap_0 =
	  ref
	  (fn (timetype_0 : string,
	       starttime_0 : string,
	       filename_0 : string) =>
	      ([] : bool list, [] : int list, [] : string list))
      val debug_1 = print
      fun operator_255 (r_65, i_306 : int) = (r_65 := ((! r_65) + i_306))
      val userRequest1_0 = ref 0
      val userRequest2_0 = ref 0
      fun setHandlerOpt_0 (NONE, _) = ()
	| setHandlerOpt_0 (SOME sg_0, handler_1) =
	  (setHandler_2 (sg_0, handler_1) ; ())
      val _ =
	  setHandlerOpt_0
	  (fromString_17 "USR1",
	   HANDLER_1
	   (fn (_, n_206, c_101) =>
	       (operator_255 (userRequest1_0, n_206) ; c_101)))
      val _ =
	  setHandlerOpt_0
	  (fromString_17 "USR2",
	   HANDLER_1
	   (fn (_, n_207, c_102) =>
	       (operator_255 (userRequest2_0, n_207) ; c_102)))
      fun clearUserRequest1_0 () = (userRequest1_0 := 0)
      fun clearUserRequest2_0 () = (userRequest2_0 := 0)
      fun getUserRequest1_0 () = (! userRequest1_0)
      fun getUserRequest2_0 () = (! userRequest2_0)
      val build_0 = exportML_2
      fun DSUI_SetStatusBarMessage_0 s_110 = (print (s_110 ^ "\n"))
      fun DSUI_GetUserYesOrNo_0 s_111 =
	  (print (s_111 ^ " (Yes/No)\n") ;
	   let val resp_0 = inputLine_2 stdIn_2
	   in case explode resp_0 of
		[] => true
	      | (#"Y" :: _) => true | (#"y" :: _) => true | _ => false
	   end)
      fun DSUI_UserAckMessage_0 s_112 = (print (s_112 ^ "\n"))
      exception EXUI_GetIntegerValue_0  of unit
      fun DSUI_GetIntegerValue_0 (arg_21 as {def = def_0, prompt = prompt_0})

      =
	  (print (prompt_0 ^ "\n") ;
	   case Int.fromString (inputLine_2 stdIn_2) of
	     (SOME x_173) => x_173
	   | NONE => (raise (EXUI_GetIntegerValue_0 ())))
      fun DSFile_NameDialog_0 (arg_22
			       as {okButtonLabel = okButtonLabel_0,
				   path = path_6,
				   prompt = prompt_1,
				   writeBox = writeBox_0}) =
	  (print (((prompt_1 ^ " relative to path ") ^ path_6) ^ "\n") ;
	   path_6 ^ (inputLine_2 stdIn_2))
      val use_string_0 : (string list -> unit) =
	  app (useStream_0 o openString_1)
      val use_string_1 : (string list -> unit) =
	  app (useStream_0 o openString_1)
      fun use_decl_0 (id_3, s_113) =
	  ((app (useStream_0 o openString_1)) s_113 ;
	   (id_3, "", [("", 0)]))
      fun is_decl_0 _ = NONE
      fun is_def_0 _ = true
      fun inc_1 r_66 = (r_66 := ((! r_66) + 1))
      fun dec_1 r_67 = (r_67 := ((! r_67) - 1))
      fun fold_30 noName_599 =
	  (fn noName_600 =>
	      (fn noName_601 =>
		  (case (noName_599, noName_600, noName_601) of
		     (f_227, l_94, b_67) => (((foldr f_227) b_67) l_94))))
      fun revfold_0 noName_602 =
	  (fn noName_603 =>
	      (fn noName_604 =>
		  (case (noName_602, noName_603, noName_604) of
		     (f_228, l_95, b_68) => (((foldl f_228) b_68) l_95))))
      fun charToWord_0 c_103 = (Word.fromInt (Char.ord c_103))
      fun hashChar_0 (c_104, h_7) =
	  ((((Word.<< (h_7, 0w5)) + h_7) + 0w720) + (charToWord_0 c_104))
      fun hashString_0 s_114 = (((CharVector.foldl hashChar_0) 0w0) s_114)
      datatype ('a, 'b) bucket_0 =
	       NIL_0 | B_1 of (word * 'a * 'b * ('a, 'b) bucket_0)
      fun index_1 (i_307, sz_32) =
	  (Word.toIntX (Word.andb (i_307, (Word.fromInt sz_32) - 0w1)))
      fun roundUp_0 n_208 =
	  let fun f_229 i_308 =
		  (if i_308 >= n_208 then i_308 else f_229 (i_308 * 2))
	  in f_229 32
	  end
      fun alloc_0 sizeHint_0 = (Array.array (roundUp_0 sizeHint_0, NIL_0))
      fun growTable_0 (table_0, newSz_0) =
	  let val newArr_0 = Array.array (newSz_0, NIL_0)
	      fun copy_11 NIL_0 = ()
		| copy_11 (B_1 (h_8, key_0, v_40, rest_37)) =
		  let val indx_1 = index_1 (h_8, newSz_0)
		  in (Array.update
		      (newArr_0,
		       indx_1,
		       B_1 (h_8, key_0, v_40, Array.sub (newArr_0, indx_1))) ;
		      copy_11 rest_37)
		  end
	  in ((Array.app copy_11) table_0 ; newArr_0)
	  end
      fun growTableIfNeeded_0 (table_1, nItems_0) =
	  let val arr_75 = ! table_1
	      val sz_33 = Array.length arr_75
	  in (nItems_0 >= sz_33)
	     andalso (table_1 := (growTable_0 (arr_75, sz_33 + sz_33)) ;
		      true)
	  end
      fun listItems_0 (table_2, nItems_1) =
	  let fun f_230 (_, l_96, 0) = l_96
		| f_230 (~1, l_97, _) = l_97
		| f_230 (i_309, l_98, n_209) =
		  let fun g_5 (NIL_0, l_99, n_210) =
			  (f_230 (i_309 - 1, l_99, n_210))
			| g_5 (B_1 (_, k_64, v_41, r_68), l_100, n_211) =
			  (g_5 (r_68, v_41 :: l_100, n_211 - 1))
		  in g_5 (Array.sub (table_2, i_309), l_98, n_209)
		  end
	  in f_230 ((Array.length table_2) - 1, [], ! nItems_1)
	  end
      fun listItemsi_0 (table_3, nItems_2) =
	  let fun f_231 (_, l_101, 0) = l_101
		| f_231 (~1, l_102, _) = l_102
		| f_231 (i_310, l_103, n_212) =
		  let fun g_6 (NIL_0, l_104, n_213) =
			  (f_231 (i_310 - 1, l_104, n_213))
			| g_6 (B_1 (_, k_65, v_42, r_69), l_105, n_214) =
			  (g_6 (r_69, (k_65, v_42) :: l_105, n_214 - 1))
		  in g_6 (Array.sub (table_3, i_310), l_103, n_212)
		  end
	  in f_231 ((Array.length table_3) - 1, [], ! nItems_2)
	  end
      fun appi_9 noName_605 =
	  (fn noName_606 =>
	      (case (noName_605, noName_606) of
		 (f_232, table_4) =>
		 let fun appF_0 NIL_0 = ()
		       | appF_0 (B_1 (_, key_1, item_4, rest_38)) =
			 (f_232 (key_1, item_4) ; appF_0 rest_38)
		 in (Array.app appF_0) table_4
		 end))
      fun app_28 noName_607 =
	  (fn noName_608 =>
	      (case (noName_607, noName_608) of
		 (f_233, table_5) =>
		 let fun appF_1 NIL_0 = ()
		       | appF_1 (B_1 (_, key_2, item_5, rest_39)) =
			 (f_233 item_5 ; appF_1 rest_39)
		 in (Array.app appF_1) table_5
		 end))
      fun mapi_4 noName_609 =
	  (fn noName_610 =>
	      (case (noName_609, noName_610) of
		 (f_234, table_6) =>
		 let fun mapF_0 NIL_0 = NIL_0
		       | mapF_0 (B_1 (hash_2, key_3, item_6, rest_40)) =
			 (B_1
			  (hash_2,
			   key_3,
			   f_234 (key_3, item_6),
			   mapF_0 rest_40))
		     val newTbl_0 =
			 Array.tabulate
			 (Array.length table_6,
			  fn i_311 => (mapF_0 (Array.sub (table_6, i_311))))
		 in newTbl_0
		 end))
      fun map_9 noName_611 =
	  (fn noName_612 =>
	      (case (noName_611, noName_612) of
		 (f_235, table_7) =>
		 let fun mapF_1 NIL_0 = NIL_0
		       | mapF_1 (B_1 (hash_3, key_4, item_7, rest_41)) =
			 (B_1 (hash_3, key_4, f_235 item_7, mapF_1 rest_41))
		     val newTbl_1 =
			 Array.tabulate
			 (Array.length table_7,
			  fn i_312 => (mapF_1 (Array.sub (table_7, i_312))))
		 in newTbl_1
		 end))
      fun foldi_1 noName_613 =
	  (fn noName_614 =>
	      (fn noName_615 =>
		  (case (noName_613, noName_614, noName_615) of
		     (f_236, init_47, table_8) =>
		     let fun foldF_0 (NIL_0, accum_37) = accum_37
			   | foldF_0 (B_1 (hash_4, key_5, item_8, rest_42),
				      accum_38) =
			     (foldF_0
			      (rest_42, f_236 (key_5, item_8, accum_38)))
		     in ((Array.foldl foldF_0) init_47) table_8
		     end)))
      fun fold_31 noName_616 =
	  (fn noName_617 =>
	      (fn noName_618 =>
		  (case (noName_616, noName_617, noName_618) of
		     (f_237, init_48, table_9) =>
		     let fun foldF_1 (NIL_0, accum_39) = accum_39
			   | foldF_1 (B_1 (hash_5, key_6, item_9, rest_43),
				      accum_40) =
			     (foldF_1 (rest_43, f_237 (item_9, accum_40)))
		     in ((Array.foldl foldF_1) init_48) table_9
		     end)))
      fun filteri_0 noName_619 =
	  (fn noName_620 =>
	      (case (noName_619, noName_620) of
		 (pred_17, table_10) =>
		 let fun filterP_0 NIL_0 = NIL_0
		       | filterP_0 (B_1 (hash_6, key_7, item_10, rest_44)) =
			 (if pred_17 (key_7, item_10)
			  then B_1
			       (hash_6, key_7, item_10, filterP_0 rest_44)
			  else filterP_0 rest_44)
		 in (Array.modify filterP_0) table_10
		 end))
      fun filter_3 noName_621 =
	  (fn noName_622 =>
	      (case (noName_621, noName_622) of
		 (pred_18, table_11) =>
		 let fun filterP_1 NIL_0 = NIL_0
		       | filterP_1 (B_1 (hash_7, key_8, item_11, rest_45)) =
			 (if pred_18 item_11
			  then B_1
			       (hash_7, key_8, item_11, filterP_1 rest_45)
			  else filterP_1 rest_45)
		 in (Array.modify filterP_1) table_11
		 end))
      fun copy_12 table_12 =
	  (Array.tabulate
	   (Array.length table_12,
	    fn i_313 => (Array.sub (table_12, i_313))))
      fun bucketSizes_0 table_13 =
	  let fun len_116 (NIL_0, n_215) = n_215
		| len_116 (B_1 (_, _, _, r_70), n_216) =
		  (len_116 (r_70, n_216 + 1))
	  in ((Array.foldr
	       (fn (b_69, l_106) => ((len_116 (b_69, 0)) :: l_106)))
	      [])
	     table_13
	  end
      datatype ('a, 'b) hash_table_0 =
	       HT_0 of {eq_pred : (('a * 'a) -> bool),
			hash_fn : ('a -> word),
			n_items : int ref,
			not_found : exn,
			table : ('a, 'b) bucket_0 array ref}
      fun index_2 (i_314, sz_34) =
	  (Word.toIntX (Word.andb (i_314, (Word.fromInt sz_34) - 0w1)))
      fun roundUp_1 n_217 =
	  let fun f_238 i_315 =
		  (if i_315 >= n_217 then i_315 else f_238 (i_315 * 2))
	  in f_238 32
	  end
      fun mkTable_0 noName_623 =
	  (fn noName_624 =>
	      (case (noName_623, noName_624) of
		 ((hash_8, eq_2), (sizeHint_1, notFound_0)) =>
		 (HT_0
		  {eq_pred = eq_2,
		   hash_fn = hash_8,
		   n_items = ref 0,
		   not_found = notFound_0,
		   table = ref (alloc_0 sizeHint_1)})))
      fun insert_4 noName_625 =
	  (fn noName_626 =>
	      (case (noName_625, noName_626) of
		 (tbl_1
		  as HT_0
		     {eq_pred = eq_pred_0,
		      hash_fn = hash_fn_0,
		      n_items = n_items_0,
		      table = table_14,...},
		  (key_9, item_12)) =>
		 let val arr_76 = ! table_14
		     val sz_35 = Array.length arr_76
		     val hash_9 = hash_fn_0 key_9
		     val indx_2 = index_2 (hash_9, sz_35)
		     fun look_1 NIL_0 =
			 (Array.update
			  (arr_76,
			   indx_2,
			   B_1
			   (hash_9,
			    key_9,
			    item_12,
			    Array.sub (arr_76, indx_2))) ;
			  n_items_0 := ((! n_items_0) + 1) ;
			  growTableIfNeeded_0 (table_14, ! n_items_0) ;
			  NIL_0)
		       | look_1 (B_1 (h_9, k_66, v_43, r_71)) =
			 (if (hash_9 = h_9)
			     andalso (eq_pred_0 (key_9, k_66))
			  then B_1 (hash_9, key_9, item_12, r_71)
			  else case look_1 r_71 of
				 NIL_0 => NIL_0
			       | rest_46 => (B_1 (h_9, k_66, v_43, rest_46)))
		 in case look_1 (Array.sub (arr_76, indx_2)) of
		      NIL_0 => ()
		    | b_70 => (Array.update (arr_76, indx_2, b_70))
		 end))
      fun lookup_0 noName_627 =
	  (fn noName_628 =>
	      (case (noName_627, noName_628) of
		 (HT_0
		  {eq_pred = eq_pred_1,
		   hash_fn = hash_fn_1,
		   not_found = not_found_0,
		   table = table_15,...},
		  key_10) =>
		 let val arr_77 = ! table_15
		     val sz_36 = Array.length arr_77
		     val hash_10 = hash_fn_1 key_10
		     val indx_3 = index_2 (hash_10, sz_36)
		     fun look_2 NIL_0 = (raise not_found_0)
		       | look_2 (B_1 (h_10, k_67, v_44, r_72)) =
			 (if (hash_10 = h_10)
			     andalso (eq_pred_1 (key_10, k_67))
			  then v_44
			  else look_2 r_72)
		 in look_2 (Array.sub (arr_77, indx_3))
		 end))
      fun find_3 noName_629 =
	  (fn noName_630 =>
	      (case (noName_629, noName_630) of
		 (HT_0
		  {eq_pred = eq_pred_2,
		   hash_fn = hash_fn_2,
		   table = table_16,...},
		  key_11) =>
		 let val arr_78 = ! table_16
		     val sz_37 = Array.length arr_78
		     val hash_11 = hash_fn_2 key_11
		     val indx_4 = index_2 (hash_11, sz_37)
		     fun look_3 NIL_0 = NONE
		       | look_3 (B_1 (h_11, k_68, v_45, r_73)) =
			 (if (hash_11 = h_11)
			     andalso (eq_pred_2 (key_11, k_68))
			  then SOME v_45
			  else look_3 r_73)
		 in look_3 (Array.sub (arr_78, indx_4))
		 end))
      fun remove_2 noName_631 =
	  (fn noName_632 =>
	      (case (noName_631, noName_632) of
		 (HT_0
		  {eq_pred = eq_pred_3,
		   hash_fn = hash_fn_3,
		   n_items = n_items_1,
		   not_found = not_found_1,
		   table = table_17},
		  key_12) =>
		 let val arr_79 = ! table_17
		     val sz_38 = Array.length arr_79
		     val hash_12 = hash_fn_3 key_12
		     val indx_5 = index_2 (hash_12, sz_38)
		     fun look_4 NIL_0 = (raise not_found_1)
		       | look_4 (B_1 (h_12, k_69, v_46, r_74)) =
			 (if (hash_12 = h_12)
			     andalso (eq_pred_3 (key_12, k_69))
			  then (v_46, r_74)
			  else let val (item_13, r'_1) = look_4 r_74
			       in (item_13, B_1 (h_12, k_69, v_46, r'_1))
			       end)
		     val (item_14, bucket_0) =
			 look_4 (Array.sub (arr_79, indx_5))
		 in (Array.update (arr_79, indx_5, bucket_0) ;
		     n_items_1 := ((! n_items_1) - 1) ;
		     item_14)
		 end))
      fun numItems_0 (HT_0 {n_items = n_items_2,...}) = (! n_items_2)
      fun listItems_1 (HT_0 {n_items = n_items_3, table = ref arr_80,...}) =
	  (listItems_0 (arr_80, n_items_3))
      fun listItemsi_1 (HT_0 {n_items = n_items_4, table = ref arr_81,...}) =
	  (listItemsi_0 (arr_81, n_items_4))
      fun appi_10 noName_633 =
	  (fn noName_634 =>
	      (case (noName_633, noName_634) of
		 (f_239, HT_0 {table = table_18,...}) =>
		 ((appi_9 f_239) (! table_18))))
      fun app_29 noName_635 =
	  (fn noName_636 =>
	      (case (noName_635, noName_636) of
		 (f_240, HT_0 {table = table_19,...}) =>
		 ((app_28 f_240) (! table_19))))
      fun mapi_5 noName_637 =
	  (fn noName_638 =>
	      (case (noName_637, noName_638) of
		 (f_241,
		  HT_0
		  {eq_pred = eq_pred_4,
		   hash_fn = hash_fn_4,
		   n_items = n_items_5,
		   not_found = not_found_2,
		   table = table_20}) =>
		 (HT_0
		  {eq_pred = eq_pred_4,
		   hash_fn = hash_fn_4,
		   n_items = ref (! n_items_5),
		   not_found = not_found_2,
		   table = ref ((mapi_4 f_241) (! table_20))})))
      fun map_10 noName_639 =
	  (fn noName_640 =>
	      (case (noName_639, noName_640) of
		 (f_242,
		  HT_0
		  {eq_pred = eq_pred_5,
		   hash_fn = hash_fn_5,
		   n_items = n_items_6,
		   not_found = not_found_3,
		   table = table_21}) =>
		 (HT_0
		  {eq_pred = eq_pred_5,
		   hash_fn = hash_fn_5,
		   n_items = ref (! n_items_6),
		   not_found = not_found_3,
		   table = ref ((map_9 f_242) (! table_21))})))
      fun foldi_2 noName_641 =
	  (fn noName_642 =>
	      (fn noName_643 =>
		  (case (noName_641, noName_642, noName_643) of
		     (f_243, init_49, HT_0 {table = table_22,...}) =>
		     (((foldi_1 f_243) init_49) (! table_22)))))
      fun fold_32 noName_644 =
	  (fn noName_645 =>
	      (fn noName_646 =>
		  (case (noName_644, noName_645, noName_646) of
		     (f_244, init_50, HT_0 {table = table_23,...}) =>
		     (((fold_31 f_244) init_50) (! table_23)))))
      fun filteri_1 noName_647 =
	  (fn noName_648 =>
	      (case (noName_647, noName_648) of
		 (pred_19, HT_0 {table = table_24,...}) =>
		 ((filteri_0 pred_19) (! table_24))))
      fun filter_4 noName_649 =
	  (fn noName_650 =>
	      (case (noName_649, noName_650) of
		 (pred_20, HT_0 {table = table_25,...}) =>
		 ((filter_3 pred_20) (! table_25))))
      fun copy_13 (HT_0
		   {eq_pred = eq_pred_6,
		    hash_fn = hash_fn_6,
		    n_items = n_items_7,
		    not_found = not_found_4,
		    table = table_26}) =
	  (HT_0
	   {eq_pred = eq_pred_6,
	    hash_fn = hash_fn_6,
	    n_items = ref (! n_items_7),
	    not_found = not_found_4,
	    table = ref (copy_12 (! table_26))})
      fun bucketSizes_1 (HT_0 {table = table_27,...}) =
	  (bucketSizes_0 (! table_27))
      fun pow_1 (i_316, j_58) =
	  let fun pow'_0 (_, 0) = 1
		| pow'_0 (i_317, 1) = i_317
		| pow'_0 (i_318, j_59) =
		  (if (Int.mod (j_59, 2)) = 0
		   then pow'_0 (Int.* (i_318, i_318), Int.div (j_59, 2))
		   else Int.*
			(i_318,
			 pow'_0 (Int.* (i_318, i_318), Int.div (j_59, 2))))
	  in if Int.<= (0, j_58)
	     then pow'_0 (i_316, j_58)
	     else raise Domain
	  end
      fun log2_0 i_319 =
	  let fun log2'_0 (x_174, n_218) =
		  (if Int.< (i_319, x_174)
		   then n_218
		   else log2'_0 (Int.* (2, x_174), Int.+ (n_218, 1)))
	  in if Int.< (0, i_319) then log2'_0 (2, 0) else raise Domain
	  end
      fun divMod_0 (i_320, j_60) =
	  (Int.div (i_320, j_60), Int.mod (i_320, j_60))
      fun quotRem_0 (i_321, j_61) =
	  ((i_321 Int.quot) j_61, (i_321 Int.rem) j_61)
      val base_16 = ""
      fun eq_3 (a_88, b_71 : string) = (a_88 = b_71)
      val hash_13 = hashString_0
      fun toString_19 s_115 = (concat ["\"", s_115, "\""])
      fun makeid_0 s_116 = s_116
      exception Empty_1 
      exception Subtract_0 
      exception BindFailure_0 
      exception BindFatalFailure_0 
      exception CPN'Error_0  of string
      exception CPN'Cancel_0  of string
      exception CPN'Stop_0  of string
      val CPN'use_0 = use_0
      val CPN'hd_0 = hd
      val CPN'tl_0 = tl
      val CPN'nth_0 = List.nth
      val CPN'app_0 = app
      val CPN'map_0 = map
      val CPN'fold_0 = fold_30
      val CPN'concat_0 = concat
      val CPN'inst_0 = ref 1
      val pims_name_0 = ref "CPN'MakeTreeListPIMS"
      val sims_name_0 = ref "CPN'MakeTreeSIMS"
      val use_pause_0 = ref false
      val use_manbind_0 = ref false
      val use_legal_check_0 = ref false
      val use_report_binds_0 = ref false
      val use_record_symbols_0 = ref true
      val use_cast_0 = ref false
      val small_enum_size_0 = ref 4
      val bindable_cs_size_0 = ref 100
      val use_dmo_0 = ref true
      fun CPN'dump_file_name_0 txt_0 = (("/tmp/CPNML-" ^ txt_0) ^ ".sml")
      val CPN'dump_file_0 = ref true
      val CPN'dump_stream_0 = ref (NONE : TextIO.outstream option)
      val CPN'debug_0 = debug_1
      fun CPN'debug_exn_0 noName_651 =
	  (fn noName_652 =>
	      (case (noName_651, noName_652) of
		 (exn_5, msg_2) =>
		 ((raise exn_5)
		  handle (InternalError_0 str_2) =>
			 (CPN'debug_0
			  ((("Exception InternalError \"" ^ str_2)
			    ^ "\" is raised ")
			   ^ msg_2))
		       | _ =>
			 (CPN'debug_0
			  ((("Exception " ^ (exnName_1 exn_5))
			    ^ " is raised ")
			   ^ msg_2)) ;
		  raise exn_5)))
      val timer_0 = ref (NONE : Timer.cpu_timer option)
      val stream_1 = ref (NONE : TextIO.outstream option)
      fun CPN'start_timing_0 name_45 =
	  (case ! timer_0 of
	     NONE =>
	     (timer_0 := (SOME (Timer.startCPUTimer ())) ;
	      stream_1 := (SOME (openOut_2 ("/tmp/" ^ name_45))))
	   | _ => ())
      fun CPN'report_timing_0 msg_3 =
	  (case ! timer_0 of
	     (SOME t_23) =>
	     let val {gc = gc_1, sys = sys_1, usr = usr_1} =
		     Timer.checkCPUTimer t_23
		 val total_0 = Time.+ (usr_1, Time.+ (gc_1, sys_1))
		 val text_0 =
		     concat
		     [msg_3,
		      "\t",
		      Time.toString total_0,
		      "=",
		      Time.toString usr_1,
		      "+",
		      Time.toString gc_1,
		      "+",
		      Time.toString sys_1,
		      "\n"]
	     in (case ! stream_1 of
		   NONE => ()
		 | (SOME s_117) =>
		   (output_5 (s_117, text_0) ; flushOut_4 s_117) ;
		 text_0)
	     end
	   | _ => "Timing Disabled")
      fun CPN'stop_timing_0 () =
	  (case ! stream_1 of
	     (SOME s_118) => (closeOut_4 s_118 ; stream_1 := NONE)
	   | NONE => () ;
	   timer_0 := NONE)
      fun a_cmp_0 noName_653 =
	  (fn noName_654 =>
	      (case (noName_653, noName_654) of
		 (lt_0, (a_89, b_72)) =>
		 (if lt_0 (a_89, b_72)
		  then LESS
		  else if lt_0 (b_72, a_89) then GREATER else EQUAL)))
      fun operator_256 (r_75, x_175 : int) = (r_75 := ((! r_75) + x_175))
      fun operator_257 (r_76, x_176 : int) = (r_76 := ((! r_76) - x_176))
      fun operator_258 (r_77, x_177 : int) = (r_77 := ((! r_77) * x_177))
      fun fold_33 noName_655 =
	  (fn noName_656 =>
	      (fn noName_657 =>
		  (case (noName_655, noName_656, noName_657) of
		     (f_245, (m_48, n_219), tail_6) =>
		     let fun fold'_0 (i_322, tail_7) =
			     (if i_322 < n_219
			      then f_245
				   (i_322, fold'_0 (i_322 + 1, tail_7))
			      else tail_7)
		     in fold'_0 (m_48, tail_6)
		     end)))
      fun revfold_1 noName_658 =
	  (fn noName_659 =>
	      (fn noName_660 =>
		  (case (noName_658, noName_659, noName_660) of
		     (f_246, (m_49, n_220), tail_8) =>
		     let fun revfold'_0 (i_323, tail_9) =
			     (if m_49 <= i_323
			      then f_246
				   (i_323, revfold'_0 (i_323 - 1, tail_9))
			      else tail_9)
		     in revfold'_0 (n_220 - 1, tail_8)
		     end)))
      fun app_30 noName_661 =
	  (fn noName_662 =>
	      (case (noName_661, noName_662) of
		 (f_247, (m_50, n_221)) =>
		 let fun app'_0 i_324 =
			 (if i_324 < n_221
			  then (f_247 i_324 ; app'_0 (i_324 + 1))
			  else ())
		 in app'_0 m_50
		 end))
      fun revapp_0 noName_663 =
	  (fn noName_664 =>
	      (case (noName_663, noName_664) of
		 (f_248, (m_51, n_222)) =>
		 let fun app'_1 i_325 =
			 (if m_51 <= i_325
			  then (f_248 i_325 ; app'_1 (i_325 - 1))
			  else ())
		 in app'_1 (n_222 - 1)
		 end))
      fun operator_259 (r_78, x_178 : real) = (r_78 := ((! r_78) + x_178))
      fun operator_260 (r_79, x_179 : real) = (r_79 := ((! r_79) - x_179))
      fun operator_261 (r_80, x_180 : real) = (r_80 := ((! r_80) * x_180))
      fun left_0 (s_119, i_326) = (substring_4 (s_119, 0, i_326))
      fun right_0 (s_120, i_327) =
	  let val n_223 = size s_120
	  in substring_4 (s_120, n_223 - i_327, i_327)
	  end
      fun makelist_0 noName_665 =
	  (fn noName_666 =>
	      (case (noName_665, noName_666) of
		 (mkstr_0, list_6) =>
		 (concat
		  ("["
		   :: (tl
		       (((fold_30
			  (fn (a_90, b_73) =>
			      ("," :: ((mkstr_0 a_90) :: b_73))))
			 list_6)
			["", "]"]))))))
      fun map_11 noName_667 =
	  (fn noName_668 =>
	      (case (noName_667, noName_668) of
		 (f_249, a_91) =>
		 let val n_224 = Array.length a_91
		     fun map'_0 i_328 =
			 (if i_328 < n_224
			  then (Array.update
				(a_91,
				 i_328,
				 f_249 (Array.sub (a_91, i_328))) ;
				map'_0 (i_328 + 1))
			  else ())
		 in map'_0 0
		 end))
      fun fold_34 noName_669 =
	  (fn noName_670 =>
	      (fn noName_671 =>
		  (case (noName_669, noName_670, noName_671) of
		     (f_250, a_92, tail_10) =>
		     let val n_225 = Array.length a_92
			 fun fold'_1 (i_329, tail_11) =
			     (if i_329 < n_225
			      then f_250
				   (Array.sub (a_92, i_329),
				    fold'_1 (i_329 + 1, tail_11))
			      else tail_11)
		     in fold'_1 (0, tail_10)
		     end)))
      fun revfold_2 noName_672 =
	  (fn noName_673 =>
	      (fn noName_674 =>
		  (case (noName_672, noName_673, noName_674) of
		     (f_251, a_93, tail_12) =>
		     let val n_226 = Array.length a_93
			 fun revfold'_1 (i_330, tail_13) =
			     (if 0 <= i_330
			      then f_251
				   (Array.sub (a_93, i_330),
				    revfold'_1 (i_330 - 1, tail_13))
			      else tail_13)
		     in revfold'_1 (n_226 - 1, tail_12)
		     end)))
      fun app_31 noName_675 =
	  (fn noName_676 =>
	      (case (noName_675, noName_676) of
		 (f_252, a_94) =>
		 let val n_227 = Array.length a_94
		     fun app'_2 i_331 =
			 (if i_331 < n_227
			  then (f_252 (Array.sub (a_94, i_331)) ;
				app'_2 (i_331 + 1))
			  else ())
		 in app'_2 0
		 end))
      fun revapp_1 noName_677 =
	  (fn noName_678 =>
	      (case (noName_677, noName_678) of
		 (f_253, a_95) =>
		 let fun revapp'_0 0 = ()
		       | revapp'_0 i_332 =
			 (f_253 (Array.sub (a_95, i_332 - 1)) ;
			  revapp'_0 (i_332 - 1))
		 in revapp'_0 (Array.length a_95)
		 end))
      fun filter_5 noName_679 =
	  (fn noName_680 =>
	      (case (noName_679, noName_680) of
		 (f_254, l_107) =>
		 (((fold_30
		    (fn (x_181, xs_2) =>
			(if f_254 x_181 then x_181 :: xs_2 else xs_2)))
		   l_107)
		  [])))
      fun flatten_0 [] = []
	| flatten_0 [xs_3] = xs_3
	| flatten_0 ((x_182 :: xs_4) :: XS_0) =
	  (x_182 :: (flatten_0 (xs_4 :: XS_0)))
	| flatten_0 ([] :: XS_1) = (flatten_0 XS_1)
      fun operator_262 (r_81, x_183) = (r_81 := (x_183 :: (! r_81)))
      fun sort_1 noName_681 =
	  (fn noName_682 =>
	      (case (noName_681, noName_682) of
		 (lt_1, list_7) =>
		 let fun merge_0 (x_184 :: xs_5, y_37 :: ys_2) =
			 (if lt_1 (x_184, y_37)
			  then x_184 :: (merge_0 (xs_5, y_37 :: ys_2))
			  else y_37 :: (merge_0 (x_184 :: xs_5, ys_2)))
		       | merge_0 ([], ys_3) = ys_3
		       | merge_0 (xs_6, []) = xs_6
		     fun mergepairs_0 (l1_8 :: (l2_9 :: ls_0), k_70) =
			 (if (Int.mod (k_70, 2)) = 1
			  then l1_8 :: (l2_9 :: ls_0)
			  else mergepairs_0
			       ((merge_0 (l1_8, l2_9)) :: ls_0, k_70 * k_70)

     )
		       | mergepairs_0 (ls_1, _) = ls_1
		     fun nextrun_0 (run_0, x_185 :: xs_7) =
			 (if lt_1 (hd run_0, x_185)
			  then nextrun_0 (x_185 :: run_0, xs_7)
			  else (rev run_0, x_185 :: xs_7))
		       | nextrun_0 (run_1, []) = (rev run_1, [])
		     fun samsorting_0 (x_186 :: xs_8, ls_2, k_71) =
			 let val (run_2, tail_14) =
				 nextrun_0 ([x_186], xs_8)
			 in samsorting_0
			    (tail_14,
			     mergepairs_0 (run_2 :: ls_2, k_71 + 1),
			     k_71 + 1)
			 end
		       | samsorting_0 ([], ls_3, _) =
			 ((hd (mergepairs_0 (ls_3, 0))) handle _ => [])
		 in samsorting_0 (list_7, [], 0)
		 end))
      fun unique_sort_0 noName_683 =
	  (fn noName_684 =>
	      (case (noName_683, noName_684) of
		 (lt_2, list_8) =>
		 let fun merge_1 (x_187 :: xs_9, y_38 :: ys_4) =
			 (if lt_2 (x_187, y_38)
			  then x_187 :: (merge_1 (xs_9, y_38 :: ys_4))
			  else if lt_2 (y_38, x_187)
			       then y_38 :: (merge_1 (x_187 :: xs_9, ys_4))
			       else x_187 :: (merge_1 (xs_9, ys_4)))
		       | merge_1 ([], ys_5) = ys_5
		       | merge_1 (xs_10, []) = xs_10
		     fun mergepairs_1 (l1_9 :: (l2_10 :: ls_4), k_72) =
			 (if (Int.mod (k_72, 2)) = 1
			  then l1_9 :: (l2_10 :: ls_4)
			  else mergepairs_1
			       ((merge_1 (l1_9, l2_10)) :: ls_4,
				k_72 * k_72))
		       | mergepairs_1 (ls_5, _) = ls_5
		     fun nextrun_1 (run_3, x_188 :: xs_11) =
			 (if lt_2 (hd run_3, x_188)
			  then nextrun_1 (x_188 :: run_3, xs_11)
			  else (rev run_3, x_188 :: xs_11))
		       | nextrun_1 (run_4, []) = (rev run_4, [])
		     fun samsorting_1 (x_189 :: xs_12, ls_6, k_73) =
			 let val (run_5, tail_15) =
				 nextrun_1 ([x_189], xs_12)
			 in samsorting_1
			    (tail_15,
			     mergepairs_1 (run_5 :: ls_6, k_73 + 1),
			     k_73 + 1)
			 end
		       | samsorting_1 ([], ls_7, _) =
			 ((hd (mergepairs_1 (ls_7, 0))) handle _ => [])
		 in samsorting_1 (list_8, [], 0)
		 end))
      fun time_of_day_0 () = (Time.toReal (Time.now ()))
      fun basename_0 xs_13 =
	  let fun get_5 ([#"/"], ys_6) = (rev ys_6)
		| get_5 ([], ys_7) = (rev ys_7)
		| get_5 (#"/" :: xs_14, _) = (get_5 (xs_14, []))
		| get_5 (x_190 :: xs_15, ys_8) =
		  (get_5 (xs_15, x_190 :: ys_8))
	  in implode (get_5 (explode xs_13, []))
	  end
      val no_0 = ref 0
      fun next_7 () = ((! no_0) before (inc_1 no_0))
      datatype 'a tree_0 =
	       TreeNil_0
	     | TreeNode_0 of {left : 'a tree_0,
			      right : 'a tree_0,
			      size : int,
			      value : 'a}
      exception EmptyTree_0 
      fun size_9 TreeNil_0 = 0
	| size_9 (TreeNode_0 {size = size_10,...}) = size_10
      fun lson_0 (TreeNode_0 {left = left_1,...}) = left_1
	| lson_0 TreeNil_0 = (raise EmptyTree_0)
      fun rson_0 (TreeNode_0 {right = right_1,...}) = right_1
	| rson_0 TreeNil_0 = (raise EmptyTree_0)
      fun sons_0 (TreeNode_0 {left = left_2, right = right_2,...}) =
	  (left_2, right_2)
	| sons_0 _ = (raise EmptyTree_0)
      fun new_5 (v_47, l_108, r_82) =
	  (TreeNode_0
	   {left = l_108,
	    right = r_82,
	    size = (1 + (size_9 l_108)) + (size_9 r_82),
	    value = v_47})
      fun single_rotate_ccw_0 (x_191,
			       s1_15,
			       TreeNode_0
			       {left = s2_15, right = s3_0, value = y_39,...}

     ) =
	  (new_5 (y_39, new_5 (x_191, s1_15, s2_15), s3_0))
	| single_rotate_ccw_0 _ = (raise Match)
      fun single_rotate_cw_0 (y_40,
			      TreeNode_0
			      {left = s1_16, right = s2_16, value = x_192,...

     },
			      s3_1) =
	  (new_5 (x_192, s1_16, new_5 (y_40, s2_16, s3_1)))
	| single_rotate_cw_0 _ = (raise Match)
      fun double_rotate_ccw_0 (x_193,
			       s1_17,
			       TreeNode_0
			       {left =
				TreeNode_0
				{left = s2_17, right = s3_2, value = y_41,...

     },
				right = s4_0,
				value = z_11,...}) =
	  (new_5
	   (y_41, new_5 (x_193, s1_17, s2_17), new_5 (z_11, s3_2, s4_0)))
	| double_rotate_ccw_0 _ = (raise Match)
      fun double_rotate_cw_0 (z_12,
			      TreeNode_0
			      {left = s1_18,
			       right =
			       TreeNode_0
			       {left = s2_18, right = s3_3, value = y_42,...}

     ,
			       value = x_194,...},
			      s4_1) =
	  (new_5
	   (y_42, new_5 (x_194, s1_18, s2_18), new_5 (z_12, s3_3, s4_1)))
	| double_rotate_cw_0 _ = (raise Match)
      val ratio_0 = 5
      fun singleton_0 item_15 =
	  (TreeNode_0
	   {left = TreeNil_0, right = TreeNil_0, size = 1, value = item_15})
      fun balance_0 (p_33 as (v_48, l_109, r_83)) =
	  let val ln_2 = size_9 l_109
	      val rn_0 = size_9 r_83
	  in if (ln_2 + rn_0) < 2
	     then new_5 p_33
	     else if rn_0 > (ratio_0 * ln_2)
		  then let val (rl_0, rr_0) = sons_0 r_83
		       in if (size_9 rl_0) < (size_9 rr_0)
			  then single_rotate_ccw_0 p_33
			  else double_rotate_ccw_0 p_33
		       end
		  else if ln_2 > (ratio_0 * rn_0)
		       then let val (ll_0, lr_0) = sons_0 l_109
			    in if (size_9 lr_0) < (size_9 ll_0)
			       then single_rotate_cw_0 p_33
			       else double_rotate_cw_0 p_33
			    end
		       else new_5 p_33
	  end
      fun min_9 (TreeNode_0 {left = TreeNil_0, value = value_1,...}) =
	  value_1
	| min_9 (TreeNode_0 {left = left_3,...}) = (min_9 left_3)
	| min_9 TreeNil_0 = (raise EmptyTree_0)
      fun max_8 (TreeNode_0 {right = TreeNil_0, value = value_2,...}) =
	  value_2
	| max_8 (TreeNode_0 {right = right_3,...}) = (max_8 right_3)
	| max_8 TreeNil_0 = (raise EmptyTree_0)
      fun delmin_0 (TreeNode_0
		    {left = TreeNil_0, right = right_4, value = value_3,...})

      =
	  (value_3, right_4)
	| delmin_0 (TreeNode_0
		    {left = left_4, right = right_5, value = value_4,...}) =
	  let val (value'_0, left'_0) = delmin_0 left_4
	  in (value'_0, balance_0 (value_4, left'_0, right_5))
	  end
	| delmin_0 _ = (raise EmptyTree_0)
      fun delmax_0 (TreeNode_0
		    {left = left_5, right = TreeNil_0, value = value_5,...}) =
	  (value_5, left_5)
	| delmax_0 (TreeNode_0
		    {left = left_6, right = right_6, value = value_6,...}) =
	  let val (value'_1, right'_0) = delmax_0 right_6
	  in (value'_1, balance_0 (value_6, left_6, right'_0))
	  end
	| delmax_0 _ = (raise EmptyTree_0)
      fun join_5 (TreeNil_0, right_7) = right_7
	| join_5 (left_7, TreeNil_0) = left_7
	| join_5 (left_8, right_8) =
	  (if (size_9 left_8) < (size_9 right_8)
	   then let val (v_49, r_84) = delmin_0 right_8
		in balance_0 (v_49, left_8, r_84)
		end
	   else let val (v_50, l_110) = delmax_0 left_8
		in balance_0 (v_50, l_110, right_8)
		end)
      fun fold_35 noName_685 =
	  (fn noName_686 =>
	      (fn noName_687 =>
		  (case (noName_685, noName_686, noName_687) of
		     (_, TreeNil_0, base_17) => base_17
		   | (f_255,
		      TreeNode_0
		      {left = left_9, right = right_9, value = value_7,...},
		      base_18) =>
		     (((fold_35 f_255) left_9)
		      (f_255 (value_7, ((fold_35 f_255) right_9) base_18))))

     ))
      fun revfold_3 noName_688 =
	  (fn noName_689 =>
	      (fn noName_690 =>
		  (case (noName_688, noName_689, noName_690) of
		     (_, TreeNil_0, base_19) => base_19
		   | (f_256,
		      TreeNode_0
		      {left = left_10, right = right_10, value = value_8,...}

     ,
		      base_20) =>
		     (((revfold_3 f_256) right_10)
		      (f_256 (value_8, ((revfold_3 f_256) left_10) base_20))

     ))))
      fun app_32 noName_691 =
	  (fn noName_692 =>
	      (case (noName_691, noName_692) of
		 (_, TreeNil_0) => ()
	       | (f_257,
		  TreeNode_0
		  {left = left_11, right = right_11, value = value_9,...}) =>
		 ((app_32 f_257) left_11 ;
		  f_257 value_9 ;
		  (app_32 f_257) right_11)))
      fun revapp_2 noName_693 =
	  (fn noName_694 =>
	      (case (noName_693, noName_694) of
		 (_, TreeNil_0) => ()
	       | (f_258,
		  TreeNode_0
		  {left = left_12, right = right_12, value = value_10,...}) =>
		 ((revapp_2 f_258) right_12 ;
		  f_258 value_10 ;
		  (revapp_2 f_258) left_12)))
      val a_96 = 16807.0
      val m_52 = 2147483647.0
      val seed_ref_0 = ref 123.0
      fun real_4 n_228 =
	  let val t_24 = a_96 * (! seed_ref_0)
	  in (seed_ref_0
	      := (t_24 - (m_52 * (Real.fromInt (floor (t_24 / m_52))))) ;
	      ((! seed_ref_0) / m_52) * n_228)
	  end
      fun int_1 n_229 = (trunc (real_4 (Real.fromInt n_229)))
      fun init_51 NONE = (seed_ref_0 := (time_of_day_0 ()))
	| init_51 (SOME seed_0) =
	  (seed_ref_0 := (Real.fromInt seed_0) ; real_4 1.0 ; ())
      val operator_263 = op @
      datatype 'a timed_0 = operator_264 of ('a * unit)
      val start_time_0 = NONE
      val model_time_0 = ref ()
      val null_2 = ()
      val null_str_0 = ""
      fun mkstr_1 _ = ""
      fun maketime_0 _ = ()
      fun ready_0 (t_25 : unit) = true
      fun time_2 () = (! model_time_0)
      val add_1 = fn _ => ()
      val sub_29 = fn _ => ()
      val lt_3 = fn _ => false
      val leq_0 = fn _ => true
      val cmp_4 = fn _ => EQUAL
      fun col_1 (operator_264 (c_105, _)) = c_105
      exception IOError_0  of string
      val white_spaces_0 = ref [#" ", #"\t", #"\n", #"\013"]
      val comment_char_0 = ref #"%"
      fun is_comment_char_0 ch_0 = (ch_0 = (! comment_char_0))
      fun is_white_space_0 ch_1 =
	  ((List.exists (fn x_195 => (x_195 = ch_1))) (! white_spaces_0))
      fun is_eof_0 s_121 = ((lookahead_3 s_121) = NONE)
      fun skip_line_0 s_122 =
	  (case input1_4 s_122 of
	     NONE => () | (SOME #"\n") => () | _ => (skip_line_0 s_122))
      fun skip_white_spaces_0 s_123 =
	  (case lookahead_3 s_123 of
	     (SOME chr_5) =>
	     (if is_white_space_0 chr_5
	      then (input1_4 s_123 ; skip_white_spaces_0 s_123)
	      else if is_comment_char_0 chr_5 then skip_line_0 s_123 else ())
	   | _ => ())
      fun get_until_0 (s_124, stop_chars_0) =
	  let fun get_string_0 noName_695 =
		  (fn noName_696 =>
		      (case (noName_695, noName_696) of
			 (cont_1, s_125) =>
			 (case input1_4 s_125 of
			    NONE =>
			    (raise (IOError_0 "Unexpected end of stream"))
			  | (SOME #"\\") =>
			    (#"\\"
			     :: ((valOf (input1_4 s_125))
				 :: ((get_string_0 cont_1) s_125)))
			  | (SOME #"\"") => (#"\"" :: (cont_1 s_125))
			  | (SOME ch_2) =>
			    (ch_2 :: ((get_string_0 cont_1) s_125)))))
	      fun get_6 noName_697 =
		  (fn noName_698 =>
		      (case (noName_697, noName_698) of
			 ((stop_chars_1, cont_2), s_126) =>
			 (case input1_4 s_126 of
			    NONE =>
			    (raise (IOError_0 "Unexpected end of stream"))
			  | (SOME ch_3) =>
			    (if (List.exists (fn ch'_0 => (ch_3 = ch'_0)))
				stop_chars_1
			     then ch_3 :: (cont_2 s_126)
			     else case ch_3 of
				    #"(" =>
				    (ch_3
				     :: ((get_6
					  ([#")"],
					   get_6 (stop_chars_1, cont_2)))
					 s_126))
				  | #"{" =>
				    (ch_3
				     :: ((get_6
					  ([#"}"],
					   get_6 (stop_chars_1, cont_2)))
					 s_126))
				  | #"[" =>
				    (ch_3
				     :: ((get_6
					  ([#"]"],
					   get_6 (stop_chars_1, cont_2)))
					 s_126))
				  | #"\"" =>
				    (ch_3
				     :: ((get_string_0
					  (get_6 (stop_chars_1, cont_2)))
					 s_126))
				  | _ =>
				    (ch_3
				     :: ((get_6 (stop_chars_1, cont_2)) s_126)

     )))))
	      fun rm_white_spaces_0 [] = []
		| rm_white_spaces_0 (x_196 :: xs_16) =
		  (if is_white_space_0 x_196
		   then rm_white_spaces_0 xs_16
		   else x_196 :: xs_16)
	  in (skip_white_spaces_0 s_124 ;
	      case rev ((get_6 (stop_chars_0, fn _ => [])) s_124) of
		(stop_char_0 :: res_0) =>
		(stop_char_0, rev (rm_white_spaces_0 res_0))
	      | _ => (raise Match))
	  end
      fun get_next_0 s_127 =
	  let val (_, list_9) = get_until_0 (s_127, ! white_spaces_0)
	  in list_9
	  end
      fun DSUI_SetStatusBarMessage_1 s_128 = (print (s_128 ^ "\n"))
      fun DSUI_GetUserYesOrNo_1 s_129 =
	  (print (s_129 ^ " (Yes/No)\n") ;
	   case explode (inputLine_2 stdIn_2) of
	     [] => true
	   | (#"Y" :: _) => true | (#"y" :: _) => true | _ => false)
      fun DSUI_UserAckMessage_1 s_130 = (print (s_130 ^ "\n"))
      exception EXUI_GetIntegerValue_1  of unit
      fun DSUI_GetIntegerValue_1 (arg_23 as {def = def_1, prompt = prompt_2})

      =
	  (print (prompt_2 ^ "\n") ;
	   case Int.fromString (inputLine_2 stdIn_2) of
	     (SOME x_197) => x_197
	   | NONE => (raise (EXUI_GetIntegerValue_1 ())))
      fun DSFile_NameDialog_1 (arg_24
			       as {okButtonLabel = okButtonLabel_1,
				   path = path_7,
				   prompt = prompt_3,
				   writeBox = writeBox_1}) =
	  (print (((prompt_3 ^ " relative to path ") ^ path_7) ^ "\n") ;
	   path_7 ^ (inputLine_2 stdIn_2))
      val empty_2 = [] : 'a list
      fun operator_265 (coef_0, col_2) =
	  (if coef_0 > 0
	   then col_2 :: (operator_265 (coef_0 - 1, col_2))
	   else [])
      fun operator_266 (ms1_0, ms2_0) = (List.@ (ms1_0, ms2_0))
      fun operator_267 (ms1_1, []) = ms1_1
	| operator_267 ([], _) = (raise Subtract_0)
	| operator_267 (ms1_2, col2_0 :: rms2_0) =
	  let fun sub_item_0 [] = (raise Subtract_0)
		| sub_item_0 (col1_0 :: rms1_0) =
		  (if col1_0 = col2_0
		   then rms1_0
		   else col1_0 :: (sub_item_0 rms1_0))
	  in operator_267 (sub_item_0 ms1_2, rms2_0)
	  end
      fun operator_268 (0, ms1_3) = []
	| operator_268 (n_230, ms1_4) =
	  (if n_230 > 0
	   then ((fold_30
		  (fn (a_97, b_74) =>
		      (((fold_33 (fn (_, c_106) => (a_97 :: c_106)))
			(0, n_230))
		       b_74)))
		 ms1_4)
		[]
	   else [])
      fun operator_269 (ms1_5, ms2_1) =
	  ((operator_269 (operator_267 (ms1_5, ms2_1), []))
	   handle Subtract_0 => false)
      fun operator_270 (ms1_6, ms2_2) =
	  ((operator_270 (operator_267 (ms1_6, ms2_2), []))
	   handle Subtract_0 => true)
      fun operator_271 (ms1_7, ms2_3) =
	  ((operator_271 (operator_267 (ms1_7, ms2_3), []))
	   handle Subtract_0 => false)
      fun operator_272 (ms1_8, ms2_4) =
	  ((operator_267 (ms1_8, ms2_4) ; true) handle Subtract_0 => false)
      fun operator_273 (ms1_9, ms2_5) =
	  ((operator_271 (operator_267 (ms2_5, ms1_9), []))
	   handle Subtract_0 => false)
      fun operator_274 (ms1_10, ms2_6) =
	  ((operator_267 (ms2_6, ms1_10) ; true)
	   handle Subtract_0 => false)
      val operator_275 = operator_270
      fun operator_276 ([], x_198) = [x_198]
	| operator_276 (true :: bs_0, x_199) = (operator_276 (bs_0, x_199))
	| operator_276 (false :: _, _) = []
      fun operator_277 ([], x_200) = x_200
	| operator_277 (true :: bs_1, x_201) = (operator_277 (bs_1, x_201))
	| operator_277 (false :: _, _) = []
      fun cf_0 (col_3, ms_9) =
	  (((fold_30
	     (fn (a_98, b_75) => (if a_98 = col_3 then 1 + b_75 else b_75))

     )
	    ms_9)
	   0)
      val size_11 = List.length
      fun ms1tocol_0 ms1_11 =
	  (if (size_11 ms1_11) = 1
	   then hd ms1_11
	   else raise (InternalError_0
		       "CPN'MS:ms1tocol called with ms not of size one"))
      val sort_ms_0 = sort_1
      fun legal_ms_0 noName_699 =
	  (fn noName_700 =>
	      (case (noName_699, noName_700) of
		 (legal_cs_0, ms_10) =>
		 (((fold_30
		    (fn (a_99, b_76) => ((legal_cs_0 a_99) andalso b_76)))
		   ms_10)
		  true)))
      fun mkstr_ms_0 noName_701 =
	  (fn noName_702 =>
	      (case (noName_701, noName_702) of
		 ((mkstr_cs_0, lt_4), ms_11) =>
		 let fun mkstr_2 (coef_1, col_4, x_202 :: xs_17) =
			 (if lt_4 (x_202, col_4)
			  then (Int.toString coef_1)
			       :: ("`"
				   :: ((mkstr_cs_0 col_4)
				       :: ("+" :: (mkstr_2 (1, x_202, xs_17)))

     ))
			  else mkstr_2 (coef_1 + 1, col_4, xs_17))
		       | mkstr_2 (coef_2, col_5, []) =
			 [Int.toString coef_2, "`", mkstr_cs_0 col_5]
		 in case (sort_1 lt_4) ms_11 of
		      (x_203 :: xs_18) =>
		      (concat (mkstr_2 (1, x_203, xs_18)))
		    | [] => "empty"
		 end))
      fun input_ms_0 noName_703 =
	  (fn noName_704 =>
	      (case (noName_703, noName_704) of
		 (input_col_0, s_131) =>
		 let fun mk_0 (coef_3, col_6, tail_16) =
			 (if coef_3 > 0
			  then col_6 :: (mk_0 (coef_3 - 1, col_6, tail_16))
			  else tail_16)
		     fun input'_0 (#"`", coef_4) =
			 (case get_until_0 (s_131, [#"+", #"`"]) of
			    (#"+", value_11) =>
			    (mk_0
			     (valOf (Int.fromString (implode coef_4)),
			      input_col_0 (openString_1 (implode value_11)),
			      input'_0 (get_until_0 (s_131, [#"`", #"+"]))))
			  | (#"`", item_16) =>
			    (raise (IOError_0
				    ("Can not find '+' in "
				     ^ (implode item_16))))
			  | (_, value_12) =>
			    (mk_0
			     (valOf (Int.fromString (implode coef_4)),
			      input_col_0 (openString_1 (implode value_12)),
			      [])))
		       | input'_0 (#"+", item_17) =
			 (raise (IOError_0
				 ("Can not find '`' in " ^ (implode item_17))))
		       | input'_0 (_, [#"e", #"m", #"p", #"t", #"y"]) = []
		       | input'_0 _ =
			 (raise (IOError_0
				 "Cannot find '`' or 'empty' when reading a ms"))
		 in input'_0 (get_until_0 (s_131, [#"`", #"+"]))
		 end))
      fun output_ms_0 noName_705 =
	  (fn noName_706 =>
	      (case (noName_705, noName_706) of
		 ((output_cs_0, lt_5), (s_132, ms_12)) =>
		 let fun put_1 (coef_5, col_7, x_204 :: xs_19) =
			 (if lt_5 (x_204, col_7)
			  then (output_5 (s_132, Int.toString coef_5) ;
				output1_4 (s_132, #"`") ;
				output_cs_0 (s_132, col_7) ;
				output1_4 (s_132, #"+") ;
				put_1 (1, x_204, xs_19))
			  else put_1 (coef_5 + 1, col_7, xs_19))
		       | put_1 (coef_6, col_8, []) =
			 (output_5 (s_132, Int.toString coef_6) ;
			  output1_4 (s_132, #"`") ;
			  output_cs_0 (s_132, col_8) ;
			  flushOut_4 s_132)
		 in case (sort_1 lt_5) ms_12 of
		      (x_205 :: xs_20) => (put_1 (1, x_205, xs_20))
		    | [] => (output_5 (s_132, "empty"))
		 end))
      fun filter_6 noName_707 =
	  (fn noName_708 =>
	      (case (noName_707, noName_708) of
		 (f_259, ms_13) =>
		 (((fold_30
		    (fn (a_100, b_77) =>
			(if f_259 a_100 then a_100 :: b_77 else b_77)))
		   ms_13)
		  [])))
      fun random_0 [] = (raise Empty_1)
	| random_0 ms_14 = (List.nth (ms_14, int_1 (size_11 ms_14)))
      val ext_col_0 = List.map
      fun ext_ms_0 noName_709 =
	  (fn noName_710 =>
	      (case (noName_709, noName_710) of
		 (f_260, ms_15) => (flatten_0 ((map f_260) ms_15))))
      fun get_ran_0 noName_711 =
	  (fn noName_712 =>
	      (case (noName_711, noName_712) of
		 (exn_6, []) => (raise exn_6)
	       | (_, ms_16) =>
		 let fun get_7 (0, col_9 :: rms_0) = (col_9, rms_0)
		       | get_7 (i_333, col_10 :: rms_1) =
			 let val (col'_0, rms'_0) =
				 get_7 (i_333 - 1, rms_1)
			 in (col'_0, col_10 :: rms'_0)
			 end
		       | get_7 _ = (raise Match)
		 in get_7 (int_1 (length ms_16), ms_16)
		 end))
      fun create_11 ms_17 =
	  {last = ref ((size_11 ms_17) - 1), set = Array.fromList ms_17}
      fun init_res_0 {last = last_2, set = set_0} =
	  (last_2 := ((Array.length set_0) - 1))
      fun random_res_0 noName_713 =
	  (fn noName_714 =>
	      (case (noName_713, noName_714) of
		 (exn_7, {last = ref ~1,...}) => (raise exn_7)
	       | (_, {last = last_3, set = set_1}) =>
		 let val draw_0 = int_1 ((! last_3) + 1)
		     val item_18 = Array.sub (set_1, draw_0)
		 in (Array.update
		     (set_1, draw_0, Array.sub (set_1, ! last_3)) ;
		     Array.update (set_1, ! last_3, item_18) ;
		     dec_1 last_3 ;
		     item_18)
		 end))
      val cs_hash_size_0 = 11
      val ref_hash_size_0 = 5
      val var_hash_size_0 = 17
      val node_hash_size_0 = 29
      val page_hash_size_0 = 17
      fun eq_4 (x_206 : string, y_43) = (x_206 = y_43)
      val hashString_1 = (hashString_0, eq_4)
      fun eq_5 (x_207 : int, y_44) = (x_207 = y_44)
      val hashInt_0 = (fn x_208 => x_208, eq_5)
      fun eq_6 ((x_209 :: xs_21) : int list, y_45 :: ys_9) =
	  (x_209 = y_45)
	| eq_6 ([], []) = true | eq_6 _ = false
      val hashIntList_0 = ((foldl Int.+) 0, eq_6)
      val hashId_0 = (hash_13, eq_3)
      datatype kind_0 =
	       unit_cs_0 of string option
	     | bool_cs_0 of (string * string) option
	     | int_cs_0 of (string * string) option
	     | real_cs_0 of (string * string) option
	     | char_cs_0 of (string * string) option
	     | string_cs_0 of {char : (string * string) option,
			       length : (string * string) option}
	     | enum_cs_0 of string list
	     | index_cs_0 of {idx : string, over : (string * string) option}
	     | list_cs_0 of {cs : string, length : (string * string) option}
	     | product_cs_0 of (string * string) list
	     | record_cs_0 of (string * string) list
	     | union_cs_0 of (string * string) list
	     | funsubset_cs_0 of {cs : string, subset : string}
	     | listsubset_cs_0 of {cs : string, subset : string list}
	     | duplicate_cs_0 of string
	     | time_cs_0
	     | alias_cs_0 of string
      val table_28 :
	  (string,
	   {alias : string list,
	    declare : string list,
	    id : string,
	    kind : kind_0,
	    msvar : string list,
	    timed : bool,
	    var : string list}) hash_table_0 =
	  (mkTable_0 hashString_1)
	  (cs_hash_size_0, InternalError_0 "CStable.find")
      val insert_5 = insert_4 table_28
      val remove_3 = remove_2 table_28
      val find_4 = lookup_0 table_28
      val peek_6 = find_3 table_28
      fun append_alias_cs_0 (n_231, name_46) =
	  (case peek_6 name_46 of
	     (SOME
	      {id = id_4, msvar = msvar_0, timed = timed_0, var = var_0,...}

     ) =>
	     (insert_5
	      (n_231,
	       {alias = [],
		declare = [],
		id = id_4,
		kind = alias_cs_0 name_46,
		msvar = msvar_0,
		timed = timed_0,
		var = var_0}))
	   | NONE => (raise (InternalError_0 "append_alias_cs")))
      fun append_declare_0 (name_47, declare'_0) =
	  (case peek_6 name_47 of
	     (SOME
	      {alias = alias_0,
	       declare = declare_0,
	       id = id_5,
	       kind = kind_2,
	       msvar = msvar_1,
	       timed = timed_1,
	       var = var_1}) =>
	     (insert_5
	      (name_47,
	       {alias = alias_0,
		declare = List.@ (declare'_0, declare_0),
		id = id_5,
		kind = kind_2,
		msvar = msvar_1,
		timed = timed_1,
		var = var_1}))
	   | NONE => (raise (InternalError_0 "append_declare")))
      fun append_alias_0 (name_48, alias'_0) =
	  (case peek_6 name_48 of
	     (SOME
	      {alias = alias_1,
	       declare = declare_1,
	       id = id_6,
	       kind = kind_3,
	       msvar = msvar_2,
	       timed = timed_2,
	       var = var_2}) =>
	     (insert_5
	      (name_48,
	       {alias = List.@ (alias'_0, alias_1),
		declare = declare_1,
		id = id_6,
		kind = kind_3,
		msvar = msvar_2,
		timed = timed_2,
		var = var_2}))
	   | NONE => (raise (InternalError_0 "append_alias")))
      fun append_var_0 (name_49, var'_0) =
	  (case peek_6 name_49 of
	     (SOME
	      {alias = alias_2,
	       declare = declare_2,
	       id = id_7,
	       kind = kind_4,
	       msvar = msvar_3,
	       timed = timed_3,
	       var = var_3}) =>
	     (insert_5
	      (name_49,
	       {alias = alias_2,
		declare = declare_2,
		id = id_7,
		kind = kind_4,
		msvar = msvar_3,
		timed = timed_3,
		var = List.@ (var'_0, var_3)}))
	   | NONE => (raise (InternalError_0 "append_var")))
      fun append_msvar_0 (name_50, msvar'_0) =
	  (case peek_6 name_50 of
	     (SOME
	      {alias = alias_3,
	       declare = declare_3,
	       id = id_8,
	       kind = kind_5,
	       msvar = msvar_4,
	       timed = timed_4,
	       var = var_4}) =>
	     (insert_5
	      (name_50,
	       {alias = alias_3,
		declare = declare_3,
		id = id_8,
		kind = kind_5,
		msvar = List.@ (msvar'_0, msvar_4),
		timed = timed_4,
		var = var_4}))
	   | NONE => (raise (InternalError_0 "append_msvar")))
      fun list_10 () =
	  let fun convert_cs_0 (unit_cs_0 NONE, tail_17) =
		  ("unit" :: tail_17)
		| convert_cs_0 (unit_cs_0 (SOME str_3), tail_18) =
		  ("unit with " :: (str_3 :: tail_18))
		| convert_cs_0 (bool_cs_0 NONE, tail_19) =
		  ("bool" :: tail_19)
		| convert_cs_0 (bool_cs_0 (SOME (low_0, high_0)), tail_20) =
		  ("bool with ("
		   :: (low_0 :: ("," :: (high_0 :: (")" :: tail_20)))))
		| convert_cs_0 (int_cs_0 NONE, tail_21) = ("int" :: tail_21)
		| convert_cs_0 (int_cs_0 (SOME (low_1, high_1)), tail_22) =
		  ("int with " :: (low_1 :: (".." :: (high_1 :: tail_22))))
		| convert_cs_0 (real_cs_0 NONE, tail_23) =
		  ("real" :: tail_23)
		| convert_cs_0 (real_cs_0 (SOME (low_2, high_2)), tail_24) =
		  ("real with " :: (low_2 :: (".." :: (high_2 :: tail_24))))
		| convert_cs_0 (char_cs_0 NONE, tail_25) =
		  ("char" :: tail_25)
		| convert_cs_0 (char_cs_0 (SOME (low_3, high_3)), tail_26) =
		  ("char with " :: (low_3 :: (".." :: (high_3 :: tail_26))))
		| convert_cs_0 (string_cs_0 {char = NONE, length = NONE},
				tail_27) =
		  ("string" :: tail_27)
		| convert_cs_0 (string_cs_0
				{char = SOME (low_4, high_4), length = NONE}

     ,
				tail_28) =
		  ("string with "
		   :: (low_4 :: (".." :: (high_4 :: tail_28))))
		| convert_cs_0 (string_cs_0
				{char = SOME (low_5, high_5),
				 length = SOME (min_10, max_9)},
				tail_29) =
		  ("string with "
		   :: (low_5
		       :: (".."
			   :: (high_5
			       :: (" length "
				   :: (min_10 :: (".." :: (max_9 :: tail_29)))

     )))))
		| convert_cs_0 (enum_cs_0 enm_0, tail_30) =
		  ("enumerate "
		   :: (tl
		       (((fold_30
			  (fn (a_101, b_78) => (" | " :: (a_101 :: b_78))))
			 enm_0)
			tail_30)))
		| convert_cs_0 (index_cs_0 {idx = idx_0, over = NONE},
				tail_31) =
		  ("index " :: (idx_0 :: tail_31))
		| convert_cs_0 (index_cs_0
				{idx = idx_1, over = SOME (low_6, high_6)},
				tail_32) =
		  ("index "
		   :: (idx_1
		       :: (" with "
			   :: (low_6 :: (".." :: (high_6 :: tail_32))))))
		| convert_cs_0 (list_cs_0 {cs = cs_43, length = NONE},
				tail_33) =
		  ("list " :: (cs_43 :: tail_33))
		| convert_cs_0 (list_cs_0
				{cs = cs_44, length = SOME (min_11, max_10)}

     ,
				tail_34) =
		  ("list "
		   :: (cs_44
		       :: (" with "
			   :: (min_11 :: (".." :: (max_10 :: tail_34))))))
		| convert_cs_0 (product_cs_0 comp_0, tail_35) =
		  ("product "
		   :: (tl
		       (((fold_30
			  (fn ((_, cs_45), b_79) =>
			      (" * " :: (cs_45 :: b_79))))
			 comp_0)
			tail_35)))
		| convert_cs_0 (record_cs_0 comp_1, tail_36) =
		  ("record "
		   :: (tl
		       (((fold_30
			  (fn ((label_0, cs_46), b_80) =>
			      (" * "
			       :: (label_0 :: (": " :: (cs_46 :: b_80))))))
			 comp_1)
			tail_36)))
		| convert_cs_0 (union_cs_0 comp_2, tail_37) =
		  ("union "
		   :: (tl
		       (((fold_30
			  (fn ((label_1, cs_47), b_81) =>
			      (" + "
			       :: (label_1 :: (": " :: (cs_47 :: b_81))))))
			 comp_2)
			tail_37)))
		| convert_cs_0 (funsubset_cs_0
				{cs = cs_48, subset = subset_0},
				tail_38) =
		  ("subset " :: (cs_48 :: (" by " :: (subset_0 :: tail_38))))
		| convert_cs_0 (listsubset_cs_0
				{cs = cs_49, subset = subset_1},
				tail_39) =
		  ("subset "
		   :: (cs_49
		       :: (" with ["
			   :: (tl
			       (((fold_30
				  (fn (a_102, b_82) =>
				      ("," :: (a_102 :: b_82))))
				 subset_1)
				("" :: ("]" :: tail_39)))))))
		| convert_cs_0 (time_cs_0, tail_40) = ("time" :: tail_40)
		| convert_cs_0 (duplicate_cs_0 cs_50, tail_41) =
		  (cs_50 :: tail_41)
		| convert_cs_0 (alias_cs_0 cs_51, tail_42) =
		  (cs_51 :: tail_42)
		| convert_cs_0 _ =
		  (raise (InternalError_0 "Must be cs in CSTable"))
	      fun convert_timed_0 (timed_5, tail_43) =
		  (if timed_5 then " timed " :: tail_43 else tail_43)
	      fun convert_var_0 ([], tail_44) = tail_44
		| convert_var_0 (var_5, tail_45) =
		  ("\nvar "
		   :: (tl
		       (((fold_30
			  (fn (a_103, b_83) => (", " :: (a_103 :: b_83))))
			 var_5)
			tail_45)))
	      fun convert_msvar_0 ([], tail_46) = tail_46
		| convert_msvar_0 (var_6, tail_47) =
		  ("\nmsvar "
		   :: (tl
		       (((fold_30
			  (fn (a_104, b_84) => (", " :: (a_104 :: b_84))))
			 var_6)
			tail_47)))
	      fun convert_declare_0 ([], tail_48) = tail_48
		| convert_declare_0 (declare_4, tail_49) =
		  ("\ndeclare "
		   :: (tl
		       (((fold_30
			  (fn (a_105, b_85) => (" " :: (a_105 :: b_85))))
			 declare_4)
			tail_49)))
	      fun convert_alias_0 ([], tail_50) = tail_50
		| convert_alias_0 (alias_4, tail_51) =
		  ("\nalias "
		   :: (tl
		       (((fold_30
			  (fn (a_106, b_86) => (" " :: (a_106 :: b_86))))
			 alias_4)
			tail_51)))
	      fun convert_0 (name_51,
			     {alias = alias_5,
			      declare = declare_5,
			      id = id_9,
			      kind = kind_6,
			      msvar = msvar_5,
			      timed = timed_6,
			      var = var_7}) =
		  (concat
		   ("\ncolorset "
		    :: (name_51
			:: (" = "
			    :: (convert_cs_0
				(kind_6,
				 convert_timed_0
				 (timed_6,
				  convert_var_0
				  (var_7,
				   convert_msvar_0
				   (msvar_5,
				    convert_declare_0
				    (declare_5, convert_alias_0 (alias_5, []))

     )))))))))
	  in (map convert_0) (listItemsi_1 table_28)
	  end
      exception SimpleCS_0 
      fun get_components_0 cs_52 =
	  (case peek_6 cs_52 of
	     (SOME {kind = kind_7,...}) =>
	     (case kind_7 of
		(product_cs_0 comp_3) => comp_3
	      | (record_cs_0 comp_4) => comp_4
	      | (union_cs_0 comp_5) => comp_5
	      | (alias_cs_0 cs_53) => (get_components_0 cs_53)
	      | (funsubset_cs_0 {cs = cs_54,...}) =>
		(get_components_0 cs_54)
	      | (listsubset_cs_0 {cs = cs_55,...}) =>
		(get_components_0 cs_55)
	      | _ => (raise SimpleCS_0))
	   | NONE => (raise (InternalError_0 "get_components")))
      fun get_cs_order_0 cs_56 =
	  let fun get_8 ([], _) = []
		| get_8 (_ :: xs_22, i_334) =
		  (i_334 :: (get_8 (xs_22, i_334 + 1)))
	  in (get_8 (get_components_0 cs_56, 1)) handle SimpleCS_0 => [1]
	  end
      fun get_prime_kind_0 (alias_cs_0 cs_57) =
	  (get_prime_kind_0
	   (case find_4 cs_57 of
	      {kind = noName_715,...} => noName_715))
	| get_prime_kind_0 (duplicate_cs_0 cs_58) =
	  (get_prime_kind_0
	   (case find_4 cs_58 of
	      {kind = noName_716,...} => noName_716))
	| get_prime_kind_0 (funsubset_cs_0 {cs = cs_59,...}) =
	  (get_prime_kind_0
	   (case find_4 cs_59 of
	      {kind = noName_717,...} => noName_717))
	| get_prime_kind_0 (listsubset_cs_0 {cs = cs_60,...}) =
	  (get_prime_kind_0
	   (case find_4 cs_60 of
	      {kind = noName_718,...} => noName_718))
	| get_prime_kind_0 cs_61 = cs_61
      fun get_prime_cs_0 cs_62 =
	  (case peek_6 cs_62 of
	     (SOME {kind = alias_cs_0 cs_63,...}) =>
	     (get_prime_cs_0 cs_63)
	   | (SOME {kind = duplicate_cs_0 cs_64,...}) =>
	     (get_prime_cs_0 cs_64)
	   | (SOME _) => cs_62
	   | NONE => (raise (InternalError_0 ("get_prime_cs " ^ cs_62))))
      fun get_parent_cs_0 cs_65 =
	  (case peek_6 cs_65 of
	     (SOME {kind = kind_8,...}) =>
	     (case kind_8 of
		(list_cs_0 {cs = cs_66,...}) => [cs_66]
	      | (product_cs_0 comp_6) =>
		((map (fn {2 = noName_719,...} => noName_719)) comp_6)
	      | (record_cs_0 comp_7) =>
		((map (fn {2 = noName_720,...} => noName_720)) comp_7)
	      | (union_cs_0 comp_8) =>
		((map (fn {2 = noName_721,...} => noName_721)) comp_8)
	      | (funsubset_cs_0 {cs = cs_67,...}) => [cs_67]
	      | (listsubset_cs_0 {cs = cs_68,...}) => [cs_68]
	      | (duplicate_cs_0 cs_69) => [cs_69]
	      | (alias_cs_0 cs_70) => [cs_70]
	      | _ => [])
	   | NONE => (raise (InternalError_0 ("get_parent_cs " ^ cs_65))))
      fun is_subtype_0 cs_71 =
	  (case peek_6 cs_71 of
	     (SOME {kind = kind_9,...}) =>
	     (case kind_9 of
		(unit_cs_0 _) => false
	      | (bool_cs_0 _) => false
	      | (int_cs_0 NONE) => false
	      | (real_cs_0 NONE) => false
	      | (string_cs_0 {char = NONE, length = NONE}) => false
	      | (index_cs_0 {over = NONE,...}) => false
	      | (list_cs_0 {length = NONE,...}) => false
	      | (product_cs_0 comp_9) =>
		(((fold_30
		   (fn ((_, cs_72), b_87) =>
		       ((is_subtype_0 cs_72) orelse b_87)))
		  comp_9)
		 false)
	      | (record_cs_0 comp_10) =>
		(((fold_30
		   (fn ((_, cs_73), b_88) =>
		       ((is_subtype_0 cs_73) orelse b_88)))
		  comp_10)
		 false)
	      | (union_cs_0 comp_11) =>
		(((fold_30
		   (fn ((_, ""), b_89) => (false orelse b_89)
		     | ((_, cs_74), b_90) =>
		       ((is_subtype_0 cs_74) orelse b_90)))
		  comp_11)
		 false)
	      | (duplicate_cs_0 cs_75) => (is_subtype_0 cs_75)
	      | (alias_cs_0 cs_76) => (is_subtype_0 cs_76)
	      | time_cs_0 => false
	      | _ => true)
	   | NONE => (raise (InternalError_0 ("is_subtype " ^ cs_71))))
      fun is_timed_0 cs_77 =
	  (case peek_6 cs_77 of
	     (SOME {timed = timed_7,...}) => timed_7
	   | NONE => (raise (InternalError_0 ("is_timed " ^ cs_77))))
      fun is_equiv_0 (cs_78, cs'_16) =
	  (((get_prime_cs_0 cs_78) = (get_prime_cs_0 cs'_16))
	   andalso ((is_timed_0 cs_78) = (is_timed_0 cs'_16)))
      val table_29 : (string, string) hash_table_0 =
	  (mkTable_0 hashString_1)
	  (var_hash_size_0, InternalError_0 "VarTable.find")
      fun insert_6 (key_13, item_19) =
	  (CPN'debug_0
	   ((("CPN'VarTable: key=" ^ key_13) ^ " item=") ^ item_19) ;
	   (insert_4 table_29) (key_13, item_19))
      val remove_4 = remove_2 table_29
      val find_5 = lookup_0 table_29
      val peek_7 = find_3 table_29
      fun list_11 () = (listItemsi_1 table_29)
      fun get_var_string_0 () =
	  (concat
	   ("("
	    :: (tl
		(((fold_30
		   (fn ((v_51, cs_79), b_91) =>
		       ("," :: (v_51 :: (": " :: (cs_79 :: b_91))))))
		  (list_11 ()))
		 ["", ")"]))))
      datatype item_0 =
	       globref_0 of {exp : string, name : string}
	     | pageref_0 of {exp : string, name : string, page : string}
	     | instref_0 of {exp : string, name : string, page : string}
      val table_30 : (string, item_0) hash_table_0 =
	  (mkTable_0 hashString_1)
	  (ref_hash_size_0, InternalError_0 "RefTable.find")
      val insert_7 = insert_4 table_30
      val remove_5 = remove_2 table_30
      val find_6 = lookup_0 table_30
      val peek_8 = find_3 table_30
      fun list_12 () = (listItemsi_1 table_30)
      fun make_rep_name_0 (globref_0 {name = name_52,...}) = name_52
	| make_rep_name_0 (pageref_0 {name = name_53, page = page_0,...}) =
	  (concat ["CPN'", name_53, makeid_0 page_0])
	| make_rep_name_0 (instref_0 {name = name_54, page = page_1,...}) =
	  (concat ["CPN'", name_54, makeid_0 page_1])
      fun get_rep_name_0 rid_0 =
	  (case peek_8 rid_0 of
	     (SOME (globref_0 {name = name_55,...})) => (name_55, name_55)
	   | (SOME (r_85 as pageref_0 {name = name_56,...})) =>
	     (name_56, make_rep_name_0 r_85)
	   | (SOME (r_86 as instref_0 {name = name_57,...})) =>
	     (name_57, make_rep_name_0 r_86)
	   | _ => (raise (InternalError_0 "get_rep_name")))
      fun get_var_string_1 var_list_0 =
	  let fun varstr_0 (pageref_0
			    {exp = exp_8, name = name_58, page = page_2},
			    l_111) =
		  ("val "
		   :: (name_58 :: (" = ref " :: (exp_8 :: (";" :: l_111)))))
		| varstr_0 (instref_0
			    {exp = exp_9, name = name_59, page = page_3},
			    l_112) =
		  ("val "
		   :: (name_59
		       :: (" = " :: ("ref " :: (exp_9 :: (";" :: l_112))))))
		| varstr_0 (globref_0 {exp = exp_10, name = name_60}, l_113)

      =
		  ("val "
		   :: (name_60
		       :: (" = " :: ("ref " :: (exp_10 :: (";" :: l_113))))))
	  in concat (((fold_30 varstr_0) ((map find_6) var_list_0)) [])
	  end
      val table_31 :
	  (string,
	   {decl : {instrefs : string list, pagerefs : string list} option,
	    page :
	    {included : bool,
	     name : string,
	     places : string list,
	     prime : int,
	     transitions : string list},
	    sub_pages : string list,
	    super_pages : string list}) hash_table_0 =
	  (mkTable_0 hashId_0)
	  (page_hash_size_0, InternalError_0 "PageTable.find")
      val peek_9 = find_3 table_31
      val find_7 = lookup_0 table_31
      val insert_8 = insert_4 table_31
      fun get_ref_vars_0 id_10 =
	  (case peek_9 id_10 of
	     (SOME {decl = NONE,...}) => []
	   | (SOME
	      {decl = SOME {instrefs = instrefs_0, pagerefs = pagerefs_0},...

     }) =>
	     (List.@ (instrefs_0, pagerefs_0))
	   | NONE => (raise (InternalError_0 "get_ref_vars")))
      fun get_page_0 id_11 =
	  (case peek_9 id_11 of
	     (SOME {page = page_4,...}) => page_4
	   | NONE => (raise (InternalError_0 "get_page")))
      fun append_2 (id_12, page_5) =
	  (case peek_9 id_12 of
	     NONE =>
	     (insert_8
	      (id_12,
	       {decl = NONE,
		page = page_5,
		sub_pages = [],
		super_pages = []}))
	   | (SOME
	      {decl = decl_0,
	       sub_pages = sub_pages_0,
	       super_pages = super_pages_0,...}) =>
	     (insert_8
	      (id_12,
	       {decl = decl_0,
		page = page_5,
		sub_pages = sub_pages_0,
		super_pages = super_pages_0})))
      fun append_sub_node_0 (super_page_0, sub_page_0) =
	  (case find_7 super_page_0 of
	     {decl = decl_1,
	      page = page_6,
	      sub_pages = sub_pages_1,
	      super_pages = super_pages_1} =>
	     (insert_8
	      (super_page_0,
	       {decl = decl_1,
		page = page_6,
		sub_pages = sub_page_0 :: sub_pages_1,
		super_pages = super_pages_1})) ;
	   case find_7 sub_page_0 of
	     {decl = decl_2,
	      page = page_7,
	      sub_pages = sub_pages_2,
	      super_pages = super_pages_2} =>
	     (insert_8
	      (sub_page_0,
	       {decl = decl_2,
		page = page_7,
		sub_pages = sub_pages_2,
		super_pages = super_page_0 :: super_pages_2})))
      fun append_group_0 (id_13, grp_0) =
	  (case peek_9 id_13 of
	     (SOME
	      {decl = decl_3,
	       page =
	       {included = included_0,
		name = name_61,
		places = places_0,
		prime = prime_0,
		transitions = transitions_0},
	       sub_pages = sub_pages_3,
	       super_pages = super_pages_3}) =>
	     (insert_8
	      (id_13,
	       {decl = decl_3,
		page =
		{included = included_0,
		 name = name_61,
		 places = grp_0 :: places_0,
		 prime = prime_0,
		 transitions = transitions_0},
		sub_pages = sub_pages_3,
		super_pages = super_pages_3}))
	   | NONE => (raise (InternalError_0 "append_group")))
      val remove_6 = remove_2 table_31
      fun remove_super_page_0 (id_14, s_133) =
	  let val {decl = decl_4,
		   page = page_8,
		   sub_pages = sub_pages_4,
		   super_pages = super_pages_4} =
		  find_7 id_14
	      fun rm_0 (x_210 :: xs_23) =
		  (if eq_3 (s_133, x_210)
		   then xs_23
		   else x_210 :: (rm_0 xs_23))
		| rm_0 [] = (raise (InternalError_0 "Not a super page"))
	  in (insert_4 table_31)
	     (id_14,
	      {decl = decl_4,
	       page = page_8,
	       sub_pages = sub_pages_4,
	       super_pages = rm_0 super_pages_4})
	  end
      fun remove_sub_page_0 (id_15, s_134) =
	  let val {decl = decl_5,
		   page = page_9,
		   sub_pages = sub_pages_5,
		   super_pages = super_pages_5} =
		  find_7 id_15
	      fun rm_1 (x_211 :: xs_24) =
		  (if eq_3 (s_134, x_211)
		   then xs_24
		   else x_211 :: (rm_1 xs_24))
		| rm_1 [] = (raise (InternalError_0 "Not a sub page"))
	  in (insert_4 table_31)
	     (id_15,
	      {decl = decl_5,
	       page = page_9,
	       sub_pages = rm_1 sub_pages_5,
	       super_pages = super_pages_5})
	  end
      fun rm_2 (_, []) = (raise (InternalError_0 "rm in PageTable"))
	| rm_2 (id_16, id'_1 :: xs_25) =
	  (if eq_3 (id_16, id'_1)
	   then xs_25
	   else id'_1 :: (rm_2 (id_16, xs_25)))
      val rm_all_0 =
	  fn (l1_10, l2_11) =>
	     (((fold_30 (fn (x_212, l_114) => (rm_2 (x_212, l_114))))
	       l2_11)
	      l1_10)
      fun remove_diff_0 (page_10,
			 {places' = places'_0, transitions' = transitions'_0})

      =
	  let val {decl = decl_6,
		   page =
		   {included = included_1,
		    name = name_62,
		    places = places_1,
		    prime = prime_1,
		    transitions = transitions_1},
		   sub_pages = sub_pages_6,
		   super_pages = super_pages_6} =
		  find_7 page_10
	  in (insert_4 table_31)
	     (page_10,
	      {decl = decl_6,
	       page =
	       {included = included_1,
		name = name_62,
		places = rm_all_0 (places_1, places'_0),
		prime = prime_1,
		transitions = rm_all_0 (transitions_1, transitions'_0)},
	       sub_pages = sub_pages_6,
	       super_pages = super_pages_6})
	  end
      fun list_13 () = (listItemsi_1 table_31)
      fun get_no_of_inst_0 id_17 =
	  let val {page = {prime = prime_2,...},
		   super_pages = super_pages_7,...} =
		  find_7 id_17
	  in ((fold_30
	       (fn (a_107, b_92) => ((get_no_of_inst_0 a_107) + b_92)))
	      super_pages_7)
	     prime_2
	  end
      fun get_all_no_of_inst_0 () =
	  ((map (fn (id_18, _) => (id_18, get_no_of_inst_0 id_18)))
	   (listItemsi_1 table_31))
      fun get_places_0 id_19 =
	  (case peek_9 id_19 of
	     (SOME {page = {places = places_2,...},...}) => places_2
	   | NONE => (raise (InternalError_0 "get_places")))
      fun get_transitions_0 id_20 =
	  (case peek_9 id_20 of
	     (SOME {page = {transitions = transitions_2,...},...}) =>
	     transitions_2
	   | NONE => (raise (InternalError_0 "get_transitions")))
      fun list_all_places_bottom_up_0 () =
	  let fun find_bottom_pages_0 ((id_21,
					{sub_pages = [],...} :
					{decl :
					 {instrefs : string list,
					  pagerefs : string list} option,
					 page :
					 {included : bool,
					  name : string,
					  places : string list,
					  prime : int,
					  transitions : string list},
					 sub_pages : string list,
					 super_pages : string list})
				       :: pages_0,
				       tail_52) =
		  (find_bottom_pages_0 (pages_0, id_21 :: tail_52))
		| find_bottom_pages_0 (_ :: pages_1, tail_53) =
		  (find_bottom_pages_0 (pages_1, tail_53))
		| find_bottom_pages_0 ([], tail_54) = tail_54
	      fun get_super_pages_0 pages_2 =
		  (flatten_0
		   (((fold_30
		      (fn (id_22, tail_55) =>
			  ((case find_7 id_22 of
			      {super_pages = noName_722,...} => noName_722)
			   :: tail_55)))
		     pages_2)
		    []))
	      fun list_all_0 (_, [], []) = []
		| list_all_0 (listed_0, listing_0, []) =
		  (list_all_0 (listed_0, [], get_super_pages_0 listing_0))
		| list_all_0 (listed_1, listing_1, id_23 :: pages_3) =
		  (if (List.exists (fn id'_2 => (eq_3 (id_23, id'_2))))
		      listed_1
		   then list_all_0 (listed_1, listing_1, pages_3)
		   else List.@
			(get_places_0 id_23,
			 list_all_0
			 (id_23 :: listed_1, id_23 :: listing_1, pages_3)))
	  in list_all_0 ([], [], find_bottom_pages_0 (list_13 (), []))
	  end
      fun find_top_pages_0 ((id_24,
			     {super_pages = [],...} :
			     {decl :
			      {instrefs : string list, pagerefs : string list}

      option,
			      page :
			      {included : bool,
			       name : string,
			       places : string list,
			       prime : int,
			       transitions : string list},
			      sub_pages : string list,
			      super_pages : string list})
			    :: pages_4,
			    tail_56) =
	  (find_top_pages_0 (pages_4, id_24 :: tail_56))
	| find_top_pages_0 (_ :: pages_5, tail_57) =
	  (find_top_pages_0 (pages_5, tail_57))
	| find_top_pages_0 ([], tail_58) = tail_58
      fun get_sub_pages_0 pages_6 =
	  (flatten_0
	   (((fold_30
	      (fn (id_25, tail_59) =>
		  ((case find_7 id_25 of
		      {sub_pages = noName_723,...} => noName_723)
		   :: tail_59)))
	     pages_6)
	    []))
      fun list_all_places_top_down_0 () =
	  let fun list_all_1 (_, [], []) = []
		| list_all_1 (listed_2, listing_2, []) =
		  (list_all_1 (listed_2, [], get_sub_pages_0 listing_2))
		| list_all_1 (listed_3, listing_3, id_26 :: pages_7) =
		  (if (List.exists (fn id'_3 => (eq_3 (id_26, id'_3))))
		      listed_3
		   then list_all_1 (listed_3, listing_3, pages_7)
		   else List.@
			(get_places_0 id_26,
			 list_all_1
			 (id_26 :: listed_3, id_26 :: listing_3, pages_7)))
	  in list_all_1 ([], [], find_top_pages_0 (list_13 (), []))
	  end
      fun list_all_pages_top_down_0 () =
	  let fun list_all_2 (_, [], []) = []
		| list_all_2 (listed_4, listing_4, []) =
		  (list_all_2 (listed_4, [], get_sub_pages_0 listing_4))
		| list_all_2 (listed_5, listing_5, id_27 :: pages_8) =
		  (if (List.exists (fn id'_4 => (eq_3 (id_27, id'_4))))
		      listed_5
		   then list_all_2 (listed_5, listing_5, pages_8)
		   else id_27
			:: (list_all_2
			    (id_27 :: listed_5, id_27 :: listing_5, pages_8)))
	  in list_all_2 ([], [], find_top_pages_0 (list_13 (), []))
	  end
      datatype exp_type_0 =
	       token_exp_0 of string
	     | ms_exp_0 of string | tms_exp_0 of string
      datatype kind_1 =
	       place_0
	     | port_5 of (string * string) list | fusion_0 of string | group_0
      val table_32 :
	  (string,
	   {ext :
	    {cs : string, im : exp_type_0, name : string, page : string},
	    int :
	    {ims : string,
	     name : string,
	     offset : string option,
	     order : int list} option,
	    kind : kind_1}) hash_table_0 =
	  (mkTable_0 hashId_0)
	  (node_hash_size_0, InternalError_0 "PlaceTable.find")
      val find_8 = lookup_0 table_32
      val peek_10 = find_3 table_32
      fun insert_9 (p_34, kind_10, ext_3) =
	  ((insert_4 table_32)
	   (p_34, {ext = ext_3, int = NONE, kind = kind_10}))
      val remove_7 = remove_2 table_32
      fun list_14 () = (listItemsi_1 table_32)
      fun append_socket_0 (p_35, s_135) =
	  (case peek_10 p_35 of
	     (SOME {ext = ext_4, int = int_2, kind = port_5 sockets_0}) =>
	     ((insert_4 table_32)
	      (p_35,
	       {ext = ext_4,
		int = int_2,
		kind = port_5 (s_135 :: sockets_0)}))
	   | (SOME {ext = ext_5, int = int_3, kind = place_0}) =>
	     ((insert_4 table_32)
	      (p_35, {ext = ext_5, int = int_3, kind = port_5 [s_135]}))
	   | _ => (raise (InternalError_0 "append_socket")))
      fun append_rep_0 (p_36, rep_10) =
	  (case peek_10 p_36 of
	     (SOME {ext = ext_6, kind = kind_11,...}) =>
	     ((insert_4 table_32)
	      (p_36, {ext = ext_6, int = SOME rep_10, kind = kind_11}))
	   | _ => (raise (InternalError_0 "append_rep")))
      fun get_ext_0 p_37 =
	  (case peek_10 p_37 of
	     (SOME {ext = ext_7,...}) => ext_7
	   | _ => (raise (InternalError_0 "get_ext")))
      fun get_rep_0 p_38 =
	  (case peek_10 p_38 of
	     (SOME
	      {ext = {cs = cs_80,...},
	       int =
	       SOME
	       {ims = ims_0,
		name = name_63,
		offset = offset_9,
		order = order_5},...}) =>
	     {cs = cs_80,
	      ims = ims_0,
	      name = name_63,
	      offset = offset_9,
	      order = order_5}
	   | _ => (raise (InternalError_0 "get_rep")))
      fun in_order_0 (p_39, keys_0 : {exp : string, label : string} list) =
	  let fun in_order'_0 ((lbl_0, _) :: rcomp_0,
			       ({label = label_2,...} :
				{exp : string, label : string})
			       :: rkeys_0) =
		  ((label_2 = lbl_0)
		   andalso (in_order'_0 (rcomp_0, rkeys_0)))
		| in_order'_0 (_, []) = true | in_order'_0 ([], _) = true
	      val {ext = {cs = cs_81,...},...} = find_8 p_39
	  in in_order'_0 (get_components_0 cs_81, keys_0)
	  end
      fun is_timed_1 p_40 =
	  (case peek_10 p_40 of
	     (SOME {ext = {cs = cs_82,...},...}) => (is_timed_0 cs_82)
	   | NONE => (raise (InternalError_0 "is_timed")))
      datatype bop_0 =
	       B_p_0 of {coef : string,
			 isdiv : (int * int) option,
			 pat : string,
			 place : string,
			 time : string option,
			 vars : string list}
	     | B_k_0 of {coef : string,
			 keys :
			 {exp : string, label : string, no : int} list,
			 pat : string,
			 place : string,
			 time : string option,
			 vars : string list}
	     | B_g_0 of {exp : string, pat : string, vars : string list}
	     | B_c_0 of {cs : string, var : string}
	     | T_a_0 of {coef : string,
			 exp : string,
			 place : string,
			 time : string option}
	     | T_g_0 of {exp : string}
	     | T_v_0 of {var1 : string, var2 : string}
      datatype item_1 =
	       substitution_0 of {border :
				  {port : string, socket : string} list,
				  name : string,
				  page : string,
				  subpage : string}
	     | transition_0 of {code_reg :
				{action : string,
				 input : string list,
				 output : string list} option,
				free_vars : string list,
				groups :
				{bops : bop_0 list, vars : string list} list,
				input :
				{arcs : (string * exp_type_0) list,
				 no_of_tokens : int option,
				 place : string} list,
				name : string,
				output :
				{arcs : (string * exp_type_0) list,
				 place : string} list,
				page : string,
				time_reg : string}
      val table_33 : (string, item_1) hash_table_0 =
	  (mkTable_0 hashId_0)
	  (node_hash_size_0, InternalError_0 "TransitionTable.find")
      val insert_10 = insert_4 table_33
      val remove_8 = remove_2 table_33
      fun find_9 t_26 = ((lookup_0 table_33) t_26)
      fun peek_11 t_27 = ((find_3 table_33) t_27)
      fun list_15 () = (listItemsi_1 table_33)
      fun list_sorted_by_name_0 () =
	  let fun f_261 (_, (transition_0 {name = name_64,...}) : item_1) =
		  name_64
		| f_261 (_, substitution_0 {name = name_65,...}) = name_65
	  in (sort_1 (fn (a_108, b_93) => ((f_261 a_108) < (f_261 b_93))))
	     (list_15 ())
	  end
      fun get_vars_0 (t_28, i_335) =
	  (case peek_11 t_28 of
	     (SOME (transition_0 {groups = groups_0,...})) =>
	     (flatten_0
	      ((map (fn {bops = _, vars = vars_0} => vars_0)) groups_0))
	   | _ => (raise (InternalError_0 "get_vars")))
      fun get_page_1 t_29 =
	  (case peek_11 t_29 of
	     (SOME (transition_0 {page = page_11,...})) => page_11
	   | (SOME (substitution_0 {page = page_12,...})) => page_12
	   | NONE => (raise (InternalError_0 "get_page")))
      fun get_name_0 (t_30, i_336 : int) =
	  (case peek_11 t_30 of
	     (SOME (transition_0 {name = name_66, page = page_13,...})) =>
	     (concat
	      [name_66,
	       "@(",
	       Int.toString i_336,
	       ":",
	       case get_page_0 page_13 of
		 {name = noName_724,...} => noName_724,
	       ")"])
	   | (SOME (substitution_0 {name = name_67, page = page_14,...})) =>
	     (concat
	      [name_67,
	       "@(",
	       Int.toString i_336,
	       ":",
	       case get_page_0 page_14 of
		 {name = noName_725,...} => noName_725,
	       ")"])
	   | NONE => (raise (InternalError_0 "get_name")))
      fun get_input_0 t_31 =
	  (case peek_11 t_31 of
	     (SOME (transition_0 {input = input_6,...})) =>
	     ((map (fn {place = noName_726,...} => noName_726)) input_6)
	   | _ => (raise (InternalError_0 "get_input")))
      fun get_output_0 t_32 =
	  (case peek_11 t_32 of
	     (SOME (transition_0 {output = output_6,...})) =>
	     ((map (fn {place = noName_727,...} => noName_727)) output_6)
	   | _ => (raise (InternalError_0 "get_output")))
      fun get_places_1 t_33 =
	  (case peek_11 t_33 of
	     (SOME (transition_0 {input = input_7, output = output_7,...})) =>
	     (((fold_30
		(fn ({place = place_1,...}, tail_60) =>
		    (place_1 :: tail_60)))
	       input_7)
	      ((map (fn {place = noName_728,...} => noName_728)) output_7))
	   | _ => (raise (InternalError_0 "get_places")))
      fun is_transition_0 t_34 =
	  (case peek_11 t_34 of
	     (SOME (transition_0 _)) => true
	   | (SOME _) => false
	   | NONE => (raise (InternalError_0 "is_transition")))
      val table_34 : (string, string) hash_table_0 =
	  (mkTable_0 hashString_1)
	  (cs_hash_size_0, InternalError_0 "IMSTable.find")
      val insert_11 = insert_4 table_34
      val remove_9 = remove_2 table_34
      val find_10 = lookup_0 table_34
      val peek_12 = find_3 table_34
      fun eq_7 (x_213 : string, y_46) = (x_213 = y_46)
      val hashString_2 = (hashString_0, eq_7)
      val hash_val_0 = 31
      fun hashVal_0 s_136 =
	  (Word.andb (Word.fromInt s_136, Word.fromInt hash_val_0))
      fun sameKey_0 (s_137, s'_5) = (s_137 = s'_5)
      datatype 'a hash_table_1 =
	       HT_1 of {n_items : int ref,
			not_found : exn,
			table : (int, 'a) bucket_0 array ref}
      fun index_3 (i_337, sz_39) =
	  (Word.toIntX (Word.andb (i_337, (Word.fromInt sz_39) - 0w1)))
      fun mkTable_1 (sizeHint_2, notFound_1) =
	  (HT_1
	   {n_items = ref 0,
	    not_found = notFound_1,
	    table = ref (alloc_0 sizeHint_2)})
      fun insert_12 noName_729 =
	  (fn noName_730 =>
	      (case (noName_729, noName_730) of
		 (tbl_2 as HT_1 {n_items = n_items_8, table = table_35,...},
		  (key_14, item_20)) =>
		 let val arr_82 = ! table_35
		     val sz_40 = Array.length arr_82
		     val hash_14 = hashVal_0 key_14
		     val indx_6 = index_3 (hash_14, sz_40)
		     fun look_5 NIL_0 =
			 (Array.update
			  (arr_82,
			   indx_6,
			   B_1
			   (hash_14,
			    key_14,
			    item_20,
			    Array.sub (arr_82, indx_6))) ;
			  n_items_8 := ((! n_items_8) + 1) ;
			  growTableIfNeeded_0 (table_35, ! n_items_8) ;
			  NIL_0)
		       | look_5 (B_1 (h_13, k_74, v_52, r_87)) =
			 (if (hash_14 = h_13)
			     andalso (sameKey_0 (key_14, k_74))
			  then B_1 (hash_14, key_14, item_20, r_87)
			  else case look_5 r_87 of
				 NIL_0 => NIL_0
			       | rest_47 => (B_1 (h_13, k_74, v_52, rest_47)))
		 in case look_5 (Array.sub (arr_82, indx_6)) of
		      NIL_0 => ()
		    | b_94 => (Array.update (arr_82, indx_6, b_94))
		 end))
      fun lookup_1 noName_731 =
	  (fn noName_732 =>
	      (case (noName_731, noName_732) of
		 (HT_1 {not_found = not_found_5, table = table_36,...},
		  key_15) =>
		 let val arr_83 = ! table_36
		     val hash_15 = hashVal_0 key_15
		     val indx_7 = index_3 (hash_15, Array.length arr_83)
		     fun look_6 NIL_0 = (raise not_found_5)
		       | look_6 (B_1 (h_14, k_75, v_53, r_88)) =
			 (if (hash_15 = h_14)
			     andalso (sameKey_0 (key_15, k_75))
			  then v_53
			  else look_6 r_88)
		 in look_6 (Array.sub (arr_83, indx_7))
		 end))
      fun find_11 noName_733 =
	  (fn noName_734 =>
	      (case (noName_733, noName_734) of
		 (HT_1 {table = table_37,...}, key_16) =>
		 let val arr_84 = ! table_37
		     val sz_41 = Array.length arr_84
		     val hash_16 = hashVal_0 key_16
		     val indx_8 = index_3 (hash_16, sz_41)
		     fun look_7 NIL_0 = NONE
		       | look_7 (B_1 (h_15, k_76, v_54, r_89)) =
			 (if (hash_16 = h_15)
			     andalso (sameKey_0 (key_16, k_76))
			  then SOME v_54
			  else look_7 r_89)
		 in look_7 (Array.sub (arr_84, indx_8))
		 end))
      fun remove_10 noName_735 =
	  (fn noName_736 =>
	      (case (noName_735, noName_736) of
		 (HT_1
		  {n_items = n_items_9,
		   not_found = not_found_6,
		   table = table_38},
		  key_17) =>
		 let val arr_85 = ! table_38
		     val sz_42 = Array.length arr_85
		     val hash_17 = hashVal_0 key_17
		     val indx_9 = index_3 (hash_17, sz_42)
		     fun look_8 NIL_0 = (raise not_found_6)
		       | look_8 (B_1 (h_16, k_77, v_55, r_90)) =
			 (if (hash_17 = h_16)
			     andalso (sameKey_0 (key_17, k_77))
			  then (v_55, r_90)
			  else let val (item_21, r'_2) = look_8 r_90
			       in (item_21, B_1 (h_16, k_77, v_55, r'_2))
			       end)
		     val (item_22, bucket_1) =
			 look_8 (Array.sub (arr_85, indx_9))
		 in (Array.update (arr_85, indx_9, bucket_1) ;
		     n_items_9 := ((! n_items_9) - 1) ;
		     item_22)
		 end))
      fun numItems_1 (HT_1 {n_items = n_items_10,...}) = (! n_items_10)
      fun listItems_2 (HT_1 {n_items = n_items_11, table = ref arr_86,...}) =
	  (listItems_0 (arr_86, n_items_11))
      fun listItemsi_2 (HT_1 {n_items = n_items_12, table = ref arr_87,...})

      =
	  (listItemsi_0 (arr_87, n_items_12))
      fun appi_11 noName_737 =
	  (fn noName_738 =>
	      (case (noName_737, noName_738) of
		 (f_262, HT_1 {table = table_39,...}) =>
		 ((appi_9 f_262) (! table_39))))
      fun app_33 noName_739 =
	  (fn noName_740 =>
	      (case (noName_739, noName_740) of
		 (f_263, HT_1 {table = table_40,...}) =>
		 ((app_28 f_263) (! table_40))))
      fun mapi_6 noName_741 =
	  (fn noName_742 =>
	      (case (noName_741, noName_742) of
		 (f_264,
		  HT_1
		  {n_items = n_items_13,
		   not_found = not_found_7,
		   table = table_41}) =>
		 (HT_1
		  {n_items = ref (! n_items_13),
		   not_found = not_found_7,
		   table = ref ((mapi_4 f_264) (! table_41))})))
      fun map_12 noName_743 =
	  (fn noName_744 =>
	      (case (noName_743, noName_744) of
		 (f_265,
		  HT_1
		  {n_items = n_items_14,
		   not_found = not_found_8,
		   table = table_42}) =>
		 (HT_1
		  {n_items = ref (! n_items_14),
		   not_found = not_found_8,
		   table = ref ((map_9 f_265) (! table_42))})))
      fun foldi_3 noName_745 =
	  (fn noName_746 =>
	      (fn noName_747 =>
		  (case (noName_745, noName_746, noName_747) of
		     (f_266, init_52, HT_1 {table = table_43,...}) =>
		     (((foldi_1 f_266) init_52) (! table_43)))))
      fun fold_36 noName_748 =
	  (fn noName_749 =>
	      (fn noName_750 =>
		  (case (noName_748, noName_749, noName_750) of
		     (f_267, init_53, HT_1 {table = table_44,...}) =>
		     (((fold_31 f_267) init_53) (! table_44)))))
      fun filteri_2 noName_751 =
	  (fn noName_752 =>
	      (case (noName_751, noName_752) of
		 (pred_21, HT_1 {table = table_45,...}) =>
		 ((filteri_0 pred_21) (! table_45))))
      fun filter_7 noName_753 =
	  (fn noName_754 =>
	      (case (noName_753, noName_754) of
		 (pred_22, HT_1 {table = table_46,...}) =>
		 ((filter_3 pred_22) (! table_46))))
      fun copy_14 (HT_1
		   {n_items = n_items_15,
		    not_found = not_found_9,
		    table = table_47}) =
	  (HT_1
	   {n_items = ref (! n_items_15),
	    not_found = not_found_9,
	    table = ref (copy_12 (! table_47))})
      fun bucketSizes_2 (HT_1 {table = table_48,...}) =
	  (bucketSizes_0 (! table_48))
      val hash_val_1 = 31
      fun hashVal_1 ([] : int list) = (Word.fromInt 0)
	| hashVal_1 (x_214 :: xs_26) =
	  (Word.andb
	   (Word.fromInt (x_214 * (Word.toInt (hashVal_1 xs_26))),
	    Word.fromInt hash_val_1))
      fun sameKey_1 (s_138, s'_6) = (s_138 = s'_6)
      datatype 'a hash_table_2 =
	       HT_2 of {n_items : int ref,
			not_found : exn,
			table : (int list, 'a) bucket_0 array ref}
      fun index_4 (i_338, sz_43) =
	  (Word.toIntX (Word.andb (i_338, (Word.fromInt sz_43) - 0w1)))
      fun mkTable_2 (sizeHint_3, notFound_2) =
	  (HT_2
	   {n_items = ref 0,
	    not_found = notFound_2,
	    table = ref (alloc_0 sizeHint_3)})
      fun insert_13 noName_755 =
	  (fn noName_756 =>
	      (case (noName_755, noName_756) of
		 (tbl_3
		  as HT_2 {n_items = n_items_16, table = table_49,...},
		  (key_18, item_23)) =>
		 let val arr_88 = ! table_49
		     val sz_44 = Array.length arr_88
		     val hash_18 = hashVal_1 key_18
		     val indx_10 = index_4 (hash_18, sz_44)
		     fun look_9 NIL_0 =
			 (Array.update
			  (arr_88,
			   indx_10,
			   B_1
			   (hash_18,
			    key_18,
			    item_23,
			    Array.sub (arr_88, indx_10))) ;
			  n_items_16 := ((! n_items_16) + 1) ;
			  growTableIfNeeded_0 (table_49, ! n_items_16) ;
			  NIL_0)
		       | look_9 (B_1 (h_17, k_78, v_56, r_91)) =
			 (if (hash_18 = h_17)
			     andalso (sameKey_1 (key_18, k_78))
			  then B_1 (hash_18, key_18, item_23, r_91)
			  else case look_9 r_91 of
				 NIL_0 => NIL_0
			       | rest_48 => (B_1 (h_17, k_78, v_56, rest_48)))
		 in case look_9 (Array.sub (arr_88, indx_10)) of
		      NIL_0 => ()
		    | b_95 => (Array.update (arr_88, indx_10, b_95))
		 end))
      fun lookup_2 noName_757 =
	  (fn noName_758 =>
	      (case (noName_757, noName_758) of
		 (HT_2 {not_found = not_found_10, table = table_50,...},
		  key_19) =>
		 let val arr_89 = ! table_50
		     val hash_19 = hashVal_1 key_19
		     val indx_11 = index_4 (hash_19, Array.length arr_89)
		     fun look_10 NIL_0 = (raise not_found_10)
		       | look_10 (B_1 (h_18, k_79, v_57, r_92)) =
			 (if (hash_19 = h_18)
			     andalso (sameKey_1 (key_19, k_79))
			  then v_57
			  else look_10 r_92)
		 in look_10 (Array.sub (arr_89, indx_11))
		 end))
      fun find_12 noName_759 =
	  (fn noName_760 =>
	      (case (noName_759, noName_760) of
		 (HT_2 {table = table_51,...}, key_20) =>
		 let val arr_90 = ! table_51
		     val sz_45 = Array.length arr_90
		     val hash_20 = hashVal_1 key_20
		     val indx_12 = index_4 (hash_20, sz_45)
		     fun look_11 NIL_0 = NONE
		       | look_11 (B_1 (h_19, k_80, v_58, r_93)) =
			 (if (hash_20 = h_19)
			     andalso (sameKey_1 (key_20, k_80))
			  then SOME v_58
			  else look_11 r_93)
		 in look_11 (Array.sub (arr_90, indx_12))
		 end))
      fun remove_11 noName_761 =
	  (fn noName_762 =>
	      (case (noName_761, noName_762) of
		 (HT_2
		  {n_items = n_items_17,
		   not_found = not_found_11,
		   table = table_52},
		  key_21) =>
		 let val arr_91 = ! table_52
		     val sz_46 = Array.length arr_91
		     val hash_21 = hashVal_1 key_21
		     val indx_13 = index_4 (hash_21, sz_46)
		     fun look_12 NIL_0 = (raise not_found_11)
		       | look_12 (B_1 (h_20, k_81, v_59, r_94)) =
			 (if (hash_21 = h_20)
			     andalso (sameKey_1 (key_21, k_81))
			  then (v_59, r_94)
			  else let val (item_24, r'_3) = look_12 r_94
			       in (item_24, B_1 (h_20, k_81, v_59, r'_3))
			       end)
		     val (item_25, bucket_2) =
			 look_12 (Array.sub (arr_91, indx_13))
		 in (Array.update (arr_91, indx_13, bucket_2) ;
		     n_items_17 := ((! n_items_17) - 1) ;
		     item_25)
		 end))
      fun numItems_2 (HT_2 {n_items = n_items_18,...}) = (! n_items_18)
      fun listItems_3 (HT_2 {n_items = n_items_19, table = ref arr_92,...}) =
	  (listItems_0 (arr_92, n_items_19))
      fun listItemsi_3 (HT_2 {n_items = n_items_20, table = ref arr_93,...})

      =
	  (listItemsi_0 (arr_93, n_items_20))
      fun appi_12 noName_763 =
	  (fn noName_764 =>
	      (case (noName_763, noName_764) of
		 (f_268, HT_2 {table = table_53,...}) =>
		 ((appi_9 f_268) (! table_53))))
      fun app_34 noName_765 =
	  (fn noName_766 =>
	      (case (noName_765, noName_766) of
		 (f_269, HT_2 {table = table_54,...}) =>
		 ((app_28 f_269) (! table_54))))
      fun mapi_7 noName_767 =
	  (fn noName_768 =>
	      (case (noName_767, noName_768) of
		 (f_270,
		  HT_2
		  {n_items = n_items_21,
		   not_found = not_found_12,
		   table = table_55}) =>
		 (HT_2
		  {n_items = ref (! n_items_21),
		   not_found = not_found_12,
		   table = ref ((mapi_4 f_270) (! table_55))})))
      fun map_13 noName_769 =
	  (fn noName_770 =>
	      (case (noName_769, noName_770) of
		 (f_271,
		  HT_2
		  {n_items = n_items_22,
		   not_found = not_found_13,
		   table = table_56}) =>
		 (HT_2
		  {n_items = ref (! n_items_22),
		   not_found = not_found_13,
		   table = ref ((map_9 f_271) (! table_56))})))
      fun foldi_4 noName_771 =
	  (fn noName_772 =>
	      (fn noName_773 =>
		  (case (noName_771, noName_772, noName_773) of
		     (f_272, init_54, HT_2 {table = table_57,...}) =>
		     (((foldi_1 f_272) init_54) (! table_57)))))
      fun fold_37 noName_774 =
	  (fn noName_775 =>
	      (fn noName_776 =>
		  (case (noName_774, noName_775, noName_776) of
		     (f_273, init_55, HT_2 {table = table_58,...}) =>
		     (((fold_31 f_273) init_55) (! table_58)))))
      fun filteri_3 noName_777 =
	  (fn noName_778 =>
	      (case (noName_777, noName_778) of
		 (pred_23, HT_2 {table = table_59,...}) =>
		 ((filteri_0 pred_23) (! table_59))))
      fun filter_8 noName_779 =
	  (fn noName_780 =>
	      (case (noName_779, noName_780) of
		 (pred_24, HT_2 {table = table_60,...}) =>
		 ((filter_3 pred_24) (! table_60))))
      fun copy_15 (HT_2
		   {n_items = n_items_23,
		    not_found = not_found_14,
		    table = table_61}) =
	  (HT_2
	   {n_items = ref (! n_items_23),
	    not_found = not_found_14,
	    table = ref (copy_12 (! table_61))})
      fun bucketSizes_3 (HT_2 {table = table_62,...}) =
	  (bucketSizes_0 (! table_62))
      val init_56 = ref (fn () => ())
      val instances_0 :
	  (string, {print : (int -> string), size : (int -> int)}) hash_table_0

      =
	  (mkTable_0 hashId_0) (64, InternalError_0 "Place.instances")
      fun print_mark_0 (p_41 : string, i_339 : int) =
	  ((case (lookup_0 instances_0) p_41 of
	      {print = noName_781,...} => noName_781)
	   i_339)
      fun size_mark_0 (p_42 : string, i_340 : int) =
	  ((case (lookup_0 instances_0) p_42 of
	      {size = noName_782,...} => noName_782)
	   i_340)
      val init_mark_funs_0 = ref ([] : (unit -> unit) list)
      fun set_init_mark_0 () =
	  ((List.app (fn f_274 => (f_274 ()))) (! init_mark_funs_0))
      fun change_mark_0 (p_43, i_341 : int, toinit_0) =
	  let val (name_69, ims_2, cs_84, im_1) =
		  case peek_10 p_43 of
		    (SOME
		     {ext = {cs = cs_83, im = im_0,...},
		      int = SOME {ims = ims_1, name = name_68,...},...}) =>
		    (name_68, ims_1, cs_83, im_0)
		  | _ => (raise (InternalError_0 "change_mark"))
	      val mark_0 =
		  if toinit_0
		  then ("!" ^ name_69) ^ ".init_mark"
		  else "CPN'marking"
	      val inst_0 = Int.toString i_341
	  in if is_timed_0 cs_84
	     then ((useStream_0 o openString_1) o concat)
		  ["val _ = (",
		   ims_2,
		   ".init(",
		   name_69,
		   ".wait ",
		   inst_0,
		   ",",
		   mark_0,
		   "); ",
		   ims_2,
		   ".pims.init(",
		   name_69,
		   ".mark ",
		   inst_0,
		   ",nil));"]
	     else ((useStream_0 o openString_1) o concat)
		  ["val _ = ",
		   ims_2,
		   ".init(",
		   name_69,
		   ".mark ",
		   inst_0,
		   ",",
		   mark_0,
		   ");"]
	  end
      val PAUSEBEFORE_0 = 10
      val PAUSEAFTER_0 = 20
      val SHOWMARKINGS_0 = 30
      val SHOWTOKENS_0 = 40
      val ENDPAUSE_0 = 50
      val MANBIND_0 = 60
      val response_1 = response_0
      fun add_ints_0 noName_783 =
	  (fn noName_784 =>
	      (case (noName_783, noName_784) of
		 ((blist_1, ilist_1, slist_1), is_0) =>
		 (blist_1, List.@ (is_0, ilist_1), slist_1)))
      fun unwrap_pause_0 (blist_2, np_0 :: (na_0 :: ilist_2), slist_2) =
	  let fun unwrap_0 noName_785 =
		  (fn noName_786 =>
		      (case (noName_785, noName_786) of
			 (bislists_0, (0, res_1)) => (res_1, bislists_0)
		       | ((b_96 :: blist_3, ilist_3, s_139 :: slist_3),
			  (i_342, res_2)) =>
			 ((unwrap_0 (blist_3, ilist_3, slist_3))
			  (i_342 - 1, (s_139, b_96) :: res_2))
		       | (_, _) =>
			 (raise (InternalError_0 "Response.pause_unwrap"))))
	      val (places_3, bislists_1) =
		  (unwrap_0 (blist_2, ilist_2, slist_2)) (np_0, [])
	      val (arcs_12, _) = (unwrap_0 bislists_1) (na_0, [])
	  in (places_3, arcs_12)
	  end
	| unwrap_pause_0 _ = (raise (InternalError_0 "Response.pause_unwrap"))
      fun pause_before_0 (t_35, i_343) =
	  (unwrap_pause_0 (response_1 ([], [PAUSEBEFORE_0, i_343], [t_35])))
      fun pause_after_0 (t_36, i_344) =
	  (unwrap_pause_0 (response_1 ([], [PAUSEAFTER_0, i_344], [t_36])))
      fun wrap_3 [] = ([], [], [])
	| wrap_3 ((size_12, string_1) :: xs_27) =
	  let val (blist_4, ilist_4, slist_4) = wrap_3 xs_27
	  in (blist_4, size_12 :: ilist_4, string_1 :: slist_4)
	  end
      fun show_markings_0 marks_0 =
	  (response_1
	   ((add_ints_0 (wrap_3 marks_0)) [SHOWMARKINGS_0, length marks_0]) ;
	   ())
      fun show_tokens_0 tokens_2 =
	  (response_1
	   ((add_ints_0 (wrap_3 tokens_2)) [SHOWTOKENS_0, length tokens_2]) ;
	   ())
      fun end_pause_0 () = (response_1 ([], [], []) ; ())
      fun man_bind_0 groups_1 =
	  let fun wrap_slist_0 noName_787 =
		  (fn noName_788 =>
		      (case (noName_787, noName_788) of
			 ((blist_5, ilist_5, slist_5), slist'_0) =>
			 (blist_5,
			  (length slist'_0) :: ilist_5,
			  List.@ (slist'_0, slist_5))))
	      fun wrap_slists_0 noName_791 =
		  (fn noName_792 =>
		      (case (noName_791, noName_792) of
			 (bislists_2, slists_0) =>
			 let fun wrap_4 noName_789 =
				 (fn noName_790 =>
				     (case (noName_789, noName_790) of
					(bislists_3, []) => bislists_3
				      | (bislists_4, slist_6 :: slists_1) =>
					let val bislists_5 =
						(wrap_4 bislists_4) slists_1
					in (wrap_slist_0 bislists_5) slist_6
					end))
			 in (add_ints_0 ((wrap_4 bislists_2) slists_0))
			    [length slists_0]
			 end))
	      fun wrap_groups_0 groups_2 =
		  let fun wrap_5 noName_793 =
			  (fn noName_794 =>
			      (case (noName_793, noName_794) of
				 (bislists_6, []) => bislists_6
			       | (bislists_7, (vars_1, binds_0) :: groups_3) =>
				 let val bislists_8 =
					 (wrap_5 bislists_7) groups_3
				 in (wrap_slist_0
				     ((wrap_slists_0 bislists_8) binds_0))
				    vars_1
				 end))
		  in (add_ints_0 ((wrap_5 ([], [], [])) groups_2))
		     [length groups_2]
		  end
	  in case response_1
		  ((add_ints_0 (wrap_groups_0 groups_1)) [MANBIND_0]) of
	       ([], _ :: ilist_6, []) => ilist_6
	     | _ => (raise (InternalError_0 "Response.man_bind"))
	  end
      val CPN'fold_1 = fold_30
      val CPN'concat_1 = concat
      val tod_0 = fn () => (Int.fromLarge (Time.toSeconds (Time.now ())))
      val list_to_ms_0 = fn x_215 => x_215
      fun CPN'bootstrap_0 (timetype_1, starttime_1, _) =
	  (case timetype_1 of
	     "" =>
	     (use_string_1
	      ["\n structure CPN'Time = CPN'UnitTime;\n val ignore = CPN'Time.null; (* NEW *)"

     ])
	   | "int" =>
	     (use_string_1
	      ["\n structure CPN'Time = CPN'IntTime (val start = ",
	       starttime_1,
	       ");\n val ignore = CPN'Time.null; (* NEW *)\n structure CPN'TMS = CPN'CreateTMS (structure Time = CPN'Time);\n structure TMS = CPN'TMS open CPN'Time TMS;"

     ])
	   | "real" =>
	     (use_string_1
	      ["\n structure CPN'Time = CPN'RealTime (val start = ",
	       starttime_1,
	       ");\n val ignore = CPN'Time.null; (* NEW *)\n structure CPN'TMS = CPN'CreateTMS (structure Time = CPN'Time);\n structure TMS = CPN'TMS\n open CPN'Time TMS;"

     ])
	   | _ => (raise (InternalError_0 "illegal time")) ;
	   use_string_1
	   ["\n structure CPN'RepTable = CPN'CreateRepTable (structure Time = CPN'Time);\n structure CPN'InstTable = CPN'CreateInstTable (structure RepTable = CPN'RepTable);\n structure CPN'SyntaxCheck = CPN'MakeSyntaxCheck (structure RepTable = CPN'RepTable);\n structure CPN'Options = CPN'CreateOptions (structure Time = CPN'Time);\n structure CPN'ColorSets = CPN'CreateColorSets (structure Time = CPN'Time);\n structure CPN'Decl = CPN'CreateDecl (structure CS = CPN'ColorSets);\n structure CPN'Place = CPN'Places;\n structure CPN'PlaceSim = CPN'CreatePlace (structure InstTable = CPN'InstTable);\n structure CPN'Reference = CPN'References;\n structure CPN'ReferenceSim = CPN'CreateReference (structure InstTable = CPN'InstTable);\n structure CPN'Transition = CPN'CreateTransition (structure InstTable = CPN'InstTable and Decl = CPN'Decl);\n structure CPN'Sim = CPN'MakeSim (structure Options = CPN'Options and InstTable = CPN'InstTable and Places = CPN'Place and References = CPN'Reference);\n val step = CPN'Sim.step;\n val inst = CPN'Sim.inst;\n structure CPN'SimGlue = CPN'CreateSimGlue (structure Options = CPN'Options and Decl = CPN'Decl and SyntaxCheck = CPN'SyntaxCheck and InstTable = CPN'InstTable and Sim = CPN'Sim and Place = CPN'PlaceSim and Reference = CPN'ReferenceSim and Transition = CPN'Transition);\n val _ = CPN'Env.init();"

     ] ;
	   ([], [], []))
      val _ = NSBootstrap_0 := CPN'bootstrap_0
      val ignore_1 = null_2
      val order_table_0 : (string, (string * int list) list) hash_table_0 =
	  (mkTable_0 hashId_0)
	  (node_hash_size_0, InternalError_0 "RepTable.find order_table")
      val offset_table_0 : (string, (string * unit option) list) hash_table_0 =
	  (mkTable_0 hashId_0)
	  (node_hash_size_0, InternalError_0 "RepTable.find offset_table")
      val dep_table_0 : (string, string list) hash_table_0 =
	  (mkTable_0 hashId_0)
	  (node_hash_size_0, InternalError_0 "RepTable.find dep_table")
      fun ins_4 (t_37, x_216, []) = [(t_37, x_216)]
	| ins_4 (t_38, x_217, (t'_1, x'_0) :: xs_28) =
	  (if eq_3 (t_38, t'_1)
	   then (t_38, x_217) :: xs_28
	   else (t'_1, x'_0) :: (ins_4 (t_38, x_217, xs_28)))
      fun append_order_0 (p_44, t_39, rep_11) =
	  let fun append_3 id_28 =
		  (case (find_3 order_table_0) id_28 of
		     NONE =>
		     ((insert_4 order_table_0) (id_28, [(t_39, rep_11)]))
		   | (SOME order_6) =>
		     ((insert_4 order_table_0)
		      (id_28, ins_4 (t_39, rep_11, order_6))))
	  in case peek_10 p_44 of
	       (SOME {kind = fusion_0 grp_1,...}) => (append_3 grp_1)
	     | (SOME _) => (append_3 p_44)
	     | _ => (raise (InternalError_0 "append_order"))
	  end
      fun append_offset_0 (p_45, t_40, tv_0) =
	  let fun append_4 id_29 =
		  (case (find_3 offset_table_0) id_29 of
		     NONE =>
		     ((insert_4 offset_table_0) (id_29, [(t_40, tv_0)]))
		   | (SOME offset_10) =>
		     ((insert_4 offset_table_0)
		      (id_29, ins_4 (t_40, tv_0, offset_10))))
	  in case peek_10 p_45 of
	       (SOME {kind = fusion_0 grp_2,...}) => (append_4 grp_2)
	     | (SOME _) => (append_4 p_45)
	     | _ => (raise (InternalError_0 "append_offset"))
	  end
      fun append_dep_0 (p_46, t_41) =
	  let fun ins_5 [] = [t_41]
		| ins_5 (t'_2 :: ts_2) =
		  (if eq_3 (t_41, t'_2)
		   then t_41 :: ts_2
		   else t'_2 :: (ins_5 ts_2))
	      fun append_5 id_30 =
		  (case (find_3 dep_table_0) id_30 of
		     NONE => ((insert_4 dep_table_0) (id_30, [t_41]))
		   | (SOME dep_0) =>
		     ((insert_4 dep_table_0) (id_30, ins_5 dep_0)))
	  in case peek_10 p_46 of
	       (SOME {kind = fusion_0 grp_3,...}) =>
	       (append_dep_0 (grp_3, t_41))
	     | (SOME _) => (append_5 p_46)
	     | _ => (raise (InternalError_0 "append_dep"))
	  end
      val rm_order_0 = fn id_31 => ((remove_2 order_table_0) id_31 ; ())
      val rm_offset_0 = fn id_32 => ((remove_2 offset_table_0) id_32 ; ())
      val rm_dep_0 = fn id_33 => ((remove_2 dep_table_0) id_33 ; ())
      val find_order_0 = lookup_0 order_table_0
      val find_offset_0 = lookup_0 offset_table_0
      val find_dep_0 = lookup_0 dep_table_0
      val peek_order_0 = find_3 order_table_0
      val peek_offset_0 = find_3 offset_table_0
      val peek_dep_0 = find_3 dep_table_0
      fun remove_12 id_34 =
	  (case peek_order_0 id_34 of
	     (SOME _) => (rm_order_0 id_34) | _ => () ;
	   case peek_offset_0 id_34 of
	     (SOME _) => (rm_offset_0 id_34) | _ => () ;
	   case peek_dep_0 id_34 of
	     (SOME _) => (rm_dep_0 id_34) | _ => ())
      fun rm_trans_0 (p_47, t_42) =
	  let fun rm_3 noName_795 =
		  (fn noName_796 =>
		      (case (noName_795, noName_796) of
			 (_, []) => []
		       | (f_275, t'_3 :: xs_29) =>
			 (if eq_3 (t_42, f_275 t'_3)
			  then xs_29
			  else t'_3 :: ((rm_3 f_275) xs_29))))
	  in (case peek_order_0 p_47 of
		(SOME l_115) =>
		((insert_4 order_table_0)
		 (p_47,
		  (rm_3 (fn {1 = noName_797,...} => noName_797)) l_115))
	      | _ => () ;
	      case peek_offset_0 p_47 of
		(SOME l_116) =>
		((insert_4 offset_table_0)
		 (p_47,
		  (rm_3 (fn {1 = noName_798,...} => noName_798)) l_116))
	      | _ => () ;
	      case peek_dep_0 p_47 of
		(SOME l_117) =>
		((insert_4 dep_table_0)
		 (p_47, (rm_3 (fn x_218 => x_218)) l_117))
	      | _ => ())
	  end
      fun get_order_0 p_48 =
	  let val p_49 =
		  case peek_10 p_48 of
		    (SOME {kind = fusion_0 grp_4,...}) => grp_4
		  | _ => p_48
	  in case peek_order_0 p_49 of
	       (SOME item_26) =>
	       ((map (fn {2 = noName_799,...} => noName_799)) item_26)
	     | NONE => []
	  end
      fun get_offset_0 p_50 =
	  let val p_51 =
		  case peek_10 p_50 of
		    (SOME {kind = fusion_0 grp_5,...}) => grp_5
		  | _ => p_50
	  in case peek_offset_0 p_51 of
	       (SOME item_27) =>
	       ((map (fn {2 = noName_800,...} => noName_800)) item_27)
	     | NONE => []
	  end
      fun get_dep_0 p_52 =
	  let val p_53 =
		  case peek_10 p_52 of
		    (SOME {kind = fusion_0 grp_6,...}) => grp_6
		  | _ => p_52
	  in case peek_dep_0 p_53 of
	       (SOME item_28) => item_28 | NONE => []
	  end
      datatype offset_type_0 =
	       one_offset_0 of string
	     | max_offset_0 of string | var_offset_0 | un_timed_0
      val transition_table_0 : (string, (int * int)) hash_table_0 =
	  (mkTable_0 hashId_0)
	  (node_hash_size_0, InternalError_0 "TransitionInstTable.find")
      val substitution_table_0 : (string, (int * int)) hash_table_0 =
	  (mkTable_0 hashId_0)
	  (page_hash_size_0, InternalError_0 "SubstitutionInstTable.find")
      val page_table_0 :
	  (string, (int * (string * string * int) option Array.array)) hash_table_0

      =
	  (mkTable_0 hashId_0)
	  (page_hash_size_0, InternalError_0 "PageInstTable.find")
      val place_table_0 : (string, (int * string * int) list) hash_table_0 =
	  (mkTable_0 hashId_0)
	  (node_hash_size_0, InternalError_0 "PlaceInstTable.find")
      val no_of_tis_0 = ref 0
      fun clean_1 () =
	  (no_of_tis_0 := 0 ;
	   (filter_4 (fn _ => false)) transition_table_0 ;
	   (filter_4 (fn _ => false)) substitution_table_0 ;
	   (filter_4 (fn _ => false)) page_table_0 ;
	   (filter_4 (fn _ => false)) place_table_0)
      fun get_no_of_inst_1 id_35 =
	  (case (lookup_0 page_table_0) id_35 of
	     {1 = noName_801,...} => noName_801)
      fun get_t_index_0 t_43 =
	  (case (find_3 transition_table_0) t_43 of
	     (SOME (m_53, n_232)) => (m_53, n_232)
	   | NONE =>
	     (raise (InternalError_0
		     (concat ["get_t_index ", toString_19 t_43, ")"]))))
      fun get_ti_index_0 (t_44, i_345) =
	  (case (find_3 transition_table_0) t_44 of
	     (SOME (n_233, _)) => ((n_233 + i_345) - 1)
	   | NONE =>
	     (raise (InternalError_0
		     (concat
		      ["get_ti_index(",
		       toString_19 t_44,
		       ",",
		       Int.toString i_345,
		       ")"]))))
      fun get_ti_indeces_0 () = (listItemsi_1 transition_table_0)
      fun init_57 () =
	  let fun init_s_0 [] = ()
		| init_s_0 ((sid_0, n_234) :: pages_9) =
		  ((insert_4 page_table_0)
		   (sid_0, (n_234, Array.array (n_234, NONE))) ;
		   init_s_0 pages_9)
	      val subpage_table_0 : (string, int) hash_table_0 =
		  (mkTable_0 hashId_0)
		  (page_hash_size_0, InternalError_0 "SubpageInstTable.find")
	      fun init_t_0 [] = ()
		| init_t_0 ((tid_0, transition_0 {page = page_15,...})
			    :: tail_61) =
		  let val no_1 = ! no_of_tis_0
		      val no'_0 = no_1 + (get_no_of_inst_1 page_15)
		  in (no_of_tis_0 := no'_0 ;
		      (insert_4 transition_table_0)
		      (tid_0, (no_1, no'_0 - 1)) ;
		      init_t_0 tail_61)
		  end
		| init_t_0 ((tid_1,
			     substitution_0
			     {page = page_16, subpage = subpage_0,...})
			    :: tail_62) =
		  let val n_235 = get_no_of_inst_1 page_16
		      val m_55 =
			  case (find_3 subpage_table_0) subpage_0 of
			    NONE => 0 | (SOME m_54) => m_54
		      val connections_0 =
			  case (lookup_0 page_table_0) subpage_0 of
			    {2 = noName_802,...} => noName_802
		      fun update_super_cons_0 0 = ()
			| update_super_cons_0 i_346 =
			  (Array.update
			   (connections_0,
			    (m_55 + i_346) - 1,
			    SOME (page_16, tid_1, i_346)) ;
			   update_super_cons_0 (i_346 - 1))
		  in ((insert_4 subpage_table_0) (subpage_0, n_235 + m_55) ;
		      (insert_4 substitution_table_0)
		      (tid_1, (m_55 + 1, m_55 + n_235)) ;
		      update_super_cons_0 n_235 ;
		      init_t_0 tail_62)
		  end
	      fun init_p_0 [] = ()
		| init_p_0 (pid_16 :: pids_0) =
		  let fun find_socket_inst_0 ([], _) = NONE
			| find_socket_inst_0 ((s_140, t_45) :: sockets_1,
					      i_347) =
			  let val (a_109, b_97) =
				  (lookup_0 substitution_table_0) t_45
			  in if (a_109 <= i_347) andalso (i_347 <= b_97)
			     then SOME (s_140, (i_347 - a_109) + 1)
			     else find_socket_inst_0 (sockets_1, i_347)
			  end
		      fun find_inst_cons_0 (_, 0, tail_63) = tail_63
			| find_inst_cons_0 ([], _, tail_64) = tail_64
			| find_inst_cons_0 (sockets_2, i_348, tail_65) =
			  (case find_socket_inst_0 (sockets_2, i_348) of
			     NONE =>
			     (find_inst_cons_0
			      (sockets_2, i_348 - 1, tail_65))
			   | (SOME (s_141, j_62)) =>
			     (case (find_3 place_table_0) s_141 of
				NONE =>
				((insert_4 place_table_0)
				 (s_141, [(j_62, pid_16, i_348)]))
			      | (SOME list_16) =>
				((insert_4 place_table_0)
				 (s_141, (j_62, pid_16, i_348) :: list_16)) ;
			      find_inst_cons_0
			      (sockets_2,
			       i_348 - 1,
			       (i_348, s_141, j_62) :: tail_65)))
		  in case peek_10 pid_16 of
		       (SOME
			{ext = {page = page_17,...},
			 kind = port_5 sockets_3,...}) =>
		       (case (find_3 place_table_0) pid_16 of
			  NONE =>
			  ((insert_4 place_table_0)
			   (pid_16,
			    find_inst_cons_0
			    (sockets_3, get_no_of_inst_1 page_17, [])))
			| (SOME list_17) =>
			  ((insert_4 place_table_0)
			   (pid_16,
			    find_inst_cons_0
			    (sockets_3, get_no_of_inst_1 page_17, list_17))) ;
			init_p_0 pids_0)
		     | (SOME _) => (init_p_0 pids_0)
		     | _ => (raise (InternalError_0 "init_p"))
		  end
	  in (clean_1 () ;
	      init_s_0 (get_all_no_of_inst_0 ()) ;
	      init_t_0 (list_sorted_by_name_0 ()) ;
	      init_p_0 (list_all_places_bottom_up_0 ()))
	  end
      fun get_socket_inst_list_0 p_54 =
	  let val sockets_5 =
		  case peek_10 p_54 of
		    (SOME {kind = port_5 sockets_4,...}) => sockets_4
		  | _ => (raise (InternalError_0 "get_socket_inst_list"))
	      val inst_cons_0 =
		  (sort_1
		   (fn ((a_110 : int, _, _), (b_98, _, _)) =>
		       (a_110 < b_98)))
		  ((lookup_0 place_table_0) p_54)
	      fun get_9 (inst_1, (i_349, s_142, j_63) :: cons_0) =
		  (if (inst_1 = i_349)
		      andalso ((List.exists
				(fn (s'_7, _) => (eq_3 (s'_7, s_142))))
			       sockets_5)
		   then (s_142, j_63) :: (get_9 (inst_1 + 1, cons_0))
		   else get_9 (inst_1, cons_0))
		| get_9 (_, []) = []
	  in if (length sockets_5) = (length inst_cons_0)
	     then (map (fn (i_350, s_143, j_64) => (s_143, j_64)))
		  inst_cons_0
	     else get_9 (1, inst_cons_0)
	  end
      fun get_inst_cons_0 ((place_2, inst_2), tail_66) =
	  let val peek_13 = find_3 place_table_0
	      fun get_10 ((place_3, inst_3),
			  SOME ((i_351, s_144, j_65) :: cons_1),
			  tail_67) =
		  let fun treated_0 (p_55, i_352 : int) =
			  ((List.exists
			    (fn (p'_0, i'_5) =>
				((i_352 = i'_5) andalso (eq_3 (p_55, p'_0))))

     )
			   tail_67)
		  in if (inst_3 = i_351)
			andalso (not (treated_0 (s_144, j_65)))
		     then get_10
			  ((place_3, inst_3),
			   SOME cons_1,
			   get_10
			   ((s_144, j_65),
			    peek_13 s_144,
			    (s_144, j_65) :: tail_67))
		     else get_10 ((place_3, inst_3), SOME cons_1, tail_67)
		  end
		| get_10 (_, _, tail_68) = tail_68
	  in get_10
	     ((place_2, inst_2),
	      peek_13 place_2,
	      (place_2, inst_2) :: tail_66)
	  end
      fun get_all_cons_0 (place_4, tail_69) =
	  let val peek_14 = find_3 place_table_0
	      fun get_11 (place_5, SOME ((_, s_145, _) :: cons_2), tail_70) =
		  let fun treated_1 p_56 =
			  ((List.exists (fn p'_1 => (eq_3 (p_56, p'_1))))
			   tail_70)
		  in if not (treated_1 s_145)
		     then get_11
			  (place_5,
			   SOME cons_2,
			   get_11 (s_145, peek_14 s_145, s_145 :: tail_70))
		     else get_11 (place_5, SOME cons_2, tail_70)
		  end
		| get_11 (_, _, tail_71) = tail_71
	  in get_11 (place_4, peek_14 place_4, place_4 :: tail_69)
	  end
      fun get_order_1 p_57 =
	  let val cs_order_0 =
		  get_cs_order_0
		  (case get_ext_0 p_57 of
		     {cs = noName_803,...} => noName_803)
	      val orders_0 =
		  Array.fromList
		  (((fold_30
		     (fn (s_146, tail_72) =>
			 (List.@ (get_order_0 s_146, tail_72))))
		    (get_all_cons_0 (p_57, [])))
		   [])
	      val n_236 = Array.length orders_0
	      val candidates_0 = Array.array (n_236, [cs_order_0])
	      fun subset_2 ([], _) = true
		| subset_2 (x_219 :: xs_30, ys_10) =
		  (((List.exists (fn y_47 => (x_219 = y_47))) ys_10)
		   andalso (subset_2 (xs_30, ys_10)))
	      fun make_0 (~1, _) = ()
		| make_0 (i_353, ~1) = (make_0 (i_353 - 1, n_236 - 1))
		| make_0 (i_354, j_66) =
		  (if subset_2
		      (Array.sub (orders_0, i_354),
		       Array.sub (orders_0, j_66))
		   then (Array.update
			 (candidates_0,
			  i_354,
			  (Array.sub (orders_0, j_66))
			  :: (Array.sub (candidates_0, i_354))) ;
			 make_0 (i_354, j_66 - 1))
		   else make_0 (i_354, j_66 - 1))
	      fun find_max_0 (~1, _, j_67) =
		  ((sort_1
		    (fn (a_111, b_99) =>
			((List.length a_111) < (List.length b_99))))
		   (Array.sub (candidates_0, j_67)))
		| find_max_0 (i_355, m_56, j_68) =
		  (if (List.length (Array.sub (candidates_0, i_355))) > m_56
		   then find_max_0
			(i_355 - 1,
			 List.length (Array.sub (candidates_0, i_355)),
			 i_355)
		   else find_max_0 (i_355 - 1, m_56, j_68))
	      fun extract_order_0 (order_7, []) = (rev order_7)
		| extract_order_0 (order_8, demand_0 :: demands_0) =
		  let fun add_unused_0 [] = order_8
			| add_unused_0 (d_38 :: ds_0) =
			  (if (List.exists (fn d'_0 => (d_38 = d'_0)))
			      order_8
			   then add_unused_0 ds_0
			   else d_38 :: (add_unused_0 ds_0))
		  in extract_order_0 (add_unused_0 demand_0, demands_0)
		  end
	  in if n_236 > 0
	     then (make_0 (n_236 - 1, n_236 - 1) ;
		   extract_order_0 ([], find_max_0 (n_236 - 1, 0, 0)))
	     else cs_order_0
	  end
      fun get_offset_1 p_58 =
	  let fun get_offset'_0 ([], _, res_3) = res_3
		| get_offset'_0 (NONE :: _, _, _) = var_offset_0
		| get_offset'_0 ((SOME t'_4) :: offs_0,
				 t_46,
				 max_offset_0 s_147) =
		  (if lt_3 (t_46, t'_4)
		   then get_offset'_0
			(offs_0, t'_4, max_offset_0 (mkstr_1 t'_4))
		   else get_offset'_0 (offs_0, t_46, max_offset_0 s_147))
		| get_offset'_0 ((SOME t'_5) :: offs_1,
				 t_47,
				 one_offset_0 s_148) =
		  (if lt_3 (t_47, t'_5)
		   then get_offset'_0
			(offs_1, t'_5, max_offset_0 (mkstr_1 t'_5))
		   else if lt_3 (t'_5, t_47)
			then get_offset'_0 (offs_1, t_47, max_offset_0 s_148)
			else get_offset'_0 (offs_1, t_47, one_offset_0 s_148))
		| get_offset'_0 _ = (raise Match)
	  in case ((fold_30
		    (fn (s_149, tail_73) =>
			(List.@ (get_offset_0 s_149, tail_73))))
		   (get_all_cons_0 (p_58, [])))
		  [] of
	       [] =>
	       (if is_timed_1 p_58
		then one_offset_0 null_str_0
		else un_timed_0)
	     | (NONE :: _) => var_offset_0
	     | ((SOME t_48) :: offsets_0) =>
	       (get_offset'_0 (offsets_0, t_48, one_offset_0 (mkstr_1 t_48)))
	  end
      fun get_input_places_0 (t_49, i_356) =
	  (((fold_30
	     (fn (p_59, tail_74) =>
		 (get_inst_cons_0 ((p_59, i_356), tail_74))))
	    (get_input_0 t_49))
	   [])
      fun get_output_places_0 (t_50, i_357) =
	  (((fold_30
	     (fn (p_60, tail_75) =>
		 (get_inst_cons_0 ((p_60, i_357), tail_75))))
	    (get_output_0 t_50))
	   [])
      fun get_sur_places_0 (t_51, i_358) =
	  (((fold_30
	     (fn (p_61, tail_76) =>
		 (get_inst_cons_0 ((p_61, i_358), tail_76))))
	    (get_places_1 t_51))
	   [])
      fun get_12 ((p_62, i_359), tail_77) =
	  (((fold_30
	     (fn (t_52, ts_3) => ((get_ti_index_0 (t_52, i_359)) :: ts_3)))
	    (get_dep_0 p_62))
	   tail_77)
      fun get_dep_trans_0 (p_63, i_360) =
	  ((unique_sort_0 Int.<)
	   (((fold_30 get_12) (get_inst_cons_0 ((p_63, i_360), []))) []))
      fun get_dep_list_0 (t_53, i_361) =
	  ((unique_sort_0 Int.<)
	   (((fold_30 get_12) (get_output_places_0 (t_53, i_361))) []))
      fun get_page_structure_0 () =
	  let val pages_10 = list_all_pages_top_down_0 ()
	      fun make_1 [] = []
		| make_1 (page_18 :: pages_11) =
		  let val (n_237, array_7) = (lookup_0 page_table_0) page_18
		      fun listofarray_0 i_362 =
			  (if i_362 < n_237
			   then (Array.sub (array_7, i_362))
				:: (listofarray_0 (i_362 + 1))
			   else [])
		  in (page_18, listofarray_0 0) :: (make_1 pages_11)
		  end
	  in make_1 pages_10
	  end
      datatype stop_crits_0 =
	       until_step_0 of int
	     | additional_steps_0 of int
	     | until_time_0 of unit
	     | additional_time_0 of unit
      val stop_crits_0 = ref ([] : stop_crits_0 list)
      val pause_before_step_0 = ref false
      val pause_after_step_0 = ref false
      val pause_show_tokens_0 = ref false
      val pause_cont_after_0 = ref (NONE : int option)
      val report_transitions_0 = ref false
      val report_bindings_0 = ref false
      val report_function_0 =
	  ref (NONE : ((string * int) -> string) option)
      val show_marking_0 = ref true
      val show_enabling_0 = ref true
      val seed_1 = ref (SOME 87)
      val _ = init_51 (! seed_1)
      val reset_ran_gen_0 = ref false
      val reset_ref_vars_0 = ref false
      fun set_sim_0 {pause =
		     (beforestep_0, afterstep_0, showtok_0, contafter_0),
		     report = (reptrans_0, repbinds_0, repfun_0),
		     show = (showmark_0, showenab_0),
		     stop_crit =
		     (untilstep_0, addsteps_0, untiltime_0, addtime_0)} =
	  (stop_crits_0 := [] ;
	   if untilstep_0 = ""
	   then ()
	   else operator_262
		(stop_crits_0,
		 until_step_0 (valOf (Int.fromString untilstep_0))) ;
	   if addsteps_0 = ""
	   then ()
	   else operator_262
		(stop_crits_0,
		 additional_steps_0 (valOf (Int.fromString addsteps_0))) ;
	   if (untiltime_0 = "") orelse (not (isSome start_time_0))
	   then ()
	   else operator_262
		(stop_crits_0, until_time_0 (maketime_0 untiltime_0)) ;
	   if (addtime_0 = "") orelse (not (isSome start_time_0))
	   then ()
	   else operator_262
		(stop_crits_0, additional_time_0 (maketime_0 addtime_0)) ;
	   pause_before_step_0 := beforestep_0 ;
	   pause_after_step_0 := afterstep_0 ;
	   pause_show_tokens_0 := showtok_0 ;
	   pause_cont_after_0 := (Int.fromString contafter_0) ;
	   report_transitions_0 := reptrans_0 ;
	   report_bindings_0 := (repbinds_0 andalso reptrans_0) ;
	   if repfun_0 = ""
	   then report_function_0 := NONE
	   else useStream_0
		(openString_1
		 (("CPN'Options.report_function:= SOME(" ^ repfun_0) ^ ")")) ;
	   show_marking_0 := showmark_0 ;
	   show_enabling_0 := showenab_0)
      fun set_init_0 {reset = (ran_gen_0, ref_vars_0), seed = no_2} =
	  (seed_1 := (if no_2 = "" then NONE else Int.fromString no_2) ;
	   init_51 (! seed_1) ;
	   reset_ran_gen_0 := ran_gen_0 ;
	   reset_ref_vars_0 := ref_vars_0)
      val out_of_range_0 = "Out of range!"
      fun error_not_use_0 (f_276, cs_85) =
	  (raise (CPN'Error_0
		  (concat
		   ["Error: function ",
		    f_276,
		    " can not be used in ",
		    cs_85,
		    " color-set!"])))
      fun error_not_decl_0 (f_277, cs_86) =
	  (raise (CPN'Error_0
		  (concat
		   ["Error: function ",
		    f_277,
		    " not declared in color-set ",
		    cs_86,
		    "!"])))
      fun error_ill_decl_0 (decl_7, cs_87) =
	  (raise (CPN'Error_0
		  (concat
		   ["Error: illegal declare clause ",
		    decl_7,
		    " used in color-set ",
		    cs_87,
		    "!\n"])))
      exception ErrorLowHigh_0 
      fun error_low_high_0 cs_88 =
	  (concat ["Error: in ", cs_88, " with low..high must low<=high!"])
      exception ErrorMinMax_0 
      fun error_min_max_0 cs_89 =
	  (concat ["Error: in ", cs_89, " length min..max must 0<=min<=max!"])
      exception ErrorNotChar_0 
      val error_not_char_0 = "Error: string must be a single char!"
      fun error_illegal_token_0 value_13 =
	  (concat ["Error: illegal token ", value_13, " in unit color-set"])
      val base_21 = 0
      fun all_3 () = (error_not_use_0 ("all", "int"))
      val lt_6 = Int.<
      val cmp_5 = Int.compare
      val mkstr_3 = Int.toString
      val mkstr_ms_1 : (int list -> string) = mkstr_ms_0 (mkstr_3, lt_6)
      val input_8 : (TextIO.instream -> int) =
	  (valOf o Int.fromString) o input_5
      fun output_8 (s_150, c_107) = (output_5 (s_150, mkstr_3 c_107))
      val input_ms_1 : (TextIO.instream -> int list) = input_ms_0 input_8
      val output_ms_1 : ((TextIO.outstream * int list) -> unit) =
	  output_ms_0 (output_8, lt_6)
      fun legal_0 _ = true
      fun size_13 () = (error_not_use_0 ("size", "int"))
      fun ord_6 _ = (error_not_use_0 ("ord", "int"))
      fun col_11 _ = (error_not_use_0 ("col", "int"))
      fun ran_0 () = (error_not_use_0 ("ran", "int"))
      val base_22 = 0.0
      fun all_4 () = (error_not_use_0 ("all", "real"))
      val lt_7 = Real.<
      val cmp_6 = Real.compare
      val mkstr_4 = Real.toString
      val mkstr_ms_2 : (real list -> string) = mkstr_ms_0 (mkstr_4, lt_7)
      val input_9 : (TextIO.instream -> real) =
	  (valOf o Real.fromString) o input_5
      fun output_9 (s_151, c_108) = (output_5 (s_151, mkstr_4 c_108))
      val input_ms_2 : (TextIO.instream -> real list) = input_ms_0 input_9
      val output_ms_2 : ((TextIO.outstream * real list) -> unit) =
	  output_ms_0 (output_9, lt_7)
      fun legal_1 _ = true
      fun size_14 () = (error_not_use_0 ("size", "real"))
      fun ord_7 _ = (error_not_use_0 ("ord", "real"))
      fun col_12 _ = (error_not_use_0 ("col", "real"))
      fun ran_1 () = (error_not_use_0 ("ran", "real"))
      val base_23 = Char.minChar
      val all_ref_0 = ref (NONE : char list option)
      fun all_5 () =
	  (case ! all_ref_0 of
	     (SOME ms_18) => ms_18
	   | NONE =>
	     let fun mk_1 (ch_4 : char) =
		     (if ch_4 < Char.maxChar
		      then ch_4 :: (mk_1 (Char.succ ch_4))
		      else [Char.maxChar])
		 val ms_19 = mk_1 Char.minChar
	     in (all_ref_0 := (SOME ms_19) ; ms_19)
	     end)
      val lt_8 = Char.<
      val cmp_7 = Char.compare
      val mkstr_5 = Char.toString
      val mkstr_ms_3 : (char list -> string) = mkstr_ms_0 (mkstr_5, lt_8)
      val input_10 : (TextIO.instream -> char) =
	  (valOf o Char.fromString) o input_5
      fun output_10 (s_152, c_109) = (output_5 (s_152, mkstr_5 c_109))
      val input_ms_3 : (TextIO.instream -> char list) = input_ms_0 input_10
      val output_ms_3 : ((TextIO.outstream * char list) -> unit) =
	  output_ms_0 (output_10, lt_8)
      fun legal_2 _ = true
      fun size_15 () = (Char.maxOrd + 1)
      val ord_8 = Char.ord
      val col_13 = Char.chr
      fun ran_2 () = (col_13 (int_1 (size_15 ())))
      val base_24 = ""
      fun all_6 () = (error_not_use_0 ("all", "string"))
      val lt_9 = String.<
      val cmp_8 = String.compare
      fun mkstr_6 s_153 = (concat ["\"", s_153, "\""])
      val mkstr_ms_4 : (string list -> string) = mkstr_ms_0 (mkstr_6, lt_9)
      val ord_quote_0 = Char.ord #"\""
      fun ordof_0 (s_154, i_363) = (Char.ord (String.sub (s_154, i_363)))
      fun input_11 s_155 =
	  let val token_0 = input_5 s_155
	      val n_238 = String.size token_0
	  in if ((n_238 >= 2)
		 andalso ((ordof_0 (token_0, 0)) = ord_quote_0))
		andalso ((ordof_0 (token_0, n_238 - 1)) = ord_quote_0)
	     then String.substring (token_0, 1, n_238 - 2)
	     else raise (IOError_0 (error_illegal_token_0 token_0))
	  end
      fun output_11 (s_156, c_110) = (output_5 (s_156, mkstr_6 c_110))
      val input_ms_4 : (TextIO.instream -> string list) =
	  input_ms_0 input_11
      val output_ms_4 : ((TextIO.outstream * string list) -> unit) =
	  output_ms_0 (output_11, lt_9)
      fun legal_3 _ = true
      fun size_16 () = (error_not_use_0 ("size", "string"))
      fun ord_9 _ = (error_not_use_0 ("ord", "string"))
      fun col_14 _ = (error_not_use_0 ("col", "string"))
      fun ran_3 () = (error_not_use_0 ("ran", "string"))
      exception EmptyRanSet_0 
      fun create_12 n_239 =
	  {index = Array.tabulate (n_239, fn i_364 => i_364),
	   set = Array.tabulate (n_239, fn i_365 => i_365),
	   size = n_239}
      fun insert_14 (rs_0
		     as ref {index = index_5, set = set_2, size = size_17},
		     i_366) =
	  (Array.update (set_2, size_17, i_366) ;
	   Array.update (index_5, i_366, size_17) ;
	   rs_0 := {index = index_5, set = set_2, size = size_17 + 1})
      fun delete_0 (rs_1
		    as ref {index = index_6, set = set_3, size = size_18},
		    i_367) =
	  let val j_69 = Array.sub (index_6, i_367)
	      val k_82 = Array.sub (set_3, size_18 - 1)
	  in (Array.update (set_3, j_69, k_82) ;
	      Array.update (index_6, k_82, j_69) ;
	      rs_1 := {index = index_6, set = set_3, size = size_18 - 1})
	  end
      fun ran_4 {size = 0,...} = (raise EmptyRanSet_0)
	| ran_4 {index = _, set = set_4, size = size_19} =
	  (Array.sub (set_4, int_1 size_19))
      val size_20 :
	  ({index : int Array.array, set : int Array.array, size : int}
	   -> int) =
	  fn {size = noName_804,...} => noName_804
      fun lt_10 ((a_112, _), (b_100, _)) = (lt_3 (a_112, b_100))
      fun father_0 i_368 = ((i_368 - 1) div 2)
      fun lson_1 i_369 = ((2 * i_369) + 1)
      fun rson_1 i_370 = ((2 * 1) + 2)
      fun swap_0 (heap_0, index_7, i_371, j_70) =
	  let val itemi_0 as (_, ki_0) = Array.sub (heap_0, i_371)
	      val itemj_0 as (_, kj_0) = Array.sub (heap_0, j_70)
	  in (Array.update (heap_0, i_371, itemj_0) ;
	      Array.update (heap_0, j_70, itemi_0) ;
	      Array.update (index_7, ki_0, j_70) ;
	      Array.update (index_7, kj_0, i_371))
	  end
      fun down_0 noName_805 =
	  (fn noName_806 =>
	      (case (noName_805, noName_806) of
		 ((heap_1, index_8, last_4), node_0) =>
		 (if (lson_1 node_0) < last_4
		  then if lt_10
			  (Array.sub (heap_1, lson_1 node_0),
			   Array.sub (heap_1, rson_1 node_0))
		       then if lt_10
			       (Array.sub (heap_1, lson_1 node_0),
				Array.sub (heap_1, node_0))
			    then (swap_0
				  (heap_1, index_8, node_0, lson_1 node_0) ;
				  (down_0 (heap_1, index_8, last_4))
				  (lson_1 node_0))
			    else ()
		       else if lt_10
			       (Array.sub (heap_1, rson_1 node_0),
				Array.sub (heap_1, node_0))
			    then (swap_0
				  (heap_1, index_8, node_0, rson_1 node_0) ;
				  (down_0 (heap_1, index_8, last_4))
				  (rson_1 node_0))
			    else ()
		  else if (lson_1 node_0) = last_4
		       then if lt_10
			       (Array.sub (heap_1, lson_1 node_0),
				Array.sub (heap_1, node_0))
			    then swap_0
				 (heap_1, index_8, node_0, lson_1 node_0)
			    else ()
		       else ())))
      fun up_0 noName_807 =
	  (fn noName_808 =>
	      (case (noName_807, noName_808) of
		 (_, 0) => ()
	       | ((heap_2, index_9, last_5), node_1) =>
		 (if lt_10
		     (Array.sub (heap_2, node_1),
		      Array.sub (heap_2, father_0 node_1))
		  then (swap_0 (heap_2, index_9, node_1, father_0 node_1) ;
			(down_0 (heap_2, index_9, last_5)) (father_0 node_1) ;
			(up_0 (heap_2, index_9, last_5)) (father_0 node_1))
		  else ())))
      exception EmptyPQ_0 
      fun create_13 n_240 =
	  {heap = Array.tabulate (n_240, fn _ => (null_2, ~1)),
	   index = Array.tabulate (n_240, fn _ => ~1),
	   last = ~1}
      fun insert_15 (pq_0
		     as ref
			({heap = heap_3, index = index_10, last = last_6} :
			 {heap : (unit * int) Array.array,
			  index : int Array.array,
			  last : int}),
		     time_3,
		     i_372) =
	  let val last_7 = last_6 + 1
	  in (Array.update (heap_3, last_7, (time_3, i_372)) ;
	      Array.update (index_10, i_372, last_7) ;
	      (up_0 (heap_3, index_10, last_7)) last_7 ;
	      pq_0 := {heap = heap_3, index = index_10, last = last_7})
	  end
      fun min_12 ({last = ~1,...} :
		  {heap : (unit * int) Array.array,
		   index : int Array.array,
		   last : int}) =
	  (raise EmptyPQ_0)
	| min_12 {heap = heap_4,...} =
	  (case Array.sub (heap_4, 0) of
	     {1 = noName_809,...} => noName_809)
      fun deleteto_0 (ref
		      ({last = ~1,...} :
		       {heap : (unit * int) Array.array,
			index : int Array.array,
			last : int}),
		      _) =
	  []
	| deleteto_0 (pq_1
		      as ref
			 {heap = heap_5, index = index_11, last = last_8},
		      until_0) =
	  let val (time_4, i_373) = Array.sub (heap_5, 0)
	      val item_29 as (_, j_71) = Array.sub (heap_5, last_8)
	  in if leq_0 (time_4, until_0)
	     then (Array.update (heap_5, 0, item_29) ;
		   Array.update (index_11, j_71, 0) ;
		   (down_0 (heap_5, index_11, last_8 - 1)) 0 ;
		   pq_1
		   := {heap = heap_5, index = index_11, last = last_8 - 1} ;
		   i_373 :: (deleteto_0 (pq_1, until_0)))
	     else []
	  end
      fun delete_1 (pq_2
		    as ref {heap = heap_6, index = index_12, last = last_9},
		    i_374) =
	  let val j_72 = Array.sub (index_12, i_374)
	      val item_30 as (_, k_83) = Array.sub (heap_6, last_9)
	  in (Array.update (heap_6, j_72, item_30) ;
	      Array.update (index_12, k_83, j_72) ;
	      (down_0 (heap_6, index_12, last_9 - 1)) j_72 ;
	      pq_2 := {heap = heap_6, index = index_12, last = last_9 - 1})
	  end
      datatype status_0 = unknown_0 | disabled_0 | maybe_ready_at_0 of unit
      datatype result_0 =
	       is_executed_0 | is_disabled_0 | is_maybe_ready_at_0 of unit
      datatype mode_0 = test_1 | fast_0 | bind_1
      exception Exit_0 
      val table_63 :
	  (string, ((mode_0 * int) -> (result_0 * string list))) hash_table_0

      =
	  (mkTable_0 hashId_0) (19, InternalError_0 "BETable.find")
      val insert_16 = insert_4 table_63
      val remove_13 = remove_2 table_63
      val find_13 = lookup_0 table_63
      val peek_15 = find_3 table_63
      fun list_18 () = (listItemsi_1 table_63)
      val table_64 :
	  (string,
	   {dep_list : int list list, index : (int * int), name : string}) hash_table_0

      =
	  (mkTable_0 hashId_0) (19, InternalError_0 "DumpTable.find")
      val no_of_tis_1 = ref 0
      val insert_17 = insert_4 table_64
      val remove_14 = remove_2 table_64
      val find_14 = lookup_0 table_64
      val peek_16 = find_3 table_64
      fun list_19 () = (listItemsi_1 table_64)
      val be_table_0 = table_63
      val generate_instances_0 = ref true
      fun add_be_0 (t_54, bf_0) =
	  (if ! generate_instances_0
	   then let val (m_57, n_241) = get_t_index_0 t_54
		    val dep_list_0 =
			((fold_33
			  (fn (a_113, b_101) =>
			      ((get_dep_list_0 (t_54, a_113)) :: b_101)))
			 (1, (n_241 - m_57) + 2))
			[]
		in (operator_256 (no_of_tis_1, (n_241 - m_57) + 1) ;
		    case peek_11 t_54 of
		      (SOME
		       (transition_0
			{name = name_70, output = output_12,...})) =>
		      (insert_17
		       (t_54,
			{dep_list = dep_list_0,
			 index = (m_57, n_241),
			 name = name_70}))
		    | _ => (raise (InternalError_0 "add_be")))
		end
	   else () ;
	   insert_16 (t_54, bf_0))
      fun dump_inst_0 () =
	  let fun dump_0 ((t_55,
			   {dep_list = dep_list_1,
			    index = (m_58, n_242),
			    name = name_71}),
			  tail_78) =
		  (","
		   :: ("("
		       :: ((toString_19 t_55)
			   :: (",{name=\""
			       :: (name_71
				   :: ("\", index=("
				       :: ((Int.toString m_58)
					   :: (","
					       :: ((Int.toString n_242)
						   :: ("), dep_list="
						       :: (((makelist_0
							     (makelist_0
							      Int.toString))
							    dep_list_1)
							   :: ("})" :: tail_78))

     ))))))))))
	  in case ! CPN'dump_stream_0 of
	       NONE => ()
	     | (SOME file_9) =>
	       ((app (fn str_4 => (output_5 (file_9, str_4))))
		("\n val _ = CPN'Sim.load_inst ("
		 :: ((Int.toString (! no_of_tis_1))
		     :: (",["
			 :: (tl
			     (((fold_30 dump_0) (list_19 ())) ["", "]);"])))

     )) ;
		flushOut_4 file_9 ;
		closeOut_4 file_9 ;
		generate_instances_0 := false)
	  end
      fun load_inst_0 (n_243, list_20) =
	  (no_of_tis_1 := n_243 ; (app insert_17) list_20)
      val unknowns_0 = ref (create_12 0)
      val maybe_readies_0 = ref (create_13 0)
      val model_time_1 = model_time_0
      val cur_step_0 = ref 0
      val stop_step_0 = ref 0
      val stop_time_0 =
	  case start_time_0 of
	    NONE => (ref null_2) | (SOME t_56) => (ref t_56)
      val report_ref_0 = ref ([] : string list)
      val time_inc_funs_0 = ref ([] : ((unit * unit) -> unit) list)
      val step_inc_funs_0 = ref ([] : (int -> unit) list)
      val no_enabled_msg_0 = ref "No more enabled transitions!"
      val dummy_ti_0 =
	  {bind_exe = fn (_ : (mode_0 * int)) => (is_disabled_0, [""]),
	   dep_list = [0],
	   id = base_16,
	   inst = 0,
	   name = "",
	   status = ref unknown_0}
      val transitions_3 = ref (Array.array (0, dummy_ti_0))
      fun create_14 () =
	  let val _ = CPN'debug_0 "Sim.create"
	      val _ = DSUI_SetStatusBarMessage_1 "Creating instances..."
	      val _ =
		  if (Array.length (! transitions_3)) <> (! no_of_tis_1)
		  then transitions_3
		       := (Array.array (! no_of_tis_1, dummy_ti_0))
		  else ()
	      fun create_ti_0 (t_57, be_0) =
		  let val {dep_list = dep_list_2,
			   index = (m_59, n_244),
			   name = name_72} =
			  find_14 t_57
		      fun create_i_0 ([], i_375) =
			  (if i_375 > ((n_244 - m_59) + 2)
			   then raise (InternalError_0 "Sim.create_ti")
			   else ())
			| create_i_0 (d_39 :: ds_1, i_376) =
			  (Array.update
			   (! transitions_3,
			    (m_59 + i_376) - 1,
			    {bind_exe = be_0,
			     dep_list = d_39,
			     id = t_57,
			     inst = i_376,
			     name = name_72,
			     status = ref unknown_0}) ;
			   create_i_0 (ds_1, i_376 + 1))
		  in create_i_0 (dep_list_2, 1)
		  end
	  in (app create_ti_0) (list_18 ())
	  end
      val instances_changed_0 = ref true
      val init_state_funs_0 = ref ([] : (unit -> unit) list)
      fun init_state_0 () =
	  let val _ = CPN'debug_0 "init_state"
	      val _ = CPN'report_timing_0 (concat ["init_state @ "])
	      val _ = CPN'stop_timing_0 ()
	      val _ =
		  if ! instances_changed_0
		  then (create_14 () ; instances_changed_0 := false)
		  else ()
	      val n_245 = Array.length (! transitions_3)
	      fun set_unknown_0 0 = ()
		| set_unknown_0 i_377 =
		  ((case Array.sub (! transitions_3, i_377 - 1) of
		      {status = noName_810,...} => noName_810)
		   := unknown_0 ;
		   set_unknown_0 (i_377 - 1))
	  in (set_unknown_0 n_245 ;
	      unknowns_0 := (create_12 n_245) ;
	      stop_step_0 := 0 ;
	      cur_step_0 := 0 ;
	      case start_time_0 of
		NONE => ()
	      | (SOME t_58) =>
		(stop_time_0 := t_58 ;
		 model_time_1 := t_58 ;
		 maybe_readies_0 := (create_13 n_245)) ;
	      if ! reset_ran_gen_0
	      then (DSUI_SetStatusBarMessage_1
		    "Initializing random genrator..." ;
		    init_51 (! seed_1))
	      else () ;
	      if ! reset_ref_vars_0
	      then (DSUI_SetStatusBarMessage_1 "Initializing references..." ;
		    (! init_56) ())
	      else () ;
	      DSUI_SetStatusBarMessage_1 "Initializing markings..." ;
	      set_init_mark_0 () ;
	      (app (fn f_278 => (f_278 ()))) (! init_state_funs_0))
	  end
      fun mark_dependents_0 [] = ()
	| mark_dependents_0 (x_220 :: xs_31) =
	  let val {status = status_2,...} =
		  Array.sub (! transitions_3, x_220)
	  in case ! status_2 of
	       unknown_0 => (mark_dependents_0 xs_31)
	     | disabled_0 =>
	       (status_2 := unknown_0 ;
		insert_14 (unknowns_0, x_220) ;
		mark_dependents_0 xs_31)
	     | (maybe_ready_at_0 time_5) =>
	       (status_2 := unknown_0 ;
		delete_1 (maybe_readies_0, x_220) ;
		insert_14 (unknowns_0, x_220) ;
		mark_dependents_0 xs_31)
	  end
      fun is_stop_request_0 () = ((getUserRequest1_0 ()) > 0)
      fun accept_stop_request_0 () = (clearUserRequest1_0 ())
      val stop_crit_msg_0 = ref ([] : string list)
      val stop_crit_header_0 = "The following stop criterias are fulfilled:"
      val disabled_msg_0 = "The transition instance is disabled!"
      val executed_msg_0 = "The transition instance has occured!"
      fun no_enabled_0 () =
	  (operator_262 (stop_crit_msg_0, ! no_enabled_msg_0))
      fun error_exn_msg_0 (exn_8, name_73) =
	  (operator_262
	   (stop_crit_msg_0,
	    concat
	    ["The exception ",
	     exn_8,
	     " is raised while\nytransition ",
	     name_73,
	     " occured!"]))
      fun critical_error_exn_msg_0 (exn_9, name_74) =
	  (operator_262
	   (stop_crit_msg_0,
	    concat
	    ["The exception ",
	     exn_9,
	     " is raised outside the code region while transition ",
	     name_74,
	     " occured, and the state might be incorect!"]))
      fun internal_error_msg_0 (msg_4, name_75) =
	  (operator_262
	   (stop_crit_msg_0,
	    concat
	    ["An Internal Error occured while transition ",
	     name_75,
	     " occured!\n",
	     "Internal Error: ",
	     msg_4]))
      fun cancel_exn_msg_0 (msg_5, name_76) =
	  (operator_262
	   (stop_crit_msg_0,
	    concat
	    ["The simulation stopped while ",
	     name_76,
	     " occured because of:\n",
	     msg_5]))
      fun stop_exn_msg_0 msg_6 = (operator_262 (stop_crit_msg_0, msg_6))
      fun check_stop_crit_0 () =
	  let fun add_2 msg_7 =
		  (case ! stop_crit_msg_0 of
		     [] => (stop_crit_msg_0 := [msg_7, stop_crit_header_0])
		   | _ => (operator_262 (stop_crit_msg_0, msg_7)))
	      fun check_0 (until_step_0 n_246) =
		  (if n_246 = (! cur_step_0)
		   then add_2
			("\n - Until Step Number is " ^ (Int.toString n_246))
		   else ())
		| check_0 (additional_steps_0 n_247) =
		  (if (n_247 + (! stop_step_0)) = (! cur_step_0)
		   then add_2
			("\n - Additional Steps " ^ (Int.toString n_247))
		   else ())
		| check_0 (until_time_0 t_59) =
		  (if ready_0 t_59
		   then (model_time_1 := t_59 ;
			 add_2 ("\n - Until Time is " ^ (mkstr_1 t_59)))
		   else ())
		| check_0 (additional_time_0 t_60) =
		  (if ready_0 (add_1 (! stop_time_0, t_60))
		   then (model_time_1 := (add_1 (! stop_time_0, t_60)) ;
			 add_2 ("\n - Additional Time " ^ (mkstr_1 t_60)))
		   else ())
	  in (if is_stop_request_0 ()
	      then (accept_stop_request_0 () ;
		    add_2 "\n - Simulation stopped by user")
	      else () ;
	      (app check_0) (! stop_crits_0) ;
	      (! stop_crit_msg_0) = [])
	  end
      val make_trans_report_0 =
	  case start_time_0 of
	    NONE =>
	    (fn (name_77, inst_4 : int, tail_79) =>
		(if ! report_transitions_0
		 then "\n"
		      :: ((Int.toString (! cur_step_0))
			  :: ("\t" :: (name_77 :: tail_79)))
		 else tail_79))
	  | (SOME _) =>
	    (fn (name_78, inst_5 : int, tail_80) =>
		(if ! report_transitions_0
		 then "\n"
		      :: ((Int.toString (! cur_step_0))
			  :: ("\t"
			      :: ((mkstr_1 (! model_time_1))
				  :: ("\t" :: (name_78 :: tail_80)))))
		 else tail_80))
      fun make_func_report_0 (id_36, inst_6) =
	  (case ! report_function_0 of
	     NONE => ()
	   | (SOME func_0) =>
	     (operator_262 (report_ref_0, func_0 (id_36, inst_6))))
      fun make_report_0 (id_37, name_79, inst_7, report_0) =
	  (if ! report_transitions_0
	   then operator_262
		(report_ref_0,
		 concat (make_trans_report_0 (name_79, inst_7, report_0)))
	   else () ;
	   make_func_report_0 (id_37, inst_7))
      fun increase_time_0 () =
	  let fun move_0 j_73 =
		  ((case Array.sub (! transitions_3, j_73) of
		      {status = noName_811,...} => noName_811)
		   := unknown_0 ;
		   insert_14 (unknowns_0, j_73))
	      val time_6 = min_12 (! maybe_readies_0)
	  in (CPN'debug_0 ("Increasing time to " ^ (mkstr_1 time_6)) ;
	      (app (fn f_279 => (f_279 (! model_time_1, time_6))))
	      (! time_inc_funs_0) ;
	      model_time_1 := time_6 ;
	      (app move_0) (deleteto_0 (maybe_readies_0, time_6)) ;
	      if check_stop_crit_0 () then time_6 else raise Exit_0)
	  end
      fun make_response_0 () =
	  (stop_step_0 := (! cur_step_0) ;
	   stop_time_0 := (! model_time_1) ;
	   (Int.toString (! cur_step_0),
	    if isSome start_time_0 then mkstr_1 (! model_time_1) else "",
	    concat (rev (! stop_crit_msg_0)))
	   before (stop_crit_msg_0 := []))
      fun inst_8 () = (! CPN'inst_0)
      fun step_0 () = (! cur_step_0)
      fun save_report_0 (filename_1 : string) =
	  let val file_10 = openOut_2 filename_1
	  in ((app (fn x_221 => (output_5 (file_10, x_221))))
	      (rev (! report_ref_0)) ;
	      closeOut_4 file_10)
	  end
      fun clear_report_0 () = (report_ref_0 := [])
      fun check_enab_0 (t_61, i_378) =
	  let val _ =
		  CPN'debug_0
		  (((("check_enab (" ^ t_61) ^ ",") ^ (Int.toString i_378))
		   ^ ")")
	      val index_13 = get_ti_index_0 (t_61, i_378)
	      val {bind_exe = bind_exe_0,
		   dep_list = dep_list_3,
		   id = id_38,
		   inst = inst_9,
		   name = name_80,
		   status = status_3} =
		  Array.sub (! transitions_3, index_13)
	  in (CPN'inst_0 := inst_9 ;
	      case ! status_3 of
		unknown_0 =>
		(case bind_exe_0 (test_1, inst_9) of
		   (is_executed_0, _) => true
		 | (is_disabled_0, _) =>
		   (status_3 := disabled_0 ;
		    delete_0 (unknowns_0, index_13) ;
		    false)
		 | (is_maybe_ready_at_0 time_7, _) =>
		   (status_3 := (maybe_ready_at_0 time_7) ;
		    delete_0 (unknowns_0, index_13) ;
		    insert_15 (maybe_readies_0, time_7, index_13) ;
		    false))
	      | _ => false)
	  end
      fun run_6 () =
	  (let fun noName_812 () =
		   (if check_stop_crit_0 ()
		    then (let val index_14 =
				  (ran_4 (! unknowns_0))
				  handle EmptyRanSet_0 =>
					 (increase_time_0 () ;
					  ran_4 (! unknowns_0))
			      val {bind_exe = bind_exe_1,
				   dep_list = dep_list_4,
				   id = id_39,
				   inst = inst_10,
				   name = name_81,
				   status = status_4} =
				  Array.sub (! transitions_3, index_14)
			  in (CPN'inst_0 := inst_10 ;
			      (case bind_exe_1 (fast_0, inst_10) of
				 (is_disabled_0, _) =>
				 (status_4 := disabled_0 ;
				  delete_0 (unknowns_0, index_14))
			       | (is_executed_0, report_1) =>
				 (inc_1 cur_step_0 ;
				  (app (fn f_280 => (f_280 (! cur_step_0))))
				  (! step_inc_funs_0) ;
				  mark_dependents_0 dep_list_4 ;
				  make_report_0
				  (id_39, name_81, inst_10, report_1))
			       | (is_maybe_ready_at_0 time_8, _) =>
				 (status_4 := (maybe_ready_at_0 time_8) ;
				  delete_0 (unknowns_0, index_14) ;
				  insert_15
				  (maybe_readies_0, time_8, index_14)))
			      handle (CPN'Error_0 str_5) =>
				     (error_exn_msg_0 (str_5, name_81))
				   | (CPN'Cancel_0 str_6) =>
				     (cancel_exn_msg_0 (str_6, name_81))
				   | (CPN'Stop_0 str_7) =>
				     (stop_exn_msg_0 str_7)
				   | (InternalError_0 str_8) =>
				     (internal_error_msg_0 (str_8, name_81))
				   | exn_10 =>
				     (critical_error_exn_msg_0
				      (exnName_1 exn_10, name_81)))
			  end
			  handle EmptyPQ_0 => (no_enabled_0 ())
			       | Exit_0 => () ;
			  noName_812 ())
		    else ())
	   in noName_812 ()
	   end ;
	   make_response_0 ())
      fun man_bind_1 (t_62, i_379) =
	  let val _ =
		  CPN'debug_0
		  (((("man_bind (" ^ t_62) ^ ",") ^ (Int.toString i_379))
		   ^ ")")
	      val _ =
		  if ! use_manbind_0
		  then ()
		  else raise (InternalError_0
			      "No code generated for manual bindings")
	      val index_15 = get_ti_index_0 (t_62, i_379)
	      val {bind_exe = bind_exe_2,
		   dep_list = dep_list_5,
		   id = id_40,
		   inst = inst_11,
		   name = name_82,
		   status = status_5} =
		  Array.sub (! transitions_3, index_15)
	      fun compute_bidings_0 () =
		  (CPN'inst_0 := inst_11 ;
		   case ! status_5 of
		     unknown_0 =>
		     (case bind_exe_2 (bind_1, inst_11) of
			(is_executed_0, report_2) =>
			(inc_1 cur_step_0 ;
			 (app (fn f_281 => (f_281 (! cur_step_0))))
			 (! step_inc_funs_0) ;
			 mark_dependents_0 dep_list_5 ;
			 make_report_0 (id_40, name_82, inst_11, report_2) ;
			 operator_262 (stop_crit_msg_0, executed_msg_0))
		      | (is_disabled_0, _) =>
			(status_5 := disabled_0 ;
			 delete_0 (unknowns_0, i_379) ;
			 operator_262 (stop_crit_msg_0, disabled_msg_0))
		      | (is_maybe_ready_at_0 time_9, _) =>
			(status_5 := (maybe_ready_at_0 time_9) ;
			 delete_0 (unknowns_0, i_379) ;
			 insert_15 (maybe_readies_0, time_9, i_379) ;
			 handle_maybe_ready_at_0 ()))
		   | disabled_0 =>
		     (operator_262 (stop_crit_msg_0, disabled_msg_0))
		   | (maybe_ready_at_0 time_10) =>
		     (handle_maybe_ready_at_0 ()))
	      and handle_maybe_ready_at_0 () =
		  let val cur_time_0 = ! model_time_1
		  in (let fun noName_813 () =
			      (if (check_all_unknowns_0 ()) = 0
			       then (increase_time_0 () ; noName_813 ())
			       else ())
		      in noName_813 ()
		      end ;
		      if lt_3 (cur_time_0, ! model_time_1)
		      then compute_bidings_0 ()
		      else operator_262 (stop_crit_msg_0, disabled_msg_0))
		  end
	      and check_all_unknowns_0 () =
		  let fun check_1 i_380 =
			  (if i_380 <= 0
			   then 0
			   else let val {bind_exe = bind_exe_3,
					 dep_list = dep_list_6,
					 id = id_41,
					 inst = inst_12,
					 name = name_83,
					 status = status_6} =
					Array.sub (! transitions_3, i_380)
				in (CPN'inst_0 := inst_12 ;
				    case bind_exe_3 (test_1, inst_12) of
				      (is_executed_0, _) =>
				      (1 + (check_1 (i_380 - 1)))
				    | (is_disabled_0, _) =>
				      (status_6 := disabled_0 ;
				       delete_0 (unknowns_0, i_380) ;
				       check_1 (i_380 - 1))
				    | (is_maybe_ready_at_0 time_11, _) =>
				      (status_6 := (maybe_ready_at_0 time_11) ;
				       delete_0 (unknowns_0, i_380) ;
				       insert_15
				       (maybe_readies_0, time_11, i_380) ;
				       check_1 (i_380 - 1)))
				end)
		  in check_1 (size_20 (! unknowns_0))
		  end
	  in ((if check_stop_crit_0 ()
	       then ()
	       else (compute_bidings_0 ())
		    handle (CPN'Error_0 str_9) =>
			   (error_exn_msg_0 (str_9, name_82))
			 | (CPN'Cancel_0 str_10) =>
			   (cancel_exn_msg_0 (str_10, name_82))
			 | (CPN'Stop_0 str_11) => (stop_exn_msg_0 str_11)
			 | (InternalError_0 str_12) =>
			   (internal_error_msg_0 (str_12, name_82))
			 | exn_11 =>
			   (critical_error_exn_msg_0
			    (exnName_1 exn_11, name_82)))
	      handle Exit_0 => () ;
	      make_response_0 ())
	  end
      val print_mark_1 = map print_mark_0
      fun print_page_mark_0 [] = []
	| print_page_mark_0 ((page_19, i_381) :: pages_12) =
	  (((fold_30
	     (fn (p_64, tail_81) =>
		 (((p_64, i_381), print_mark_0 (p_64, i_381)) :: tail_81)))
	    (get_places_0 page_19))
	   (print_page_mark_0 pages_12))
      val print_size_0 = map size_mark_0
      fun print_page_size_0 [] = []
	| print_page_size_0 ((page_20, i_382) :: pages_13) =
	  (((fold_30
	     (fn (p_65, tail_82) =>
		 (((p_65, i_382), size_mark_0 (p_65, i_382)) :: tail_82)))
	    (get_places_0 page_20))
	   (print_page_size_0 pages_13))
      val print_enab_0 = map check_enab_0
      fun print_page_enab_0 [] = []
	| print_page_enab_0 ((page_21, i_383) :: pages_14) =
	  let fun f_282 (t_63, tail_83) =
		  (if is_transition_0 t_63
		   then ((t_63, i_383), check_enab_0 (t_63, i_383))
			:: tail_83
		   else tail_83)
	  in ((fold_30 f_282) (get_transitions_0 page_21))
	     (print_page_enab_0 pages_14)
	  end
      fun change_mark_1 ([], _) = ()
	| change_mark_1 ((pid_17, inst_13) :: places_4, toinit_1) =
	  (CPN'inst_0 := inst_13 ;
	   change_mark_0 (pid_17, inst_13, toinit_1) ;
	   mark_dependents_0 (get_dep_trans_0 (pid_17, inst_13)) ;
	   change_mark_1 (places_4, toinit_1))
      fun change_model_time_0 time_12 =
	  let fun move_1 j_74 =
		  ((case Array.sub (! transitions_3, j_74) of
		      {status = noName_814,...} => noName_814)
		   := unknown_0 ;
		   insert_14 (unknowns_0, j_74))
	  in (model_time_1 := (maketime_0 time_12) ;
	      (app move_1) (deleteto_0 (maybe_readies_0, ! model_time_1)))
	  end
      fun fetch_tms_0 noName_815 =
	  (fn noName_816 =>
	      (fn noName_817 =>
		  (case (noName_815, noName_816, noName_817) of
		     (exn_12, _, ([], _, _)) => (raise exn_12)
		   | (_, _, (_, 0, _)) => []
		   | (exn_13, lt_11, (tms_0, coef_7, col_15)) =>
		     let val (item_31, tms'_0) = (get_ran_0 exn_13) tms_0
			 val col'_1 = col_1 item_31
		     in if (lt_11 (col_15, col'_1))
			   orelse (lt_11 (col'_1, col_15))
			then ((fetch_tms_0 exn_13) lt_11)
			     (tms'_0, coef_7, col_15)
			else item_31
			     :: (((fetch_tms_0 exn_13) lt_11)
				 (tms'_0, coef_7 - 1, col_15))
		     end)))
      fun collect_token_0 noName_818 =
	  (fn noName_819 =>
	      (case (noName_818, noName_819) of
		 (exn_14, (mark_1, collect_0, cmp_9, exp_11, tv_1)) =>
		 let fun cf_1 (operator_264 (col_16, time_13)) =
			 (case cmp_9
			       (operator_264 (col_16, null_2),
				operator_264 (exp_11, null_2)) of
			    EQUAL =>
			    (if ready_0 (sub_29 (time_13, tv_1))
			     then EQUAL
			     else LESS)
			  | rel_0 => rel_0)
		 in case (collect_0 cf_1) (! mark_1) of
		      [] => (raise exn_14) | tms_1 => (random_0 tms_1)
		 end))
      fun collect_tms_0 noName_823 =
	  (fn noName_824 =>
	      (case (noName_823, noName_824) of
		 (exn_15, (mark_2, collect_1, cmp_10, exp_12)) =>
		 let fun fetch_0 ([], _) = (raise exn_15)
		       | fetch_0 (tms_2, tail_84) =
			 ((case (get_ran_0 exn_15) tms_2 of
			     {1 = noName_820,...} => noName_820)
			  :: tail_84)
		     fun cf_2 noName_821 =
			 (fn noName_822 =>
			     (case (noName_821, noName_822) of
				(operator_264 (col1_1, time1_0),
				 operator_264 (col2_1, time2_0)) =>
				(case cmp_10
				      (operator_264 (col2_1, null_2),
				       operator_264 (col1_1, null_2)) of
				   EQUAL =>
				   (if ready_0 (sub_29 (time2_0, time1_0))
				    then EQUAL
				    else LESS)
				 | rel_1 => rel_1)))
		     fun bf_1 [] = []
		       | bf_1 (item_32 :: rtms_0) =
			 (fetch_0
			  ((collect_1 (cf_2 item_32)) (! mark_2),
			   bf_1 rtms_0))
		 in bf_1 exp_12
		 end))
      fun each_place_0 (enough_tokens_0, answer_0) =
	  (if enough_tokens_0 then answer_0 else (false, is_disabled_0))
      fun each_timed_place_0 (enough1_0, NONE, (enough2_0, state_1)) =
	  (enough1_0 andalso enough2_0, state_1)
	| each_timed_place_0 (enough1_1,
			      SOME time_14,
			      (enough2_1, is_disabled_0)) =
	  (enough1_1 andalso enough2_1, is_maybe_ready_at_0 time_14)
	| each_timed_place_0 (enough1_2,
			      SOME time1_1,
			      (enough2_2, is_maybe_ready_at_0 time2_1)) =
	  (enough1_2 andalso enough2_2,
	   is_maybe_ready_at_0
	   (if lt_3 (time1_1, time2_1) then time1_1 else time2_1))
	| each_timed_place_0 _ = (raise (InternalError_0 "each_timed_place"))
      fun show_place_0 noName_825 =
	  (fn noName_826 =>
	      (case (noName_825, noName_826) of
		 (i_384, (p_66, true)) =>
		 (size_mark_0 (p_66, i_384), print_mark_0 (p_66, i_384))
	       | (i_385, (p_67, false)) => (size_mark_0 (p_67, i_385), ""))

     )
      fun pause_before_1 noName_827 =
	  (fn noName_828 =>
	      (case (noName_827, noName_828) of
		 (show_arc_0, (t_64, i_386)) =>
		 (if ! pause_before_step_0
		  then let val (places_5, arcs_13) =
			       pause_before_0 (t_64, i_386)
		       in (if ! show_marking_0
			   then show_markings_0
				((map (show_place_0 i_386)) places_5)
			   else () ;
			   if ! pause_show_tokens_0
			   then show_tokens_0 ((map show_arc_0) arcs_13)
			   else () ;
			   end_pause_0 ())
		       end
		  else ())))
      fun pause_after_1 noName_829 =
	  (fn noName_830 =>
	      (case (noName_829, noName_830) of
		 (show_arc_1, (t_65, i_387)) =>
		 (if ! pause_after_step_0
		  then let val (places_6, arcs_14) =
			       pause_after_0 (t_65, i_387)
		       in (if ! show_marking_0
			   then show_markings_0
				((map (show_place_0 i_387)) places_6)
			   else () ;
			   if ! pause_show_tokens_0
			   then show_tokens_0 ((map show_arc_1) arcs_14)
			   else () ;
			   end_pause_0 ())
		       end
		  else ())))
      fun tst_ill_marks_0 (ref []) = ()
	| tst_ill_marks_0 (ref arcs_15) =
	  (raise (CPN'Cancel_0
		  (concat
		   ("The following arc(s) resulted in illegal marking(s):\n"
		    :: ((map toString_19) arcs_15)))))
      fun code_action_0 noName_831 =
	  (fn noName_832 =>
	      (fn noName_833 =>
		  (case (noName_831, noName_832, noName_833) of
		     (action_1, inst_14, input_12) =>
		     (((action_1 inst_14) input_12)
		      handle (CPN'Cancel_0 s_157) =>
			     (raise (CPN'Cancel_0 s_157))
			   | (CPN'Stop_0 s_158) => (raise (CPN'Stop_0 s_158))
			   | (IOError_0 s_159) =>
			     (raise (CPN'Cancel_0 s_159))
			   | exn_16 =>
			     (raise (CPN'Error_0 (exnName_1 exn_16)))))))
      val step_1 = step_0
      val inst_15 = inst_8
      val res_4 =
	  (use_dmo_0 := false ;
	   generate_instances_0 := false ;
	   instances_changed_0 := true ;
	   stop_crits_0 := [])
      val time_store_0 = ref 0
      val log_stream_0 = ref stdOut_2
      fun init_time_0 () =
	  (time_store_0 := (tod_0 ()) ;
	   log_stream_0 := (openOut_2 "lllllll.txt"))
      fun logit_0 st_2 =
	  (output_5 (! log_stream_0, st_2) ; flushOut_4 (! log_stream_0))
      fun rel_time_0 () =
	  let val nt_0 = tod_0 ()
	  in (nt_0 - (! time_store_0)) before (time_store_0 := nt_0)
	  end
      val maxcnt_0 = 100000
      val base_25 = (base_21, base_21, base_21, base_21)
      fun legal_4 (CPN'1_0, CPN'2_0, CPN'3_0, CPN'4_0) =
	  ((((legal_0 CPN'1_0) andalso (legal_0 CPN'2_0))
	    andalso (legal_0 CPN'3_0))
	   andalso (legal_0 CPN'4_0))
      fun lt_12 ((CPN'1_1, CPN'2_1, CPN'3_1, CPN'4_1),
		 (CPN'1'_0, CPN'2'_0, CPN'3'_0, CPN'4'_0)) =
	  ((lt_6 (CPN'1_1, CPN'1'_0))
	   orelse (if lt_6 (CPN'1'_0, CPN'1_1)
		   then false
		   else (lt_6 (CPN'2_1, CPN'2'_0))
			orelse (if lt_6 (CPN'2'_0, CPN'2_1)
				then false
				else (lt_6 (CPN'3_1, CPN'3'_0))
				     orelse (if lt_6 (CPN'3'_0, CPN'3_1)
					     then false
					     else (lt_6 (CPN'4_1, CPN'4'_0))
						  orelse ((lt_6
							   (CPN'4'_0, CPN'4_1))
							  andalso false)))))
      val cmp_11 = a_cmp_0 lt_12
      fun mkstr_7 (CPN'1_2, CPN'2_2, CPN'3_2, CPN'4_2) =
	  (CPN'concat_1
	   ["(",
	    mkstr_3 CPN'1_2,
	    ",",
	    mkstr_3 CPN'2_2,
	    ",",
	    mkstr_3 CPN'3_2,
	    ",",
	    mkstr_3 CPN'4_2,
	    ")"])
      val mkstr_ms_5 : ((int * int * int * int) list -> string) =
	  mkstr_ms_0 (mkstr_7, lt_12)
      fun size_21 () =
	  ((((size_13 ()) * (size_13 ())) * (size_13 ())) * (size_13 ()))
      fun input_13 _ = (error_not_decl_0 ("input", "T"))
      fun output_13 (CPN's_0, CPN'c_0) =
	  (output_5 (CPN's_0, mkstr_7 CPN'c_0))
      val input_ms_5 : (TextIO.instream -> (int * int * int * int) list) =
	  input_ms_0 input_13
      val output_ms_5 :
	  ((TextIO.outstream * (int * int * int * int) list) -> unit) =
	  output_ms_0 (output_13, lt_12)
      fun all_7 () = (error_not_decl_0 ("all", "T"))
      fun ord_10 _ = (error_not_decl_0 ("ord", "T"))
      fun col_17 _ = (error_not_decl_0 ("col", "T"))
      fun ran_5 _ = (error_not_decl_0 ("ran", "T"))
      datatype E_0 = e_28
      val base_26 = e_28
      fun all_8 () = [e_28]
      fun legal_5 (_ : E_0) = true
      val mkstr_8 = fn e_28 => "e"
      fun input_14 _ = (error_not_decl_0 ("input", "E"))
      val col_18 =
	  fn 0 => e_28 | _ => (raise (CPN'Error_0 out_of_range_0))
      val ord_11 = fn e_28 => 0
      val lt_13 = fn (_, _) => false
      val cmp_12 : ((E_0 * E_0) -> order) = a_cmp_0 lt_13
      fun output_14 (CPN's_1, CPN'c_1) =
	  (output_5 (CPN's_1, mkstr_8 CPN'c_1))
      val input_ms_6 : (TextIO.instream -> E_0 list) = input_ms_0 input_14
      val output_ms_6 : ((TextIO.outstream * E_0 list) -> unit) =
	  output_ms_0 (output_14, lt_13)
      fun size_22 () = 1
      val mkstr_ms_6 : (E_0 list -> string) = mkstr_ms_0 (mkstr_8, lt_13)
      fun ran_6 () = (col_18 (int_1 (size_22 ())))
      val time_zero_0 = (0, 0, 0, 0)
      fun addsecs_0 noName_834 =
	  (fn noName_835 =>
	      (fn noName_836 =>
		  (case (noName_834, noName_835, noName_836) of
		     ((d_40, h_21, m_60, s_160), ds_2, cnt_1) =>
		     ((ds_2,
		       cnt_1 + 1,
		       let val ns_0 = s_160 + ds_2
		       in if ns_0 < 60
			  then (d_40, h_21, m_60, ns_0)
			  else let val nm_0 = m_60 + (ns_0 div 60)
				   val ns_1 = ns_0 mod 60
			       in if nm_0 < 60
				  then (d_40, h_21, nm_0, ns_1)
				  else let val nh_0 = h_21 + (nm_0 div 60)
					   val nm_1 = nm_0 mod 60
				       in if nh_0 < 24
					  then (d_40, nh_0, nm_1, ns_1)
					  else let val nd_0 =
						       d_40 + (nh_0 div 24)
						   val nh_1 = nh_0 mod 24
					       in (nd_0, nh_1, nm_1, ns_1)
					       end
				       end
			       end
		       end)
		      handle operator_278 => (0, 0, time_zero_0)))))
      fun tosec_0 (d_41, h_22, m_61, s_161) =
	  (((((((real d_41) * 24.0) + (real h_22)) * 60.0) + (real m_61))
	    * 60.0)
	   + (real s_161))
      val cmp_13 =
	  fn ((CPN'1_3, CPN'2_3, CPN'3_3, CPN'4_3),
	      (CPN'1'_1, CPN'2'_1, CPN'3'_1, CPN'4'_1)) =>
	     (if lt_6 (CPN'4_3, CPN'4'_1)
	      then LESS
	      else if lt_6 (CPN'4'_1, CPN'4_3)
		   then GREATER
		   else if lt_6 (CPN'3_3, CPN'3'_1)
			then LESS
			else if lt_6 (CPN'3'_1, CPN'3_3)
			     then GREATER
			     else if lt_6 (CPN'2_3, CPN'2'_1)
				  then LESS
				  else if lt_6 (CPN'2'_1, CPN'2_3)
				       then GREATER
				       else if lt_6 (CPN'1_3, CPN'1'_1)
					    then LESS
					    else if lt_6 (CPN'1'_1, CPN'1_3)
						 then GREATER
						 else EQUAL)
      val cmp_14 = cmp_13
      val tsize_0 = size_9
      val tfold_0 = fold_35
      fun tinsert_0 (TreeNil_0, col_19) = (singleton_0 col_19)
	| tinsert_0 (TreeNode_0
		     {left = left_13, right = right_13, value = value_14,...}

     ,
		     col_20) =
	  (case cmp_14 (value_14, col_20) of
	     LESS =>
	     (balance_0 (value_14, left_13, tinsert_0 (right_13, col_20)))
	   | GREATER =>
	     (balance_0 (value_14, tinsert_0 (left_13, col_20), right_13))
	   | EQUAL =>
	     (if (tsize_0 left_13) < (tsize_0 right_13)
	      then balance_0
		   (value_14, tinsert_0 (left_13, col_20), right_13)
	      else balance_0
		   (value_14, left_13, tinsert_0 (right_13, col_20))))
      fun taddto_0 (tree_0, []) = tree_0
	| taddto_0 (tree_1, col_21 :: rms_2) =
	  (taddto_0 (tinsert_0 (tree_1, col_21), rms_2))
      fun tdelete_0 (TreeNil_0, col_22) = (raise Subtract_0)
	| tdelete_0 (tree_2
		     as TreeNode_0
			{left = left_14, right = right_14, value = value_15,...

     },
		     col_23) =
	  (case cmp_14 (value_15, col_23) of
	     LESS =>
	     (balance_0 (value_15, left_14, tdelete_0 (right_14, col_23)))
	   | GREATER =>
	     (balance_0 (value_15, tdelete_0 (left_14, col_23), right_14))
	   | EQUAL => (join_5 (left_14, right_14)))
      fun tsubfrom_0 (tree_3, []) = tree_3
	| tsubfrom_0 (tree_4, col_24 :: rms_3) =
	  (tsubfrom_0 (tdelete_0 (tree_4, col_24), rms_3))
      fun empty_3 () = {list = [], tree = TreeNil_0}
      fun is_empty_0 {list = [], tree = TreeNil_0} = true
	| is_empty_0 _ = false
      fun fold_38 noName_837 =
	  (fn noName_838 =>
	      (fn noName_839 =>
		  (case (noName_837, noName_838, noName_839) of
		     (f_283, {list = list_21, tree = tree_5}, base_27) =>
		     (((tfold_0 f_283) (taddto_0 (tree_5, list_21)))
		      base_27))))
      fun size_23 {list = list_22, tree = tree_6} =
	  ((tsize_0 tree_6) + (length list_22))
      fun insert_18 (ims_3
		     as ref
			({list = list_23, tree = tree_7} :
			 {list : (int * int * int * int) list,
			  tree : (int * int * int * int) tree_0}),
		     col_25) =
	  (ims_3 := {list = list_23, tree = tinsert_0 (tree_7, col_25)})
      fun addto_0 (ims_4, ems_0) =
	  ((List.app (fn x_222 => (insert_18 (ims_4, x_222)))) ems_0)
      fun init_58 (ims_5, ems_1) =
	  (ims_5 := (empty_3 ()) ; addto_0 (ims_5, ems_1))
      fun delete_2 (ims_6
		    as ref
		       ({list = [], tree = tree_8} :
			{list : (int * int * int * int) list,
			 tree : (int * int * int * int) tree_0}),
		    col_26) =
	  (ims_6 := {list = [], tree = tdelete_0 (tree_8, col_26)})
	| delete_2 (ims_7
		    as ref
		       ({list = list_24 as value_16 :: res_5, tree = tree_9}

      :
			{list : (int * int * int * int) list,
			 tree : (int * int * int * int) tree_0}),
		    col_27) =
	  (case cmp_14 (col_27, value_16) of
	     EQUAL => (ims_7 := {list = res_5, tree = tree_9})
	   | _ =>
	     (ims_7
	      := {list = [],
		  tree = tdelete_0 (taddto_0 (tree_9, list_24), col_27)}))
      fun subfrom_0 (ims_8, ems_2) =
	  ((List.app (fn x_223 => (delete_2 (ims_8, x_223)))) ems_2)
      fun tmember_0 (TreeNil_0, _) = false
	| tmember_0 (TreeNode_0
		     {left = left_15, right = right_15, value = value_17,...}

     ,
		     col_28) =
	  (case cmp_14 (value_17, col_28) of
	     LESS => (tmember_0 (right_15, col_28))
	   | GREATER => (tmember_0 (left_15, col_28)) | EQUAL => true)
      fun lmember_0 ([], _) = false
	| lmember_0 (value_18 :: list_25, col_29) =
	  (case cmp_14 (value_18, col_29) of
	     EQUAL => true | _ => (lmember_0 (list_25, col_29)))
      fun member_0 ({list = list_26, tree = tree_10}, col_30) =
	  ((tmember_0 (tree_10, col_30))
	   orelse (lmember_0 (list_26, col_30)))
      fun tcf_0 (TreeNil_0, _) = 0
	| tcf_0 (TreeNode_0
		 {left = left_16, right = right_16, value = value_19,...},
		 col_31) =
	  (case cmp_14 (value_19, col_31) of
	     LESS => (tcf_0 (right_16, col_31))
	   | GREATER => (tcf_0 (left_16, col_31))
	   | EQUAL =>
	     ((1 + (tcf_0 (left_16, col_31))) + (tcf_0 (right_16, col_31)))

     )
      fun lcf_0 ([], _) = 0
	| lcf_0 (value_20 :: list_27, col_32) =
	  (case cmp_14 (value_20, col_32) of
	     EQUAL => (1 + (lcf_0 (list_27, col_32)))
	   | _ => (lcf_0 (list_27, col_32)))
      fun cf_3 ({list = list_28, tree = tree_11}, col_33) =
	  ((tcf_0 (tree_11, col_33)) + (lcf_0 (list_28, col_33)))
      fun subset_3 (ims_9 :
		    {list : (int * int * int * int) list,
		     tree : (int * int * int * int) tree_0},
		    ems_3 : (int * int * int * int) list) =
	  (((size_11 ems_3) <= (size_23 ims_9))
	   andalso let fun subset'_0 (coef_8, col_34, x_224 :: xs_32) =
			   (if lt_12 (x_224, col_34)
			    then (coef_8 <= (cf_3 (ims_9, col_34)))
				 andalso (subset'_0 (1, x_224, xs_32))
			    else subset'_0 (coef_8 + 1, col_34, xs_32))
			 | subset'_0 (coef_9, col_35, []) =
			   (coef_9 <= (cf_3 (ims_9, col_35)))
		   in case (sort_1 lt_12) ems_3 of
			(x_225 :: xs_33) => (subset'_0 (1, x_225, xs_33))
		      | [] => true
		   end)
      fun filter_9 noName_840 =
	  (fn noName_841 =>
	      (case (noName_840, noName_841) of
		 (func_1,
		  {list = list_29, tree = tree_12} :
		  {list : (int * int * int * int) list,
		   tree : (int * int * int * int) tree_0}) =>
		 let fun filter'_0 (TreeNil_0, l_118) = l_118
		       | filter'_0 (TreeNode_0
				    {left = left_17,
				     right = right_17,
				     value = value_21,...},
				    l_119) =
			 (if func_1 value_21
			  then filter'_0
			       (left_17,
				value_21 :: (filter'_0 (right_17, l_119)))
			  else filter'_0
			       (left_17, filter'_0 (right_17, l_119)))
		 in filter'_0 (taddto_0 (tree_12, list_29), [])
		 end))
      fun collect_2 noName_842 =
	  (fn noName_843 =>
	      (case (noName_842, noName_843) of
		 (cmp'_1,
		  {list = list_30, tree = tree_13} :
		  {list : (int * int * int * int) list,
		   tree : (int * int * int * int) tree_0}) =>
		 let fun collect'_0 (TreeNil_0, l_120) = l_120
		       | collect'_0 (TreeNode_0
				     {left = left_18,
				      right = right_18,
				      value = value_22,...},
				     l_121) =
			 (case cmp'_1 value_22 of
			    LESS => (collect'_0 (right_18, l_121))
			  | GREATER => (collect'_0 (left_18, l_121))
			  | EQUAL =>
			    (collect'_0
			     (left_18,
			      value_22 :: (collect'_0 (right_18, l_121)))))
		 in collect'_0 (taddto_0 (tree_13, list_30), [])
		 end))
      fun extract_10 ({list = list_31, tree = tree_14} :
		      {list : (int * int * int * int) list,
		       tree : (int * int * int * int) tree_0}) =
	  let val tree'_0 = taddto_0 (tree_14, list_31)
	  in (tsize_0 tree'_0, ((tfold_0 op ::) tree'_0) [])
	  end
      fun init_res_1 (ims_10 as ref {list = list_32, tree = tree_15}) =
	  let val tree'_1 = taddto_0 (tree_15, list_32)
	  in (ims_10 := {list = [], tree = tree'_1} ; tsize_0 tree'_1)
	  end
      fun random_1 noName_844 =
	  (fn noName_845 =>
	      (case (noName_844, noName_845) of
		 (exn_17,
		  {tree = TreeNil_0,...} :
		  {list : (int * int * int * int) list,
		   tree : (int * int * int * int) tree_0}) =>
		 (raise exn_17)
	       | (exn_18, {tree = tree_16,...}) =>
		 let fun get_13 (TreeNode_0
				 {left = left_19,
				  right = right_19,
				  value = value_23,...},
				 i_388) =
			 (case Int.compare (i_388, tsize_0 left_19) of
			    LESS => (get_13 (left_19, i_388))
			  | EQUAL => value_23
			  | GREATER =>
			    (get_13
			     (right_19, (i_388 - (tsize_0 left_19)) - 1)))
		       | get_13 _ = (raise exn_18)
		 in get_13 (tree_16, int_1 (tsize_0 tree_16))
		 end))
      fun random_res_1 noName_846 =
	  (fn noName_847 =>
	      (case (noName_846, noName_847) of
		 (exn_19, ref {tree = TreeNil_0,...}) => (raise exn_19)
	       | (_,
		  ims_11
		  as ref
		     ({list = list_33, tree = tree_17} :
		      {list : (int * int * int * int) list,
		       tree : (int * int * int * int) tree_0})) =>
		 let fun get_14 (TreeNode_0
				 {left = left_20,
				  right = right_20,
				  value = value_24,...},
				 i_389) =
			 (case Int.compare (i_389, tsize_0 left_20) of
			    LESS =>
			    let val (left'_1, col'_2) =
				    get_14 (left_20, i_389)
			    in (balance_0 (value_24, left'_1, right_20),
				col'_2)
			    end
			  | GREATER =>
			    let val (right'_1, col'_3) =
				    get_14
				    (right_20,
				     (i_389 - (tsize_0 left_20)) - 1)
			    in (balance_0 (value_24, left_20, right'_1),
				col'_3)
			    end
			  | EQUAL => (join_5 (left_20, right_20), value_24))
		       | get_14 _ = (raise (InternalError_0 "random_res"))
		     val draw_1 = int_1 (tsize_0 tree_17)
		     val (tree'_2, col_36) = get_14 (tree_17, draw_1)
		 in (ims_11 := {list = col_36 :: list_33, tree = tree'_2} ;
		     col_36)
		 end))
      fun ms_random_res_0 noName_848 =
	  (fn noName_849 =>
	      (fn noName_850 =>
		  (case (noName_848, noName_849, noName_850) of
		     (_,
		      exn_20,
		      ref
		      ({list = [], tree = TreeNil_0} :
		       {list : (int * int * int * int) list,
			tree : (int * int * int * int) tree_0})) =>
		     (raise exn_20)
		   | ((reslist_0, cur_0, no_3),
		      exn_21,
		      ims_12 as ref {list = list_34, tree = tree_18}) =>
		     let val reserved_0 = length list_34
			 val notreserved_0 = tsize_0 tree_18
			 val index_16 =
			     (((! cur_0) + no_3) - 1)
			     mod (reserved_0 + notreserved_0)
		     in if (! cur_0) = (reserved_0 + notreserved_0)
			then raise exn_21
			else if index_16 >= reserved_0
			     then ((random_res_1 exn_21) ims_12)
				  before (inc_1 cur_0)
			     else (List.nth (list_34, index_16))
				  before (inc_1 cur_0)
		     end)))
      fun check_res_0 noName_851 =
	  (fn noName_852 =>
	      (fn noName_853 =>
		  (case (noName_851, noName_852, noName_853) of
		     (exn_22, f_284, ims_13) =>
		     let val col_37 = (random_res_1 exn_22) ims_13
		     in if f_284 col_37
			then col_37
			else ((check_res_0 exn_22) f_284) ims_13
		     end)))
      val no_of_inst_0 = 1
      val marking_0 =
	  Array.tabulate (no_of_inst_0, fn _ => (ref (empty_3 ())))
      val init_mark_0 = ref ([] : (int * int * int * int) list)
      fun mark_3 i_390 = (Array.sub (marking_0, i_390 - 1))
      fun set_init_mark_1 _ =
	  (CPN'inst_0 := 1 ;
	   let fun noName_854 () =
		   (if (! CPN'inst_0) <= no_of_inst_0
		    then ((init_58 (mark_3 (! CPN'inst_0), ! init_mark_0) ;
			   inc_1 CPN'inst_0) ;
			  noName_854 ())
		    else ())
	   in noName_854 ()
	   end)
      fun init_59 i_391 = (init_res_1 (mark_3 i_391))
      fun size_24 i_392 = (size_23 (! (mark_3 i_392)))
      fun print_4 i_393 =
	  let val (size_25, ems_4) = extract_10 (! (mark_3 i_393))
	  in mkstr_ms_5 ems_4
	  end
      fun addto_1 noName_855 =
	  (fn noName_856 =>
	      (case (noName_855, noName_856) of
		 (i_394, ems_5) => (addto_0 (mark_3 i_394, ems_5))))
      fun subfrom_1 noName_857 =
	  (fn noName_858 =>
	      (case (noName_857, noName_858) of
		 (i_395, ems_6) => (subfrom_0 (mark_3 i_395, ems_6))))
      fun get_15 i_396 = (((fold_38 op ::) (! (mark_3 i_396))) [])
      fun set_5 noName_859 =
	  (fn noName_860 =>
	      (case (noName_859, noName_860) of
		 (i_397, ems_7) => (init_58 (mark_3 i_397, ems_7))))
      val _ =
	  (init_mark_0 := [] ;
	   operator_262 (init_mark_funs_0, set_init_mark_1))
      val _ =
	  (insert_4 instances_0) ("19", {print = print_4, size = size_24})
      val cmp_15 = a_cmp_0 lt_9
      val cmp_16 = cmp_15
      val tsize_1 = size_9
      val tfold_1 = fold_35
      fun tinsert_1 (TreeNil_0, col_38) = (singleton_0 col_38)
	| tinsert_1 (TreeNode_0
		     {left = left_21, right = right_21, value = value_25,...}

     ,
		     col_39) =
	  (case cmp_16 (value_25, col_39) of
	     LESS =>
	     (balance_0 (value_25, left_21, tinsert_1 (right_21, col_39)))
	   | GREATER =>
	     (balance_0 (value_25, tinsert_1 (left_21, col_39), right_21))
	   | EQUAL =>
	     (if (tsize_1 left_21) < (tsize_1 right_21)
	      then balance_0
		   (value_25, tinsert_1 (left_21, col_39), right_21)
	      else balance_0
		   (value_25, left_21, tinsert_1 (right_21, col_39))))
      fun taddto_1 (tree_19, []) = tree_19
	| taddto_1 (tree_20, col_40 :: rms_4) =
	  (taddto_1 (tinsert_1 (tree_20, col_40), rms_4))
      fun tdelete_1 (TreeNil_0, col_41) = (raise Subtract_0)
	| tdelete_1 (tree_21
		     as TreeNode_0
			{left = left_22, right = right_22, value = value_26,...

     },
		     col_42) =
	  (case cmp_16 (value_26, col_42) of
	     LESS =>
	     (balance_0 (value_26, left_22, tdelete_1 (right_22, col_42)))
	   | GREATER =>
	     (balance_0 (value_26, tdelete_1 (left_22, col_42), right_22))
	   | EQUAL => (join_5 (left_22, right_22)))
      fun tsubfrom_1 (tree_22, []) = tree_22
	| tsubfrom_1 (tree_23, col_43 :: rms_5) =
	  (tsubfrom_1 (tdelete_1 (tree_23, col_43), rms_5))
      fun empty_4 () = {list = [], tree = TreeNil_0}
      fun is_empty_1 {list = [], tree = TreeNil_0} = true
	| is_empty_1 _ = false
      fun fold_39 noName_861 =
	  (fn noName_862 =>
	      (fn noName_863 =>
		  (case (noName_861, noName_862, noName_863) of
		     (f_285, {list = list_35, tree = tree_24}, base_28) =>
		     (((tfold_1 f_285) (taddto_1 (tree_24, list_35)))
		      base_28))))
      fun size_26 {list = list_36, tree = tree_25} =
	  ((tsize_1 tree_25) + (length list_36))
      fun insert_19 (ims_14
		     as ref
			({list = list_37, tree = tree_26} :
			 {list : string list, tree : string tree_0}),
		     col_44) =
	  (ims_14 := {list = list_37, tree = tinsert_1 (tree_26, col_44)})
      fun addto_2 (ims_15, ems_8) =
	  ((List.app (fn x_226 => (insert_19 (ims_15, x_226)))) ems_8)
      fun init_60 (ims_16, ems_9) =
	  (ims_16 := (empty_4 ()) ; addto_2 (ims_16, ems_9))
      fun delete_3 (ims_17
		    as ref
		       ({list = [], tree = tree_27} :
			{list : string list, tree : string tree_0}),
		    col_45) =
	  (ims_17 := {list = [], tree = tdelete_1 (tree_27, col_45)})
	| delete_3 (ims_18
		    as ref
		       ({list = list_38 as value_27 :: res_6,
			 tree = tree_28} :
			{list : string list, tree : string tree_0}),
		    col_46) =
	  (case cmp_16 (col_46, value_27) of
	     EQUAL => (ims_18 := {list = res_6, tree = tree_28})
	   | _ =>
	     (ims_18
	      := {list = [],
		  tree = tdelete_1 (taddto_1 (tree_28, list_38), col_46)}))
      fun subfrom_2 (ims_19, ems_10) =
	  ((List.app (fn x_227 => (delete_3 (ims_19, x_227)))) ems_10)
      fun tmember_1 (TreeNil_0, _) = false
	| tmember_1 (TreeNode_0
		     {left = left_23, right = right_23, value = value_28,...}

     ,
		     col_47) =
	  (case cmp_16 (value_28, col_47) of
	     LESS => (tmember_1 (right_23, col_47))
	   | GREATER => (tmember_1 (left_23, col_47)) | EQUAL => true)
      fun lmember_1 ([], _) = false
	| lmember_1 (value_29 :: list_39, col_48) =
	  (case cmp_16 (value_29, col_48) of
	     EQUAL => true | _ => (lmember_1 (list_39, col_48)))
      fun member_1 ({list = list_40, tree = tree_29}, col_49) =
	  ((tmember_1 (tree_29, col_49))
	   orelse (lmember_1 (list_40, col_49)))
      fun tcf_1 (TreeNil_0, _) = 0
	| tcf_1 (TreeNode_0
		 {left = left_24, right = right_24, value = value_30,...},
		 col_50) =
	  (case cmp_16 (value_30, col_50) of
	     LESS => (tcf_1 (right_24, col_50))
	   | GREATER => (tcf_1 (left_24, col_50))
	   | EQUAL =>
	     ((1 + (tcf_1 (left_24, col_50))) + (tcf_1 (right_24, col_50)))

     )
      fun lcf_1 ([], _) = 0
	| lcf_1 (value_31 :: list_41, col_51) =
	  (case cmp_16 (value_31, col_51) of
	     EQUAL => (1 + (lcf_1 (list_41, col_51)))
	   | _ => (lcf_1 (list_41, col_51)))
      fun cf_4 ({list = list_42, tree = tree_30}, col_52) =
	  ((tcf_1 (tree_30, col_52)) + (lcf_1 (list_42, col_52)))
      fun subset_4 (ims_20 : {list : string list, tree : string tree_0},
		    ems_11 : string list) =
	  (((size_11 ems_11) <= (size_26 ims_20))
	   andalso let fun subset'_1 (coef_10, col_53, x_228 :: xs_34) =
			   (if lt_9 (x_228, col_53)
			    then (coef_10 <= (cf_4 (ims_20, col_53)))
				 andalso (subset'_1 (1, x_228, xs_34))
			    else subset'_1 (coef_10 + 1, col_53, xs_34))
			 | subset'_1 (coef_11, col_54, []) =
			   (coef_11 <= (cf_4 (ims_20, col_54)))
		   in case (sort_1 lt_9) ems_11 of
			(x_229 :: xs_35) => (subset'_1 (1, x_229, xs_35))
		      | [] => true
		   end)
      fun filter_10 noName_864 =
	  (fn noName_865 =>
	      (case (noName_864, noName_865) of
		 (func_2,
		  {list = list_43, tree = tree_31} :
		  {list : string list, tree : string tree_0}) =>
		 let fun filter'_1 (TreeNil_0, l_122) = l_122
		       | filter'_1 (TreeNode_0
				    {left = left_25,
				     right = right_25,
				     value = value_32,...},
				    l_123) =
			 (if func_2 value_32
			  then filter'_1
			       (left_25,
				value_32 :: (filter'_1 (right_25, l_123)))
			  else filter'_1
			       (left_25, filter'_1 (right_25, l_123)))
		 in filter'_1 (taddto_1 (tree_31, list_43), [])
		 end))
      fun collect_3 noName_866 =
	  (fn noName_867 =>
	      (case (noName_866, noName_867) of
		 (cmp'_2,
		  {list = list_44, tree = tree_32} :
		  {list : string list, tree : string tree_0}) =>
		 let fun collect'_1 (TreeNil_0, l_124) = l_124
		       | collect'_1 (TreeNode_0
				     {left = left_26,
				      right = right_26,
				      value = value_33,...},
				     l_125) =
			 (case cmp'_2 value_33 of
			    LESS => (collect'_1 (right_26, l_125))
			  | GREATER => (collect'_1 (left_26, l_125))
			  | EQUAL =>
			    (collect'_1
			     (left_26,
			      value_33 :: (collect'_1 (right_26, l_125)))))
		 in collect'_1 (taddto_1 (tree_32, list_44), [])
		 end))
      fun extract_11 ({list = list_45, tree = tree_33} :
		      {list : string list, tree : string tree_0}) =
	  let val tree'_3 = taddto_1 (tree_33, list_45)
	  in (tsize_1 tree'_3, ((tfold_1 op ::) tree'_3) [])
	  end
      fun init_res_2 (ims_21 as ref {list = list_46, tree = tree_34}) =
	  let val tree'_4 = taddto_1 (tree_34, list_46)
	  in (ims_21 := {list = [], tree = tree'_4} ; tsize_1 tree'_4)
	  end
      fun random_2 noName_868 =
	  (fn noName_869 =>
	      (case (noName_868, noName_869) of
		 (exn_23,
		  {tree = TreeNil_0,...} :
		  {list : string list, tree : string tree_0}) =>
		 (raise exn_23)
	       | (exn_24, {tree = tree_35,...}) =>
		 let fun get_16 (TreeNode_0
				 {left = left_27,
				  right = right_27,
				  value = value_34,...},
				 i_398) =
			 (case Int.compare (i_398, tsize_1 left_27) of
			    LESS => (get_16 (left_27, i_398))
			  | EQUAL => value_34
			  | GREATER =>
			    (get_16
			     (right_27, (i_398 - (tsize_1 left_27)) - 1)))
		       | get_16 _ = (raise exn_24)
		 in get_16 (tree_35, int_1 (tsize_1 tree_35))
		 end))
      fun random_res_2 noName_870 =
	  (fn noName_871 =>
	      (case (noName_870, noName_871) of
		 (exn_25, ref {tree = TreeNil_0,...}) => (raise exn_25)
	       | (_,
		  ims_22
		  as ref
		     ({list = list_47, tree = tree_36} :
		      {list : string list, tree : string tree_0})) =>
		 let fun get_17 (TreeNode_0
				 {left = left_28,
				  right = right_28,
				  value = value_35,...},
				 i_399) =
			 (case Int.compare (i_399, tsize_1 left_28) of
			    LESS =>
			    let val (left'_2, col'_4) =
				    get_17 (left_28, i_399)
			    in (balance_0 (value_35, left'_2, right_28),
				col'_4)
			    end
			  | GREATER =>
			    let val (right'_2, col'_5) =
				    get_17
				    (right_28,
				     (i_399 - (tsize_1 left_28)) - 1)
			    in (balance_0 (value_35, left_28, right'_2),
				col'_5)
			    end
			  | EQUAL => (join_5 (left_28, right_28), value_35))
		       | get_17 _ = (raise (InternalError_0 "random_res"))
		     val draw_2 = int_1 (tsize_1 tree_36)
		     val (tree'_5, col_55) = get_17 (tree_36, draw_2)
		 in (ims_22 := {list = col_55 :: list_47, tree = tree'_5} ;
		     col_55)
		 end))
      fun ms_random_res_1 noName_872 =
	  (fn noName_873 =>
	      (fn noName_874 =>
		  (case (noName_872, noName_873, noName_874) of
		     (_,
		      exn_26,
		      ref
		      ({list = [], tree = TreeNil_0} :
		       {list : string list, tree : string tree_0})) =>
		     (raise exn_26)
		   | ((reslist_1, cur_1, no_4),
		      exn_27,
		      ims_23 as ref {list = list_48, tree = tree_37}) =>
		     let val reserved_1 = length list_48
			 val notreserved_1 = tsize_1 tree_37
			 val index_17 =
			     (((! cur_1) + no_4) - 1)
			     mod (reserved_1 + notreserved_1)
		     in if (! cur_1) = (reserved_1 + notreserved_1)
			then raise exn_27
			else if index_17 >= reserved_1
			     then ((random_res_2 exn_27) ims_23)
				  before (inc_1 cur_1)
			     else (List.nth (list_48, index_17))
				  before (inc_1 cur_1)
		     end)))
      fun check_res_1 noName_875 =
	  (fn noName_876 =>
	      (fn noName_877 =>
		  (case (noName_875, noName_876, noName_877) of
		     (exn_28, f_286, ims_24) =>
		     let val col_56 = (random_res_2 exn_28) ims_24
		     in if f_286 col_56
			then col_56
			else ((check_res_1 exn_28) f_286) ims_24
		     end)))
      val no_of_inst_1 = 1
      val marking_1 =
	  Array.tabulate (no_of_inst_1, fn _ => (ref (empty_4 ())))
      val init_mark_1 = ref ([] : string list)
      fun mark_4 i_400 = (Array.sub (marking_1, i_400 - 1))
      fun set_init_mark_2 _ =
	  (CPN'inst_0 := 1 ;
	   let fun noName_878 () =
		   (if (! CPN'inst_0) <= no_of_inst_1
		    then ((init_60 (mark_4 (! CPN'inst_0), ! init_mark_1) ;
			   inc_1 CPN'inst_0) ;
			  noName_878 ())
		    else ())
	   in noName_878 ()
	   end)
      fun init_61 i_401 = (init_res_2 (mark_4 i_401))
      fun size_27 i_402 = (size_26 (! (mark_4 i_402)))
      fun print_5 i_403 =
	  let val (size_28, ems_12) = extract_11 (! (mark_4 i_403))
	  in mkstr_ms_4 ems_12
	  end
      fun addto_3 noName_879 =
	  (fn noName_880 =>
	      (case (noName_879, noName_880) of
		 (i_404, ems_13) => (addto_2 (mark_4 i_404, ems_13))))
      fun subfrom_3 noName_881 =
	  (fn noName_882 =>
	      (case (noName_881, noName_882) of
		 (i_405, ems_14) => (subfrom_2 (mark_4 i_405, ems_14))))
      fun get_18 i_406 = (((fold_39 op ::) (! (mark_4 i_406))) [])
      fun set_6 noName_883 =
	  (fn noName_884 =>
	      (case (noName_883, noName_884) of
		 (i_407, ems_15) => (init_60 (mark_4 i_407, ems_15))))
      val _ =
	  (init_mark_1 := [] ;
	   operator_262 (init_mark_funs_0, set_init_mark_2))
      val _ =
	  (insert_4 instances_0) ("18", {print = print_5, size = size_27})
      val cmp_17 = a_cmp_0 lt_6
      val cmp_18 = cmp_17
      val tsize_2 = size_9
      val tfold_2 = fold_35
      fun tinsert_2 (TreeNil_0, col_57) = (singleton_0 col_57)
	| tinsert_2 (TreeNode_0
		     {left = left_29, right = right_29, value = value_36,...}

     ,
		     col_58) =
	  (case cmp_18 (value_36, col_58) of
	     LESS =>
	     (balance_0 (value_36, left_29, tinsert_2 (right_29, col_58)))
	   | GREATER =>
	     (balance_0 (value_36, tinsert_2 (left_29, col_58), right_29))
	   | EQUAL =>
	     (if (tsize_2 left_29) < (tsize_2 right_29)
	      then balance_0
		   (value_36, tinsert_2 (left_29, col_58), right_29)
	      else balance_0
		   (value_36, left_29, tinsert_2 (right_29, col_58))))
      fun taddto_2 (tree_38, []) = tree_38
	| taddto_2 (tree_39, col_59 :: rms_6) =
	  (taddto_2 (tinsert_2 (tree_39, col_59), rms_6))
      fun tdelete_2 (TreeNil_0, col_60) = (raise Subtract_0)
	| tdelete_2 (tree_40
		     as TreeNode_0
			{left = left_30, right = right_30, value = value_37,...

     },
		     col_61) =
	  (case cmp_18 (value_37, col_61) of
	     LESS =>
	     (balance_0 (value_37, left_30, tdelete_2 (right_30, col_61)))
	   | GREATER =>
	     (balance_0 (value_37, tdelete_2 (left_30, col_61), right_30))
	   | EQUAL => (join_5 (left_30, right_30)))
      fun tsubfrom_2 (tree_41, []) = tree_41
	| tsubfrom_2 (tree_42, col_62 :: rms_7) =
	  (tsubfrom_2 (tdelete_2 (tree_42, col_62), rms_7))
      fun empty_5 () = {list = [], tree = TreeNil_0}
      fun is_empty_2 {list = [], tree = TreeNil_0} = true
	| is_empty_2 _ = false
      fun fold_40 noName_885 =
	  (fn noName_886 =>
	      (fn noName_887 =>
		  (case (noName_885, noName_886, noName_887) of
		     (f_287, {list = list_49, tree = tree_43}, base_29) =>
		     (((tfold_2 f_287) (taddto_2 (tree_43, list_49)))
		      base_29))))
      fun size_29 {list = list_50, tree = tree_44} =
	  ((tsize_2 tree_44) + (length list_50))
      fun insert_20 (ims_25
		     as ref
			({list = list_51, tree = tree_45} :
			 {list : int list, tree : int tree_0}),
		     col_63) =
	  (ims_25 := {list = list_51, tree = tinsert_2 (tree_45, col_63)})
      fun addto_4 (ims_26, ems_16) =
	  ((List.app (fn x_230 => (insert_20 (ims_26, x_230)))) ems_16)
      fun init_62 (ims_27, ems_17) =
	  (ims_27 := (empty_5 ()) ; addto_4 (ims_27, ems_17))
      fun delete_4 (ims_28
		    as ref
		       ({list = [], tree = tree_46} :
			{list : int list, tree : int tree_0}),
		    col_64) =
	  (ims_28 := {list = [], tree = tdelete_2 (tree_46, col_64)})
	| delete_4 (ims_29
		    as ref
		       ({list = list_52 as value_38 :: res_7,
			 tree = tree_47} :
			{list : int list, tree : int tree_0}),
		    col_65) =
	  (case cmp_18 (col_65, value_38) of
	     EQUAL => (ims_29 := {list = res_7, tree = tree_47})
	   | _ =>
	     (ims_29
	      := {list = [],
		  tree = tdelete_2 (taddto_2 (tree_47, list_52), col_65)}))
      fun subfrom_4 (ims_30, ems_18) =
	  ((List.app (fn x_231 => (delete_4 (ims_30, x_231)))) ems_18)
      fun tmember_2 (TreeNil_0, _) = false
	| tmember_2 (TreeNode_0
		     {left = left_31, right = right_31, value = value_39,...}

     ,
		     col_66) =
	  (case cmp_18 (value_39, col_66) of
	     LESS => (tmember_2 (right_31, col_66))
	   | GREATER => (tmember_2 (left_31, col_66)) | EQUAL => true)
      fun lmember_2 ([], _) = false
	| lmember_2 (value_40 :: list_53, col_67) =
	  (case cmp_18 (value_40, col_67) of
	     EQUAL => true | _ => (lmember_2 (list_53, col_67)))
      fun member_2 ({list = list_54, tree = tree_48}, col_68) =
	  ((tmember_2 (tree_48, col_68))
	   orelse (lmember_2 (list_54, col_68)))
      fun tcf_2 (TreeNil_0, _) = 0
	| tcf_2 (TreeNode_0
		 {left = left_32, right = right_32, value = value_41,...},
		 col_69) =
	  (case cmp_18 (value_41, col_69) of
	     LESS => (tcf_2 (right_32, col_69))
	   | GREATER => (tcf_2 (left_32, col_69))
	   | EQUAL =>
	     ((1 + (tcf_2 (left_32, col_69))) + (tcf_2 (right_32, col_69)))

     )
      fun lcf_2 ([], _) = 0
	| lcf_2 (value_42 :: list_55, col_70) =
	  (case cmp_18 (value_42, col_70) of
	     EQUAL => (1 + (lcf_2 (list_55, col_70)))
	   | _ => (lcf_2 (list_55, col_70)))
      fun cf_5 ({list = list_56, tree = tree_49}, col_71) =
	  ((tcf_2 (tree_49, col_71)) + (lcf_2 (list_56, col_71)))
      fun subset_5 (ims_31 : {list : int list, tree : int tree_0},
		    ems_19 : int list) =
	  (((size_11 ems_19) <= (size_29 ims_31))
	   andalso let fun subset'_2 (coef_12, col_72, x_232 :: xs_36) =
			   (if lt_6 (x_232, col_72)
			    then (coef_12 <= (cf_5 (ims_31, col_72)))
				 andalso (subset'_2 (1, x_232, xs_36))
			    else subset'_2 (coef_12 + 1, col_72, xs_36))
			 | subset'_2 (coef_13, col_73, []) =
			   (coef_13 <= (cf_5 (ims_31, col_73)))
		   in case (sort_1 lt_6) ems_19 of
			(x_233 :: xs_37) => (subset'_2 (1, x_233, xs_37))
		      | [] => true
		   end)
      fun filter_11 noName_888 =
	  (fn noName_889 =>
	      (case (noName_888, noName_889) of
		 (func_3,
		  {list = list_57, tree = tree_50} :
		  {list : int list, tree : int tree_0}) =>
		 let fun filter'_2 (TreeNil_0, l_126) = l_126
		       | filter'_2 (TreeNode_0
				    {left = left_33,
				     right = right_33,
				     value = value_43,...},
				    l_127) =
			 (if func_3 value_43
			  then filter'_2
			       (left_33,
				value_43 :: (filter'_2 (right_33, l_127)))
			  else filter'_2
			       (left_33, filter'_2 (right_33, l_127)))
		 in filter'_2 (taddto_2 (tree_50, list_57), [])
		 end))
      fun collect_4 noName_890 =
	  (fn noName_891 =>
	      (case (noName_890, noName_891) of
		 (cmp'_3,
		  {list = list_58, tree = tree_51} :
		  {list : int list, tree : int tree_0}) =>
		 let fun collect'_2 (TreeNil_0, l_128) = l_128
		       | collect'_2 (TreeNode_0
				     {left = left_34,
				      right = right_34,
				      value = value_44,...},
				     l_129) =
			 (case cmp'_3 value_44 of
			    LESS => (collect'_2 (right_34, l_129))
			  | GREATER => (collect'_2 (left_34, l_129))
			  | EQUAL =>
			    (collect'_2
			     (left_34,
			      value_44 :: (collect'_2 (right_34, l_129)))))
		 in collect'_2 (taddto_2 (tree_51, list_58), [])
		 end))
      fun extract_12 ({list = list_59, tree = tree_52} :
		      {list : int list, tree : int tree_0}) =
	  let val tree'_6 = taddto_2 (tree_52, list_59)
	  in (tsize_2 tree'_6, ((tfold_2 op ::) tree'_6) [])
	  end
      fun init_res_3 (ims_32 as ref {list = list_60, tree = tree_53}) =
	  let val tree'_7 = taddto_2 (tree_53, list_60)
	  in (ims_32 := {list = [], tree = tree'_7} ; tsize_2 tree'_7)
	  end
      fun random_3 noName_892 =
	  (fn noName_893 =>
	      (case (noName_892, noName_893) of
		 (exn_29,
		  {tree = TreeNil_0,...} :
		  {list : int list, tree : int tree_0}) =>
		 (raise exn_29)
	       | (exn_30, {tree = tree_54,...}) =>
		 let fun get_19 (TreeNode_0
				 {left = left_35,
				  right = right_35,
				  value = value_45,...},
				 i_408) =
			 (case Int.compare (i_408, tsize_2 left_35) of
			    LESS => (get_19 (left_35, i_408))
			  | EQUAL => value_45
			  | GREATER =>
			    (get_19
			     (right_35, (i_408 - (tsize_2 left_35)) - 1)))
		       | get_19 _ = (raise exn_30)
		 in get_19 (tree_54, int_1 (tsize_2 tree_54))
		 end))
      fun random_res_3 noName_894 =
	  (fn noName_895 =>
	      (case (noName_894, noName_895) of
		 (exn_31, ref {tree = TreeNil_0,...}) => (raise exn_31)
	       | (_,
		  ims_33
		  as ref
		     ({list = list_61, tree = tree_55} :
		      {list : int list, tree : int tree_0})) =>
		 let fun get_20 (TreeNode_0
				 {left = left_36,
				  right = right_36,
				  value = value_46,...},
				 i_409) =
			 (case Int.compare (i_409, tsize_2 left_36) of
			    LESS =>
			    let val (left'_3, col'_6) =
				    get_20 (left_36, i_409)
			    in (balance_0 (value_46, left'_3, right_36),
				col'_6)
			    end
			  | GREATER =>
			    let val (right'_3, col'_7) =
				    get_20
				    (right_36,
				     (i_409 - (tsize_2 left_36)) - 1)
			    in (balance_0 (value_46, left_36, right'_3),
				col'_7)
			    end
			  | EQUAL => (join_5 (left_36, right_36), value_46))
		       | get_20 _ = (raise (InternalError_0 "random_res"))
		     val draw_3 = int_1 (tsize_2 tree_55)
		     val (tree'_8, col_74) = get_20 (tree_55, draw_3)
		 in (ims_33 := {list = col_74 :: list_61, tree = tree'_8} ;
		     col_74)
		 end))
      fun ms_random_res_2 noName_896 =
	  (fn noName_897 =>
	      (fn noName_898 =>
		  (case (noName_896, noName_897, noName_898) of
		     (_,
		      exn_32,
		      ref
		      ({list = [], tree = TreeNil_0} :
		       {list : int list, tree : int tree_0})) =>
		     (raise exn_32)
		   | ((reslist_2, cur_2, no_5),
		      exn_33,
		      ims_34 as ref {list = list_62, tree = tree_56}) =>
		     let val reserved_2 = length list_62
			 val notreserved_2 = tsize_2 tree_56
			 val index_18 =
			     (((! cur_2) + no_5) - 1)
			     mod (reserved_2 + notreserved_2)
		     in if (! cur_2) = (reserved_2 + notreserved_2)
			then raise exn_33
			else if index_18 >= reserved_2
			     then ((random_res_3 exn_33) ims_34)
				  before (inc_1 cur_2)
			     else (List.nth (list_62, index_18))
				  before (inc_1 cur_2)
		     end)))
      fun check_res_2 noName_899 =
	  (fn noName_900 =>
	      (fn noName_901 =>
		  (case (noName_899, noName_900, noName_901) of
		     (exn_34, f_288, ims_35) =>
		     let val col_75 = (random_res_3 exn_34) ims_35
		     in if f_288 col_75
			then col_75
			else ((check_res_2 exn_34) f_288) ims_35
		     end)))
      val no_of_inst_2 = 1
      val marking_2 =
	  Array.tabulate (no_of_inst_2, fn _ => (ref (empty_5 ())))
      val init_mark_2 = ref ([] : int list)
      fun mark_5 i_410 = (Array.sub (marking_2, i_410 - 1))
      fun set_init_mark_3 _ =
	  (CPN'inst_0 := 1 ;
	   let fun noName_902 () =
		   (if (! CPN'inst_0) <= no_of_inst_2
		    then ((init_62 (mark_5 (! CPN'inst_0), ! init_mark_2) ;
			   inc_1 CPN'inst_0) ;
			  noName_902 ())
		    else ())
	   in noName_902 ()
	   end)
      fun init_63 i_411 = (init_res_3 (mark_5 i_411))
      fun size_30 i_412 = (size_29 (! (mark_5 i_412)))
      fun print_6 i_413 =
	  let val (size_31, ems_20) = extract_12 (! (mark_5 i_413))
	  in mkstr_ms_1 ems_20
	  end
      fun addto_5 noName_903 =
	  (fn noName_904 =>
	      (case (noName_903, noName_904) of
		 (i_414, ems_21) => (addto_4 (mark_5 i_414, ems_21))))
      fun subfrom_5 noName_905 =
	  (fn noName_906 =>
	      (case (noName_905, noName_906) of
		 (i_415, ems_22) => (subfrom_4 (mark_5 i_415, ems_22))))
      fun get_21 i_416 = (((fold_40 op ::) (! (mark_5 i_416))) [])
      fun set_7 noName_907 =
	  (fn noName_908 =>
	      (case (noName_907, noName_908) of
		 (i_417, ems_23) => (init_62 (mark_5 i_417, ems_23))))
      val _ =
	  (init_mark_2 := [maxcnt_0] ;
	   operator_262 (init_mark_funs_0, set_init_mark_3))
      val _ =
	  (insert_4 instances_0) ("17", {print = print_6, size = size_30})
      val no_of_inst_3 = 1
      val marking_3 =
	  Array.tabulate (no_of_inst_3, fn _ => (ref (empty_5 ())))
      val init_mark_3 = ref ([] : int list)
      fun mark_6 i_418 = (Array.sub (marking_3, i_418 - 1))
      fun set_init_mark_4 _ =
	  (CPN'inst_0 := 1 ;
	   let fun noName_909 () =
		   (if (! CPN'inst_0) <= no_of_inst_3
		    then ((init_62 (mark_6 (! CPN'inst_0), ! init_mark_3) ;
			   inc_1 CPN'inst_0) ;
			  noName_909 ())
		    else ())
	   in noName_909 ()
	   end)
      fun init_64 i_419 = (init_res_3 (mark_6 i_419))
      fun size_32 i_420 = (size_29 (! (mark_6 i_420)))
      fun print_7 i_421 =
	  let val (size_33, ems_24) = extract_12 (! (mark_6 i_421))
	  in mkstr_ms_1 ems_24
	  end
      fun addto_6 noName_910 =
	  (fn noName_911 =>
	      (case (noName_910, noName_911) of
		 (i_422, ems_25) => (addto_4 (mark_6 i_422, ems_25))))
      fun subfrom_6 noName_912 =
	  (fn noName_913 =>
	      (case (noName_912, noName_913) of
		 (i_423, ems_26) => (subfrom_4 (mark_6 i_423, ems_26))))
      fun get_22 i_424 = (((fold_40 op ::) (! (mark_6 i_424))) [])
      fun set_8 noName_914 =
	  (fn noName_915 =>
	      (case (noName_914, noName_915) of
		 (i_425, ems_27) => (init_62 (mark_6 i_425, ems_27))))
      val _ =
	  (init_mark_3 := (operator_265 (1, ~1)) ;
	   operator_262 (init_mark_funs_0, set_init_mark_4))
      val _ =
	  (insert_4 instances_0) ("16", {print = print_7, size = size_32})
      val cmp_19 = a_cmp_0 lt_13
      val cmp_20 = cmp_19
      val tsize_3 = size_9
      val tfold_3 = fold_35
      fun tinsert_3 (TreeNil_0, col_76) = (singleton_0 col_76)
	| tinsert_3 (TreeNode_0
		     {left = left_37, right = right_37, value = value_47,...}

     ,
		     col_77) =
	  (case cmp_20 (value_47, col_77) of
	     LESS =>
	     (balance_0 (value_47, left_37, tinsert_3 (right_37, col_77)))
	   | GREATER =>
	     (balance_0 (value_47, tinsert_3 (left_37, col_77), right_37))
	   | EQUAL =>
	     (if (tsize_3 left_37) < (tsize_3 right_37)
	      then balance_0
		   (value_47, tinsert_3 (left_37, col_77), right_37)
	      else balance_0
		   (value_47, left_37, tinsert_3 (right_37, col_77))))
      fun taddto_3 (tree_57, []) = tree_57
	| taddto_3 (tree_58, col_78 :: rms_8) =
	  (taddto_3 (tinsert_3 (tree_58, col_78), rms_8))
      fun tdelete_3 (TreeNil_0, col_79) = (raise Subtract_0)
	| tdelete_3 (tree_59
		     as TreeNode_0
			{left = left_38, right = right_38, value = value_48,...

     },
		     col_80) =
	  (case cmp_20 (value_48, col_80) of
	     LESS =>
	     (balance_0 (value_48, left_38, tdelete_3 (right_38, col_80)))
	   | GREATER =>
	     (balance_0 (value_48, tdelete_3 (left_38, col_80), right_38))
	   | EQUAL => (join_5 (left_38, right_38)))
      fun tsubfrom_3 (tree_60, []) = tree_60
	| tsubfrom_3 (tree_61, col_81 :: rms_9) =
	  (tsubfrom_3 (tdelete_3 (tree_61, col_81), rms_9))
      fun empty_6 () = {list = [], tree = TreeNil_0}
      fun is_empty_3 {list = [], tree = TreeNil_0} = true
	| is_empty_3 _ = false
      fun fold_41 noName_916 =
	  (fn noName_917 =>
	      (fn noName_918 =>
		  (case (noName_916, noName_917, noName_918) of
		     (f_289, {list = list_63, tree = tree_62}, base_30) =>
		     (((tfold_3 f_289) (taddto_3 (tree_62, list_63)))
		      base_30))))
      fun size_34 {list = list_64, tree = tree_63} =
	  ((tsize_3 tree_63) + (length list_64))
      fun insert_21 (ims_36
		     as ref
			({list = list_65, tree = tree_64} :
			 {list : E_0 list, tree : E_0 tree_0}),
		     col_82) =
	  (ims_36 := {list = list_65, tree = tinsert_3 (tree_64, col_82)})
      fun addto_7 (ims_37, ems_28) =
	  ((List.app (fn x_234 => (insert_21 (ims_37, x_234)))) ems_28)
      fun init_65 (ims_38, ems_29) =
	  (ims_38 := (empty_6 ()) ; addto_7 (ims_38, ems_29))
      fun delete_5 (ims_39
		    as ref
		       ({list = [], tree = tree_65} :
			{list : E_0 list, tree : E_0 tree_0}),
		    col_83) =
	  (ims_39 := {list = [], tree = tdelete_3 (tree_65, col_83)})
	| delete_5 (ims_40
		    as ref
		       ({list = list_66 as value_49 :: res_8,
			 tree = tree_66} :
			{list : E_0 list, tree : E_0 tree_0}),
		    col_84) =
	  (case cmp_20 (col_84, value_49) of
	     EQUAL => (ims_40 := {list = res_8, tree = tree_66})
	   | _ =>
	     (ims_40
	      := {list = [],
		  tree = tdelete_3 (taddto_3 (tree_66, list_66), col_84)}))
      fun subfrom_7 (ims_41, ems_30) =
	  ((List.app (fn x_235 => (delete_5 (ims_41, x_235)))) ems_30)
      fun tmember_3 (TreeNil_0, _) = false
	| tmember_3 (TreeNode_0
		     {left = left_39, right = right_39, value = value_50,...}

     ,
		     col_85) =
	  (case cmp_20 (value_50, col_85) of
	     LESS => (tmember_3 (right_39, col_85))
	   | GREATER => (tmember_3 (left_39, col_85)) | EQUAL => true)
      fun lmember_3 ([], _) = false
	| lmember_3 (value_51 :: list_67, col_86) =
	  (case cmp_20 (value_51, col_86) of
	     EQUAL => true | _ => (lmember_3 (list_67, col_86)))
      fun member_3 ({list = list_68, tree = tree_67}, col_87) =
	  ((tmember_3 (tree_67, col_87))
	   orelse (lmember_3 (list_68, col_87)))
      fun tcf_3 (TreeNil_0, _) = 0
	| tcf_3 (TreeNode_0
		 {left = left_40, right = right_40, value = value_52,...},
		 col_88) =
	  (case cmp_20 (value_52, col_88) of
	     LESS => (tcf_3 (right_40, col_88))
	   | GREATER => (tcf_3 (left_40, col_88))
	   | EQUAL =>
	     ((1 + (tcf_3 (left_40, col_88))) + (tcf_3 (right_40, col_88)))

     )
      fun lcf_3 ([], _) = 0
	| lcf_3 (value_53 :: list_69, col_89) =
	  (case cmp_20 (value_53, col_89) of
	     EQUAL => (1 + (lcf_3 (list_69, col_89)))
	   | _ => (lcf_3 (list_69, col_89)))
      fun cf_6 ({list = list_70, tree = tree_68}, col_90) =
	  ((tcf_3 (tree_68, col_90)) + (lcf_3 (list_70, col_90)))
      fun subset_6 (ims_42 : {list : E_0 list, tree : E_0 tree_0},
		    ems_31 : E_0 list) =
	  (((size_11 ems_31) <= (size_34 ims_42))
	   andalso let fun subset'_3 (coef_14, col_91, x_236 :: xs_38) =
			   (if lt_13 (x_236, col_91)
			    then (coef_14 <= (cf_6 (ims_42, col_91)))
				 andalso (subset'_3 (1, x_236, xs_38))
			    else subset'_3 (coef_14 + 1, col_91, xs_38))
			 | subset'_3 (coef_15, col_92, []) =
			   (coef_15 <= (cf_6 (ims_42, col_92)))
		   in case (sort_1 lt_13) ems_31 of
			(x_237 :: xs_39) => (subset'_3 (1, x_237, xs_39))
		      | [] => true
		   end)
      fun filter_12 noName_919 =
	  (fn noName_920 =>
	      (case (noName_919, noName_920) of
		 (func_4,
		  {list = list_71, tree = tree_69} :
		  {list : E_0 list, tree : E_0 tree_0}) =>
		 let fun filter'_3 (TreeNil_0, l_130) = l_130
		       | filter'_3 (TreeNode_0
				    {left = left_41,
				     right = right_41,
				     value = value_54,...},
				    l_131) =
			 (if func_4 value_54
			  then filter'_3
			       (left_41,
				value_54 :: (filter'_3 (right_41, l_131)))
			  else filter'_3
			       (left_41, filter'_3 (right_41, l_131)))
		 in filter'_3 (taddto_3 (tree_69, list_71), [])
		 end))
      fun collect_5 noName_921 =
	  (fn noName_922 =>
	      (case (noName_921, noName_922) of
		 (cmp'_4,
		  {list = list_72, tree = tree_70} :
		  {list : E_0 list, tree : E_0 tree_0}) =>
		 let fun collect'_3 (TreeNil_0, l_132) = l_132
		       | collect'_3 (TreeNode_0
				     {left = left_42,
				      right = right_42,
				      value = value_55,...},
				     l_133) =
			 (case cmp'_4 value_55 of
			    LESS => (collect'_3 (right_42, l_133))
			  | GREATER => (collect'_3 (left_42, l_133))
			  | EQUAL =>
			    (collect'_3
			     (left_42,
			      value_55 :: (collect'_3 (right_42, l_133)))))
		 in collect'_3 (taddto_3 (tree_70, list_72), [])
		 end))
      fun extract_13 ({list = list_73, tree = tree_71} :
		      {list : E_0 list, tree : E_0 tree_0}) =
	  let val tree'_9 = taddto_3 (tree_71, list_73)
	  in (tsize_3 tree'_9, ((tfold_3 op ::) tree'_9) [])
	  end
      fun init_res_4 (ims_43 as ref {list = list_74, tree = tree_72}) =
	  let val tree'_10 = taddto_3 (tree_72, list_74)
	  in (ims_43 := {list = [], tree = tree'_10} ; tsize_3 tree'_10)
	  end
      fun random_4 noName_923 =
	  (fn noName_924 =>
	      (case (noName_923, noName_924) of
		 (exn_35,
		  {tree = TreeNil_0,...} :
		  {list : E_0 list, tree : E_0 tree_0}) =>
		 (raise exn_35)
	       | (exn_36, {tree = tree_73,...}) =>
		 let fun get_23 (TreeNode_0
				 {left = left_43,
				  right = right_43,
				  value = value_56,...},
				 i_426) =
			 (case Int.compare (i_426, tsize_3 left_43) of
			    LESS => (get_23 (left_43, i_426))
			  | EQUAL => value_56
			  | GREATER =>
			    (get_23
			     (right_43, (i_426 - (tsize_3 left_43)) - 1)))
		       | get_23 _ = (raise exn_36)
		 in get_23 (tree_73, int_1 (tsize_3 tree_73))
		 end))
      fun random_res_4 noName_925 =
	  (fn noName_926 =>
	      (case (noName_925, noName_926) of
		 (exn_37, ref {tree = TreeNil_0,...}) => (raise exn_37)
	       | (_,
		  ims_44
		  as ref
		     ({list = list_75, tree = tree_74} :
		      {list : E_0 list, tree : E_0 tree_0})) =>
		 let fun get_24 (TreeNode_0
				 {left = left_44,
				  right = right_44,
				  value = value_57,...},
				 i_427) =
			 (case Int.compare (i_427, tsize_3 left_44) of
			    LESS =>
			    let val (left'_4, col'_8) =
				    get_24 (left_44, i_427)
			    in (balance_0 (value_57, left'_4, right_44),
				col'_8)
			    end
			  | GREATER =>
			    let val (right'_4, col'_9) =
				    get_24
				    (right_44,
				     (i_427 - (tsize_3 left_44)) - 1)
			    in (balance_0 (value_57, left_44, right'_4),
				col'_9)
			    end
			  | EQUAL => (join_5 (left_44, right_44), value_57))
		       | get_24 _ = (raise (InternalError_0 "random_res"))
		     val draw_4 = int_1 (tsize_3 tree_74)
		     val (tree'_11, col_93) = get_24 (tree_74, draw_4)
		 in (ims_44 := {list = col_93 :: list_75, tree = tree'_11} ;
		     col_93)
		 end))
      fun ms_random_res_3 noName_927 =
	  (fn noName_928 =>
	      (fn noName_929 =>
		  (case (noName_927, noName_928, noName_929) of
		     (_,
		      exn_38,
		      ref
		      ({list = [], tree = TreeNil_0} :
		       {list : E_0 list, tree : E_0 tree_0})) =>
		     (raise exn_38)
		   | ((reslist_3, cur_3, no_6),
		      exn_39,
		      ims_45 as ref {list = list_76, tree = tree_75}) =>
		     let val reserved_3 = length list_76
			 val notreserved_3 = tsize_3 tree_75
			 val index_19 =
			     (((! cur_3) + no_6) - 1)
			     mod (reserved_3 + notreserved_3)
		     in if (! cur_3) = (reserved_3 + notreserved_3)
			then raise exn_39
			else if index_19 >= reserved_3
			     then ((random_res_4 exn_39) ims_45)
				  before (inc_1 cur_3)
			     else (List.nth (list_76, index_19))
				  before (inc_1 cur_3)
		     end)))
      fun check_res_3 noName_930 =
	  (fn noName_931 =>
	      (fn noName_932 =>
		  (case (noName_930, noName_931, noName_932) of
		     (exn_40, f_290, ims_46) =>
		     let val col_94 = (random_res_4 exn_40) ims_46
		     in if f_290 col_94
			then col_94
			else ((check_res_3 exn_40) f_290) ims_46
		     end)))
      val no_of_inst_4 = 1
      val marking_4 =
	  Array.tabulate (no_of_inst_4, fn _ => (ref (empty_6 ())))
      val init_mark_4 = ref ([] : E_0 list)
      fun mark_7 i_428 = (Array.sub (marking_4, i_428 - 1))
      fun set_init_mark_5 _ =
	  (CPN'inst_0 := 1 ;
	   let fun noName_933 () =
		   (if (! CPN'inst_0) <= no_of_inst_4
		    then ((init_65 (mark_7 (! CPN'inst_0), ! init_mark_4) ;
			   inc_1 CPN'inst_0) ;
			  noName_933 ())
		    else ())
	   in noName_933 ()
	   end)
      fun init_66 i_429 = (init_res_4 (mark_7 i_429))
      fun size_35 i_430 = (size_34 (! (mark_7 i_430)))
      fun print_8 i_431 =
	  let val (size_36, ems_32) = extract_13 (! (mark_7 i_431))
	  in mkstr_ms_6 ems_32
	  end
      fun addto_8 noName_934 =
	  (fn noName_935 =>
	      (case (noName_934, noName_935) of
		 (i_432, ems_33) => (addto_7 (mark_7 i_432, ems_33))))
      fun subfrom_8 noName_936 =
	  (fn noName_937 =>
	      (case (noName_936, noName_937) of
		 (i_433, ems_34) => (subfrom_7 (mark_7 i_433, ems_34))))
      fun get_25 i_434 = (((fold_41 op ::) (! (mark_7 i_434))) [])
      fun set_9 noName_938 =
	  (fn noName_939 =>
	      (case (noName_938, noName_939) of
		 (i_435, ems_35) => (init_65 (mark_7 i_435, ems_35))))
      val _ =
	  (init_mark_4 := [e_28] ;
	   operator_262 (init_mark_funs_0, set_init_mark_5))
      val _ =
	  (insert_4 instances_0) ("15", {print = print_8, size = size_35})
      fun CPN'transition20_0 (CPN'mode_0, CPN'inst_1) =
	  let val CPN'id_0 = "20"
	      val CPN'bh1_0 = ref ([] : {count : int} list)
	      val (CPN'enough_tokens_0, CPN'answer_0) =
		  each_place_0
		  (1 <= (init_63 CPN'inst_1), (true, is_disabled_0))
	      fun CPN'bindfun_0 () =
		  let fun CPN'bf2_0 () = ()
		      fun CPN'bf1_0 () =
			  let val _ = init_res_3 (mark_5 CPN'inst_1)
			      fun CPN'bf_0 () =
				  (let val count_3 =
					   (random_res_3 BindFatalFailure_0)
					   (mark_5 CPN'inst_1)
				   in (if count_3 < maxcnt_0
				       then (operator_262
					     (CPN'bh1_0, {count = count_3}) ;
					     case CPN'mode_0 of
					       bind_1 => (raise BindFailure_0)
					     | _ => (CPN'bf2_0 ()))
				       else raise BindFailure_0)
				      handle BindFailure_0 => (CPN'bf_0 ())
				   end
				   handle Bind => (CPN'bf_0 ()))
			  in CPN'bf_0 ()
			  end
		  in case CPN'mode_0 of
		       bind_1 => ((CPN'bf1_0 ()) handle _ => ())
		     | _ => (CPN'bf1_0 ())
		  end
	      fun CPN'occfun_0 {count = count_4} =
		  (delete_4 (mark_5 CPN'inst_1, count_4) ;
		   insert_20 (mark_5 CPN'inst_1, count_4 + 1) ;
		   (is_executed_0,
		    if ! report_bindings_0
		    then ["\n - count = ", mkstr_3 count_4]
		    else []))
	  in if CPN'enough_tokens_0
	     then (CPN'bindfun_0 () ;
		   case CPN'mode_0 of
		     fast_0 => (CPN'occfun_0 (CPN'hd_0 (! CPN'bh1_0)))
		   | _ => (is_executed_0, []))
		  handle BindFatalFailure_0 => (CPN'answer_0, [])
	     else (CPN'answer_0, [])
	  end
      val _ = add_be_0 ("20", CPN'transition20_0)
      val CPN'code_action23_0 =
	  code_action_0
	  (fn CPN'inst_2 => (fn (st_3 : string) => (kitstop (*logit_0 changed 2001-02-17, Niels*) st_3)))
      fun CPN'transition23_0 (CPN'mode_1, CPN'inst_3) =
	  let val CPN'id_1 = "23"
	      val CPN'bh1_1 = ref ([] : {st : string} list)
	      val (CPN'enough_tokens_1, CPN'answer_1) =
		  each_place_0
		  (1 <= (init_61 CPN'inst_3), (true, is_disabled_0))
	      fun CPN'bindfun_1 () =
		  let fun CPN'bf2_1 () = ()
		      fun CPN'bf1_1 () =
			  let val _ = init_res_2 (mark_4 CPN'inst_3)
			      fun CPN'bf_1 () =
				  (let val st_4 =
					   (random_res_2 BindFatalFailure_0)
					   (mark_4 CPN'inst_3)
				   in (operator_262 (CPN'bh1_1, {st = st_4}) ;
				       case CPN'mode_1 of
					 bind_1 => (raise BindFailure_0)
				       | _ => (CPN'bf2_1 ()))
				      handle BindFailure_0 => (CPN'bf_1 ())
				   end
				   handle Bind => (CPN'bf_1 ()))
			  in CPN'bf_1 ()
			  end
		  in case CPN'mode_1 of
		       bind_1 => ((CPN'bf1_1 ()) handle _ => ())
		     | _ => (CPN'bf1_1 ())
		  end
	      fun CPN'occfun_1 {st = st_5} =
		  let val () = (CPN'code_action23_0 CPN'inst_3) st_5
		      val _ = delete_3 (mark_4 CPN'inst_3, st_5)
		      val _ = insert_20 (mark_5 CPN'inst_3, 0)
		  in (is_executed_0,
		      if ! report_bindings_0
		      then ["\n - st = ", mkstr_6 st_5]
		      else [])
		  end
	  in if CPN'enough_tokens_1
	     then (CPN'bindfun_1 () ;
		   case CPN'mode_1 of
		     fast_0 => (CPN'occfun_1 (CPN'hd_0 (! CPN'bh1_1)))
		   | _ => (is_executed_0, []))
		  handle BindFatalFailure_0 => (CPN'answer_1, [])
	     else (CPN'answer_1, [])
	  end
      val _ = add_be_0 ("23", CPN'transition23_0)
      val CPN'code_action33_0 =
	  code_action_0 (fn CPN'inst_4 => (fn () => (init_time_0 ())))
      fun CPN'transition33_0 (CPN'mode_2, CPN'inst_5) =
	  let val CPN'id_2 = "33"
	      val (CPN'enough_tokens_2, CPN'answer_2) =
		  each_place_0
		  (1 <= (init_66 CPN'inst_5), (true, is_disabled_0))
	      fun CPN'bindfun_2 () =
		  let fun CPN'bf2_2 () = ()
		      fun CPN'bf1_2 () =
			  (if member_3 (! (mark_7 CPN'inst_5), e_28)
			   then case CPN'mode_2 of
				  bind_1 => (raise BindFatalFailure_0)
				| _ => (CPN'bf2_2 ())
			   else raise BindFatalFailure_0)
		  in case CPN'mode_2 of
		       bind_1 => ((CPN'bf1_2 ()) handle _ => ())
		     | _ => (CPN'bf1_2 ())
		  end
	      fun CPN'occfun_2 () =
		  let val () = (CPN'code_action33_0 CPN'inst_5) ()
		      val _ = delete_5 (mark_7 CPN'inst_5, e_28)
		      val _ = insert_18 (mark_3 CPN'inst_5, time_zero_0)
		  in (is_executed_0, if ! report_bindings_0 then [] else [])
		  end
	  in if CPN'enough_tokens_2
	     then (CPN'bindfun_2 () ;
		   case CPN'mode_2 of
		     fast_0 => (CPN'occfun_2 ())
		   | _ => (is_executed_0, []))
		  handle BindFatalFailure_0 => (CPN'answer_2, [])
	     else (CPN'answer_2, [])
	  end
      val _ = add_be_0 ("33", CPN'transition33_0)
      val CPN'code_action26_0 =
	  code_action_0
	  (fn CPN'inst_6 =>
	      (fn (clock_0 : (int * int * int * int), count_5 : int) =>
		  (((addsecs_0 clock_0) (rel_time_0 ())) count_5)))
      fun CPN'transition26_0 (CPN'mode_3, CPN'inst_7) =
	  let val CPN'id_3 = "26"
	      val CPN'bh2_0 =
		  ref ([] : {clock : (int * int * int * int)} list)
	      val CPN'bh3_0 = ref ([] : {count : int} list)
	      val (CPN'enough_tokens_3, CPN'answer_3) =
		  each_place_0
		  (1 <= (init_59 CPN'inst_7),
		   each_place_0
		   (1 <= (init_64 CPN'inst_7),
		    each_place_0
		    (1 <= (init_63 CPN'inst_7), (true, is_disabled_0))))
	      fun CPN'bindfun_3 () =
		  let fun CPN'bf4_0 () = ()
		      fun CPN'bf3_0 () =
			  let val _ = init_res_3 (mark_6 CPN'inst_7)
			      fun CPN'bf_2 () =
				  (let val count_6 =
					   (random_res_3 BindFatalFailure_0)
					   (mark_6 CPN'inst_7)
				   in (operator_262
				       (CPN'bh3_0, {count = count_6}) ;
				       case CPN'mode_3 of
					 bind_1 => (raise BindFailure_0)
				       | _ => (CPN'bf4_0 ()))
				      handle BindFailure_0 => (CPN'bf_2 ())
				   end
				   handle Bind => (CPN'bf_2 ()))
			  in CPN'bf_2 ()
			  end
		      fun CPN'bf2_3 () =
			  let val _ = init_res_1 (mark_3 CPN'inst_7)
			      fun CPN'bf_3 () =
				  (let val clock_1 =
					   (random_res_1 BindFatalFailure_0)
					   (mark_3 CPN'inst_7)
				   in (operator_262
				       (CPN'bh2_0, {clock = clock_1}) ;
				       case CPN'mode_3 of
					 bind_1 => (raise BindFailure_0)
				       | _ => (CPN'bf3_0 ()))
				      handle BindFailure_0 => (CPN'bf_3 ())
				   end
				   handle Bind => (CPN'bf_3 ()))
			  in CPN'bf_3 ()
			  end
		      fun CPN'bf1_3 () =
			  (if member_2 (! (mark_5 CPN'inst_7), maxcnt_0)
			   then case CPN'mode_3 of
				  bind_1 => (raise BindFatalFailure_0)
				| _ => (CPN'bf2_3 ())
			   else raise BindFatalFailure_0)
		  in case CPN'mode_3 of
		       bind_1 =>
		       ((CPN'bf1_3 ()) handle _ => () ;
			(CPN'bf2_3 ()) handle _ => () ;
			(CPN'bf3_0 ()) handle _ => ())
		     | _ => (CPN'bf1_3 ())
		  end
	      fun CPN'occfun_3 ({clock = clock_2}, {count = count_7}) =
		  let val (t_66, newcount_0, newclock_0) =
			  (CPN'code_action26_0 CPN'inst_7) (clock_2, count_7)
		      val _ =
			  (delete_2 (mark_3 CPN'inst_7, clock_2) ;
			   delete_4 (mark_6 CPN'inst_7, count_7) ;
			   delete_4 (mark_5 CPN'inst_7, maxcnt_0))
		      val _ =
			  (insert_20 (mark_6 CPN'inst_7, newcount_0) ;
			   insert_19
			   (mark_4 CPN'inst_7,
			    if newclock_0 = time_zero_0
			    then "Start\n"
			    else (((((mkstr_7 newclock_0)
				     ^ "      counts/sekund= ")
				    ^ (Int.toString
				       (if t_66 <> 0
					then maxcnt_0 div t_66
					else ~1)))
				   ^ "     average= ")
				  ^ (Int.toString
				     (if t_66 <> 0
				      then floor
					   (((real newcount_0)
					     * (real maxcnt_0))
					    / (tosec_0 newclock_0))
				      else ~1)))
				 ^ "\n") ;
			   insert_18 (mark_3 CPN'inst_7, newclock_0))
		  in (is_executed_0,
		      if ! report_bindings_0
		      then ["\n - clock = ",
			    mkstr_7 clock_2,
			    "\n - count = ",
			    mkstr_3 count_7]
		      else [])
		  end
	  in if CPN'enough_tokens_3
	     then (CPN'bindfun_3 () ;
		   case CPN'mode_3 of
		     fast_0 =>
		     (CPN'occfun_3
		      (CPN'hd_0 (! CPN'bh2_0), CPN'hd_0 (! CPN'bh3_0)))
		   | _ => (is_executed_0, []))
		  handle BindFatalFailure_0 => (CPN'answer_3, [])
	     else (CPN'answer_3, [])
	  end
      val _ = add_be_0 ("26", CPN'transition26_0)
      val _ =
	  load_inst_0
	  (4,
	   [("20", {dep_list = [[0, 1]], index = (0, 0), name = "count"})

     ,
	    ("23", {dep_list = [[0, 1]], index = (2, 2), name = "out"}),
	    ("33", {dep_list = [[1]], index = (3, 3), name = "set"}),
	    ("26", {dep_list = [[1, 2]], index = (1, 1), name = "get"})])
      val _ = init_state_0 ()
      val _ = run_6 ()
in 
end
