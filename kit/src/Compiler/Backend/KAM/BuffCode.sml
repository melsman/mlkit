(* To buffer bytecode during emission *)
(* Taken from the Moscow ML compiler  *)

functor BuffCode () : BUFF_CODE =
  struct
    local
      fun make_buffer n = Word8Array.array(n, Word8.fromInt 0);
      fun incr r = r := !r + 1;
    in
      val out_buffer = ref (make_buffer 512)
      val out_position = ref 0

      fun realloc_out_buffer () =
	let 
	  val len = Word8Array.length (!out_buffer)
	  val new_buffer = make_buffer (2 * len)
	in
	  Word8Array.copy { src = !out_buffer, si = 0, len = NONE,
			  dst = new_buffer, di = 0 };
	  out_buffer := new_buffer
	end

      fun init_out_code () = (out_position := 0)

      fun out_w8 (b : Word8.word) =
	let
          val out_w8 = b
	  val _ = print (Word8.toString out_w8 ^ ",")
	in
	  (if !out_position < Word8Array.length (!out_buffer) then 
	     () 
	   else
	     realloc_out_buffer();
	  Word8Array.update(!out_buffer, !out_position, out_w8);
	  incr out_position)
	end

      fun wtow8 (w : Word.word) = Word8.fromLargeWord (Word31.toLargeWord w)
      fun w32tow8 (w : Word32.word) = Word8.fromLargeWord (Word32.toLargeWord w)
      fun itow8 (i : int) = Word8.fromInt i

      val out_w = out_w8 o wtow8

      fun out_i (b : int) = out_w8 (itow8 b)
	
      fun out_short_w (s : Word.word)  =
	(out_w8 (wtow8 s);
	 out_w8 (wtow8 (Word.>> (s,Word.fromInt 8))))

      fun out_short_i (s : int)  = 
	(out_w8 (wtow8 (Word.fromInt s));
	 out_w8 (wtow8 (Word.~>> (Word.fromInt s,Word.fromInt 8))))

      fun out_long_w (l : Word.word) =
	(out_w8 (wtow8 l);
	 out_w8 (wtow8 (Word.>> (l,Word.fromInt 8)));
	 out_w8 (wtow8 (Word.>> (l,Word.fromInt 16)));
	 out_w8 (wtow8 (Word.>> (l,Word.fromInt 24))))

      fun out_long_i (l : int)  =
	(out_w8 (itow8 l);
	 out_w8 (wtow8 (Word.>> (Word.fromInt l,Word.fromInt 8)));
	 out_w8 (wtow8 (Word.>> (Word.fromInt l,Word.fromInt 16)));
	 out_w8 (wtow8 (Word.~>> (Word.fromInt l,Word.fromInt 24))))

      fun out_long_w32 (l : Word32.word) = 
	(out_w8 (w32tow8 l);
	 out_w8 (w32tow8 (Word32.>> (l,Word.fromInt 8)));
	 out_w8 (w32tow8 (Word32.>> (l,Word.fromInt 16)));
	 out_w8 (w32tow8 (Word32.>> (l,Word.fromInt 24))))

      fun out_long_w32' (os, l : Word32.word) = 
	(BinIO.output1 (os, w32tow8 l);
	 BinIO.output1 (os, w32tow8 (Word32.>> (l,Word.fromInt 8)));
	 BinIO.output1 (os, w32tow8 (Word32.>> (l,Word.fromInt 16)));	 
	 BinIO.output1 (os, w32tow8 (Word32.>> (l,Word.fromInt 24))))
	 
      fun out_pairs (os, ps) =
	app (fn (a,b) => (print ("  (" ^ Int.toString a ^ ", " ^ Int.toString b ^ ")\n");
			  out_long_w32'(os, Word32.fromInt a);
			  out_long_w32'(os, Word32.fromInt b))) ps

      fun dump_buffer {filename : string, 
		       main_lab_opt : int option,
		       map_import_code : (int * int) list,      (* (address,label)-pairs *)
		       map_import_data : (int * int) list,      (* (address,label)-pairs *)
		       map_export_code : (int * int) list,      (* (label,address)-pairs *)
		       map_export_data : (int * int) list} =    (* (label,address)-pairs *)
	let
	  val os : BinIO.outstream = BinIO.openOut filename
	  val main_lab = case main_lab_opt
			   of SOME i => i
			    | NONE => 0
	  val magic = case Word32.fromString "0x4b303031" (*K001*)
			of SOME magic => magic
			 | NONE => raise Fail "NO WAY!"
	in
	  print ("Out position is " ^ Int.toString (!out_position) ^ "\n");
	  (out_long_w32'(os, Word32.fromInt (!out_position));
	   out_long_w32'(os, Word32.fromInt main_lab);
	   out_long_w32'(os, Word32.fromInt (List.length map_import_code));
	   out_long_w32'(os, Word32.fromInt (List.length map_import_data));
	   out_long_w32'(os, Word32.fromInt (List.length map_export_code));
	   out_long_w32'(os, Word32.fromInt (List.length map_export_data));
	   out_long_w32'(os, magic);
	   BinIO.output(os, Word8Array.extract(!out_buffer, 0, SOME (!out_position)));
	   print ("Writing code import (address,label)-pairs\n");
	   out_pairs(os, map_import_code);
	   print ("Writing data import (address,label)-pairs\n");
	   out_pairs(os, map_import_data);
	   print ("Writing code export (label,address)-pairs\n");
	   out_pairs(os, map_export_code);
	   print ("Writing data export (label,address)-pairs\n");
	   out_pairs(os, map_export_data);
	   BinIO.closeOut os) handle E => (BinIO.closeOut os; raise E)
	end 
    end
  end
