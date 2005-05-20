(* To buffer bytecode during emission *)
(* Taken from the Moscow ML compiler  *)

structure BuffCode : BUFF_CODE =
  struct
    type key = int * string
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
	    Word8Array.foldl (fn (e,i) => 
			      (Word8Array.update(new_buffer,i,e); i+1)) 
	    0 (!out_buffer);
	    out_buffer := new_buffer
	end

      fun init_out_code () = (out_position := 0)

      fun out_w8 (b : Word8.word) =
	let
          val out_w8 = b
(*	  val _ = print (Word8.toString out_w8 ^ ",") *)
	in
	  (if !out_position < Word8Array.length (!out_buffer) then 
	     () 
	   else
	     realloc_out_buffer();
	  Word8Array.update(!out_buffer, !out_position, out_w8);
	  incr out_position)
	end

      fun wtow8 (w : Word.word) = Word8.fromLargeWord (Word.toLargeWord w)
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

      fun out_long_i32 (l : Int32.int) =
	out_long_w32 (Word32.fromLargeInt (Int32.toLarge l))

      fun out_long_w32' (os, l : Word32.word) = 
	(BinIO.output1 (os, w32tow8 l);
	 BinIO.output1 (os, w32tow8 (Word32.>> (l,Word.fromInt 8)));
	 BinIO.output1 (os, w32tow8 (Word32.>> (l,Word.fromInt 16)));	 
	 BinIO.output1 (os, w32tow8 (Word32.>> (l,Word.fromInt 24))))

      fun out_real (r : real) : unit =
	  Word8Vector.app out_w8 (PackRealLittle.toBytes r)

      fun out_string (os,s:string) : unit =
	  let val sz = size s
	  in out_long_w32' (os, Word32.fromInt sz)
	      ; BinIO.output (os, Byte.stringToBytes s)
	      
	  end

      fun out_lab (os, lab) =
	  let val (i,s) = lab
	  in out_long_w32'(os, Word32.fromInt i)
	      ; out_string(os, s)
	  end

      fun out_addr (os, addr) =
	  out_long_w32'(os, Word32.fromInt addr)

      fun out_addr_lab_pairs (os, ps) =
	  app (fn (addr,lab) => (out_addr (os,addr) ; out_lab(os,lab))) ps

      fun out_lab_addr_pairs (os, ps) =
	  app (fn (lab,addr) => (out_lab(os,lab); out_addr (os,addr))) ps

      fun extract(a,n) =
	  Word8Vector.tabulate(n,fn i => Word8Array.sub(a,i))

      fun dump_buffer {filename : string, 
		       main_lab_opt : key option,
		       map_import_code : (int * key) list,      (* (address,label)-pairs *) (* meaning: at address in bytecode, there is a use of the label *)
		       map_import_data : (int * key) list,      (* (address,label)-pairs *)
		       map_export_code : (key * int) list,      (* (label,address)-pairs *) (* meaning: function labeled label is defined at address *)
		       map_export_data : (key * int) list} =    (* (label,address)-pairs *)
	let
	  val os : BinIO.outstream = BinIO.openOut filename
	  val main_lab = case main_lab_opt
			   of SOME lab => lab
			    | NONE => (0,"")
	  val magic = case Word32.fromString "0x4b303031" (*K001*)
			of SOME magic => magic
			 | NONE => raise Fail "NO WAY!"
	in
(*	  print ("Out position is " ^ Int.toString (!out_position) ^ "\n"); *)
	  (out_long_w32'(os, Word32.fromInt (!out_position));
	   out_lab(os, main_lab);
	   out_long_w32'(os, Word32.fromInt (List.length map_import_code));
	   out_long_w32'(os, Word32.fromInt (List.length map_import_data));
	   out_long_w32'(os, Word32.fromInt (List.length map_export_code));
	   out_long_w32'(os, Word32.fromInt (List.length map_export_data));
	   out_long_w32'(os, magic);
	   BinIO.output(os, extract(!out_buffer, !out_position));
(*	   print ("Writing code import (address,label)-pairs\n"); *)
	   out_addr_lab_pairs(os, map_import_code);
(*	   print ("Writing data import (address,label)-pairs\n"); *)
	   out_addr_lab_pairs(os, map_import_data);
(*	   print ("Writing code export (label,address)-pairs\n"); *)
	   out_lab_addr_pairs(os, map_export_code);
(*	   print ("Writing data export (label,address)-pairs\n"); *)
	   out_lab_addr_pairs(os, map_export_data);
	   BinIO.closeOut os) handle E => (BinIO.closeOut os; raise E)
	end 
    end
  end
