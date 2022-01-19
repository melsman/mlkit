(* Bit stream module
 * Copyright, Martin Elsman 2003-2011
 * GPL Licence
 *)

structure Bitstream :> BITSTREAM =
  struct
    (* some utilities *)
    fun pairmap0 f (a,b) = (f a,b)
    fun pairmap1 f (a,b) = (a,f b)

    fun wordToWord8 (w: Word.word) : Word8.word =
        Word8.fromLarge(Word.toLarge w)

    fun word8ToWord (w: Word8.word) : Word.word =
        Word.fromInt(Word8.toInt w)

    fun word32ToWord8 (w: Word32.word) : Word8.word =
        Word8.fromLarge(Word32.toLarge w)

    fun word8ToWord32 (w: Word8.word) : Word32.word =
        Word32.fromInt(Word8.toInt w)

    type pos = word

    type instream =
         {pos: pos,               (* bits read *)
          buf: Word8Vector.vector (* entire input *)
         }
    type outstream =
         {pos: pos,               (* bits written *)
          buf: Word8Array.array   (* output *)
         }

    fun isEmpty {pos,buf} =
        pos >= 0w8 * Word.fromInt(Word8Vector.length buf)

    fun double buf =
        let val sz = Word8Array.length buf
            val newbuf = Word8Array.array(sz*2,0w0)
        in Word8Array.copy{src=buf,dst=newbuf,di=0}
         ; newbuf
        end handle Size => raise Fail ("Bitstream.double - sz = " ^ Int.toString (Word8Array.length buf))

    fun array_capacity buf =
        Word.fromInt (8 * Word8Array.length buf)

    fun vector_capacity buf =
        Word.fromInt (8 * Word8Vector.length buf)

    fun ensurespace (s as {pos,buf}) i =
        if pos + i < array_capacity buf then s
        else {pos=pos,buf=double buf}

    fun alignment_properties pos =
        let val pos_byte = Word.>>(pos,0w3)
            val pos_align8 = Word.<<(pos_byte,0w3)
            val bits = pos - pos_align8
        in (bits, Word.toInt pos_byte)
        end

    fun mask n w =
        let val p = wordToWord8 (Word.<<(0w1,n) - 0w1)
        in Word8.andb(w,p)
        end

    fun outwN n (e : Word8.word, s:outstream) : outstream =   (* n \in {1,2,3,4,5,6,7,8} *)
        let val e = mask n e
            val s as {pos,buf} = ensurespace s n
            val (bits, pos_byte) = alignment_properties pos
            val _ =
              if bits = 0w0 then
                Word8Array.update(buf,pos_byte,e)
              else (* 0 < bits < 8 *)
                let
                    (* write least significant bits first *)
                    (* e{0:7-bits} -> buf[pos]{bits:7} *)
                    val w = Word8Array.sub(buf,pos_byte)
                    val e' = Word8.<<(e,bits)  (* e*2^bits *)
                    val w' = Word8.orb(w,e')
                    val _ = Word8Array.update(buf,pos_byte,w')
                in if n <= (0w8 - bits) then () (* no need to use additional byte *)
                   else
                     let
                       (* e{8-bits:7} -> buf[pos+1]{0:bits-1} *)
                       val e'' = Word8.>>(e,0w8-bits)  (* e/2^(8-bits) *)
                       val _ = Word8Array.update(buf,pos_byte+1,e'')
                     in ()
                     end
                end
        in {pos=pos+n,
            buf=buf}
        end

    fun getwN n ({pos,buf}:instream) : Word8.word * instream = (* n \in {1,2,3,4,5,6,7,8} *)
        if pos + n > vector_capacity buf then
          raise Fail "getwN: empty in-stream"
        else
          let val (bits, pos_byte) = alignment_properties pos
              val e =
                  if bits = 0w0 then
                    Word8Vector.sub(buf,pos_byte)
                  else (* 0 < bits < 8 *)
                    let
                      (* read least significant bits first *)
                      (* e{0:7-bits} <- buf[pos]{bits:7} *)
                      val w1 = Word8Vector.sub(buf,pos_byte)
                      val e1 = Word8.>>(w1,bits)    (* w1/2^bits *)
                    in
                      if n <= (0w8 - bits) then e1 (* no need to read additional byte *)
                      else
                        let
                          (* e{8-bits:7} <- buf[pos+1]{0:bits-1} *)
                          val w2 = Word8Vector.sub(buf,pos_byte+1)
                          val e2 = Word8.<<(w2,0w8-bits)  (* w2*2^(8-bits) *)
                        in Word8.orb(e1,e2)
                        end
                    end
              val e = mask n e
          in (e, {pos=pos+n,buf=buf})
          end

    val outw1 = outwN 0w1
    val getw1 = getwN 0w1
    val outw2 = outwN 0w2
    val getw2 = getwN 0w2
    val outb = outw1 o (pairmap0 (fn false => 0w0 | true => 0w1))
    val getb = pairmap0 (fn 0w0 => false | _ => true) o getw1
    val outw8 = outwN 0w8
    val getw8 = getwN 0w8
    val outc = outw8 o (pairmap0 Byte.charToByte)
    val getc = pairmap0 Byte.byteToChar o getw8

    local
      fun outGen extract (w,s) =
	  let val s = outw8(extract 0w0,s)
	      val s = outw8(extract 0w8,s)
	      val s = outw8(extract 0w16,s)
	  in outw8(extract 0w24,s)
	  end
    in
      fun outw (w,s) =
          let fun extract i = wordToWord8(Word.>>(w, i))
	  in outGen extract (w,s)
          end

      fun outw32 (w,s) =
          let fun extract i = word32ToWord8(Word32.>>(w, i))
	  in outGen extract (w,s)
          end
    end

    local
      fun getGen (op << : 'a * Word.word -> 'a) (op +) (fromWord8: Word8.word -> 'a) (s: instream) : 'a * instream =
	let val (w0,s) = getw8 s
            val (w1,s) = getw8 s
            val (w2,s) = getw8 s
            val (w3,s) = getw8 s
            val w0 = fromWord8 w0
	    val w1 = fromWord8 w1
	    val w2 = fromWord8 w2
	    val w3 = fromWord8 w3
	    infix <<
	    val w = w0 + (w1 << 0w8) + (w2 << 0w16) + (w3 << 0w24)
	in (w, s)
	end
    in
      fun getw s =
          getGen Word.<< Word.+ word8ToWord s
      fun getw32 s =
          getGen Word32.<< Word32.+ word8ToWord32 s
    end

    fun outwN' n (w,s) =    (* n <= 0w31 *)
        if n > 0w31 then raise Fail "outwN'"
        else if n = 0w0 then s
        else if n <= 0w7 then
          outwN n (wordToWord8 w,s)
        else let val s = outw8 (wordToWord8 w,s)
                 val w2 = Word.>>(w,0w8)
             in outwN' (n-0w8) (w2,s)
             end

    fun getwN' n s =
        if n > 0w31 then raise Fail "getwN'"
        else let fun loop n off a s =
                     if n = 0w0 then (a,s)
                     else if n <= 0w7 then
                       let val (w8,s) = getwN n s
                       in (Word.orb(Word.<<(word8ToWord w8,off),a),s)
                       end
                     else
                       let val (w8,s) = getw8 s
                           val a = Word.orb(Word.<<(word8ToWord w8,off),a)
                       in loop (n-0w8) (off+0w8) a s
                       end
             in loop n 0w0 0w0 s
             end

    fun outcw (w,s) =
	if w <= 0w254 then outw8(wordToWord8 w,s)
	else let val s = outw8(0w255,s)
	     in outw(w,s)
	     end

    fun getcw s =
	let val (w,s) = getw8 s
	in if w = 0w255 then getw s
	   else (word8ToWord w,s)
	end

    fun outcw2 (w,s) = (* 255*256=65280 *)
	if w <= 0w65279 then
	    let val w1 = wordToWord8 (w div 0w256)
		val w2 = wordToWord8 (w mod 0w256)
	    in outw8(w2,outw8(w1,s))
	    end
	else
	    let val s = outw8(0w255,s)
	    in outw(w,s)
	    end

    fun getcw2 s =
	let val (w,s) = getw8 s
	in if w = 0w255 then getw s
	   else
	       let val (w2,s) = getw8 s
                   val w = word8ToWord w
                   val w2 = word8ToWord w2
	       in (w*0w256 + w2,s)
	       end
	end

    fun outcw32 (w,s) =
	if w <= 0w254 then outw8(word32ToWord8 w,s)
	else let val s = outw8(0w255,s)
	     in outw32(w,s)
	     end

    fun getcw32 s =
	let val (w,s) = getw8 s
	in if w = 0w255 then getw32 s
	   else (word8ToWord32 w,s)
	end

    fun toBytes {pos,buf} =
        let val pos_byte = Word.toInt(Word.>>(pos,0w3))
            val slice = Word8ArraySlice.slice(buf,0,SOME (pos_byte + 1))
        in Word8ArraySlice.vector slice
        end

    fun toString os = Byte.bytesToString(toBytes os)

    fun openOut () = {pos=0w0,buf=Word8Array.array(1024,0w0)}

    fun openInBytes buf = {pos=0w0,buf=buf}

    fun openIn s = openInBytes(Byte.stringToBytes s)

  end
