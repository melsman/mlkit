(* Bit stream module
 * Copyright, Martin Elsman 2003-2011
 * GPL Licence
 *)

signature BITSTREAM =
  sig
    type instream
    type outstream

    val isEmpty   : instream -> bool

    val outwN     : word -> Word8.word * outstream -> outstream
    val getwN     : word -> instream -> Word8.word * instream

    val outwN'    : word -> word * outstream -> outstream
    val getwN'    : word -> instream -> word * instream

    val outw1     : Word8.word * outstream -> outstream
    val getw1     : instream -> Word8.word * instream

    val outb      : bool * outstream -> outstream
    val getb      : instream -> bool * instream

    val outw2     : Word8.word * outstream -> outstream
    val getw2     : instream -> Word8.word * instream

    val outw8     : Word8.word * outstream -> outstream
    val getw8     : instream -> Word8.word * instream

    val outc      : char * outstream -> outstream
    val getc      : instream -> char * instream

    val outw      : word * outstream -> outstream
    val getw      : instream -> word * instream

    val outcw     : word * outstream -> outstream
    val getcw     : instream -> word * instream

    val outcw2    : word * outstream -> outstream
    val getcw2    : instream -> word * instream

    val outw32    : Word32.word * outstream -> outstream
    val getw32    : instream -> Word32.word * instream

    val outcw32   : Word32.word * outstream -> outstream
    val getcw32   : instream -> Word32.word * instream

    val toString  : outstream -> string
    val openOut   : unit -> outstream
    val openIn    : string -> instream
  end

(* [outw1(w,os)] output the one bit w&0w1 to the stream os.

   [getw1(w,o)] read one bit from stream is.
*)
