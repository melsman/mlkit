(*$StringType : STRING_TYPE *)

structure StringType: STRING_TYPE =

(* CHARACTER CLASSES

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        4 Oct 1989

Maintenance:	Author

DESCRIPTION

   Functions to find the types of strings.


RCS LOG

$Log$
Revision 1.1  1998/01/22 17:01:37  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.5  91/02/11  20:54:08  20:54:08  db (Dave Berry)
Moved names of ASCII control characters to Ascii.sml, as part of the
major reorganisation of the library.

Revision 1.4  91/01/28  13:33:00  13:33:00  db (Dave Berry)
Changed implementation to use CoreVector instead of Vector.

Revision 1.3  91/01/25  20:21:39  20:21:39  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.2  91/01/24  17:27:56  17:27:56  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  15:52:32  15:52:32  db (Dave Berry)
Initial revision


*)

struct

  open OldString

(* CONSTANTS *)

  val digits = "1234567890"
  val hexes = "1234567890abcdefABCDEF"
  val formats = "\^H\^I\^J\^L\^M "
  val uppers = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val lowers = "abcdefghijklmnopqrstuvwxyz"
  val letters = uppers ^ lowers
  val alNums = letters ^ digits
  val ids = alNums ^ "_'"
  val symbols = "!%&$#+-/:<=>?@\\~`^|*"
  val puncts = symbols ^ "'_(),.;[]{}"
  val visibles = letters ^ digits ^ puncts
  val prints = visibles ^ formats
  val controls =
        "\000\001\002\003\004\005\006\007\008\009\010\011\012\013\014\015\
        \\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031\127"
  val asciis = controls ^ visibles


(* OBSERVERS *)

  local
    datatype Types =
        D  (* Digit *)
      | U  (* Upper-case letter *)
      | L  (* Lower-case letter *)
      | UX (* Upper-case heX digit letter *)
      | LX (* Lower-case heX digit letter *)
      | F  (* Formatting control character *)
      | C  (* non-formatting Control char *)
      | SP (* SPace *)
      | S  (* sml Symbol *)
      | I  (* prime or underbar (used in sml alphanumeric Identifiers) *)
      | P  (* Punctuation character that can't appear in sml identifers *)
      | E  (* Error (non-Ascii char) *)

    val types = CoreVector.vector [

(*  nul soh stx etx eot enq ack bel  bs  ht  nl  vt  np  cr  so  si *)
     C,  C,  C,  C,  C,  C,  C,  C,  F,  F,  F,  C,  F,  F,  C,  C,

(*  dle dc1 dc2 dc3 dc4 nak syn etb  can em  sub esc fs  gs  rs  us *)
     C,  C,  C,  C,  C,  C,  C,  C,  C,  C,  C,  C,  C,  C,  C,  C,

(*  sp   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /  *)
     SP, S,  P,  S,  S,  S,  S,  I,  P,  P,  S,  S,  P,  S,  P,  S,

(*   0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?  *)
     D,  D,  D,  D,  D,  D,  D,  D,  D,  D,  S,  P,  S,  S,  S,  S,

(*   @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O  *)
     S,  UX, UX, UX, UX, UX, UX, U,  U,  U,  U,  U,  U,  U,  U,  U,

(*   P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _  *)
     U,  U,  U,  U,  U,  U,  U,  U,  U,  U,  U,  P,  S,  P,  S,  I,

(*   `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o  *)
     S,  LX, LX, LX, LX, LX, LX, L,  L,  L,  L,  L,  L,  L,  L,  L,

(*   p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~  del *)
     L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  L,  P,  S,  P,  S,  C,

     E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,
     E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,
     E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,
     E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,
     E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,
     E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,
     E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,
     E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E,  E  ]
    val sub = CoreVector.sub
    infix 9 sub
  in
    exception Empty of string
    fun isDigit ""   = raise Empty "isDigit"
    |   isDigit s    = types sub ord s = D
    fun isHex ""     = raise Empty "isHex"
    |   isHex s      = case types sub ord s of
                         D  => true
                       | LX => true
                       | UX => true
                       | _  => false
    fun isFormat ""  = raise Empty "isFormat"
    |   isFormat s   = case types sub ord s of
                         F  => true
                       | SP => true
                       | _  => false
    fun isPrint ""   = raise Empty "isPrint"
    |   isPrint s    = case types sub ord s of
                         C  => false
                       | E  => false
                       | _  => true
    fun isVisible "" = raise Empty "isVisible"
    |   isVisible s  = case types sub ord s of
                         C  => false
                       | F  => false
                       | SP => false
                       | E  => false
                       | _  => true
    fun isLetter ""  = raise Empty "isLetter"
    |   isLetter s   = case types sub ord s of
                         U  => true
                       | L  => true
                       | UX => true
                       | LX => true
                       | _  => false
    fun isUpper ""   = raise Empty "isUpper"
    |   isUpper s    = case types sub ord s of
                         U  => true
                       | UX => true
                       | _  => false
    fun isLower ""   = raise Empty "isLower"
    |   isLower s    = case types sub ord s of
                         L  => true
                       | LX => true
                       | _  => false
    fun isPunct ""   = raise Empty "isPunct"
    |   isPunct s    = case types sub ord s of
                         P  => true
                       | I  => true
                       | S  => true
                       | _  => false
    fun isControl "" = raise Empty "isControl"
    |   isControl s  = case types sub ord s of
                         F  => true
                       | C  => true
                       | _  => false
    fun isAlNum ""   = raise Empty "isAlNum"
    |   isAlNum s    = case types sub ord s of
                         U  => true
                       | L  => true
                       | UX => true
                       | LX => true
                       | D  => true
                       | _  => false
    fun isId ""      = raise Empty "isId"
    |   isId s       = case types sub ord s of
                         U  => true
                       | L  => true
                       | UX => true
                       | LX => true
                       | D  => true
                       | I  => true
                       | _  => false
    fun isSymbol ""  = raise Empty "isSymbol"
    |   isSymbol s   = types sub ord s = S
    fun isAscii ""   = raise Empty "isAscii"
    |   isAscii s    = ord s < 128
  end

end
