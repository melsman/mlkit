
                        (******************************************)
                        (*                  Menu                  *)
                        (*                                        *)
                        (* Defines an abstract menu type and      *)
                        (* operations for building and interacting*)
                        (* with menus.                            *)
                        (******************************************)

signature MENU =
  sig               (*key*)  (*path*)
    type 'a entry = string * string list * 'a ref

    val add_flag_to_menu                : bool entry -> unit
    val add_string_to_menu              : string entry -> unit
    val add_int_to_menu                 : int entry -> unit
    val add_int_list_to_menu            : int list entry -> unit
    val add_int_pair_list_to_menu       : (int*int) list entry -> unit

    val add_action_to_menu              : string * string list * (unit->unit) -> unit
    val add_action_read_to_menu         : string * string list * (unit->unit) * string ref -> unit
    val add_action_with_value_to_menu   : string * string list * (unit->unit) * (unit->string) -> unit
    val add_bool_action_to_menu         : string * string list * (bool->unit) * (unit->bool) -> unit

    val show_full_menu                  : unit -> unit
    val interact: unit -> unit
  end   

functor Menu(structure Crash : CRASH
	     val help_topic : string -> string) : MENU =
struct

  type 'a entry = string * string list * 'a ref

  fun die s = Crash.impossible ("Menu." ^ s)
  fun outLine (s) = print(s ^ "\n")
  fun quote s = "\"" ^ String.toString s ^ "\""

  datatype attribute = SWITCH of bool ref | VALUE of unit -> string

  datatype menu = 
    DISPLAY of item list
  | ACTION of unit -> unit     (* show >>> in entry *)
  | ACTION0 of unit -> unit    (* don't show >>> in entry *)
  | NOMENU

  withtype item = {key: string, text: string, attr: attribute, below: menu} 
  
  
  fun empty NOMENU = true
    | empty _ = false;

  fun max(i:int, j: int) = if i>=j then i else j
  fun pad width text = StringCvt.padRight #"." width text;
  fun help () = outLine "\n\n***Try again\n"
  
  val menu = ref(DISPLAY[]) ; (* updated later*)
    
  fun mk_toggle key (b: bool ref) txt = {key=key, text=txt, attr=SWITCH b, below=NOMENU}

  fun mk_header text below : item = {key="", text=text,attr=VALUE(fn _ => ""),below=below}

    (* -----------------------------------
     *         Parse functions 
     * ----------------------------------- *)
    
  type ('a, 'b) reader = ('a, 'b) StringCvt.reader


    fun scan_comma (getc : (char,'cs) reader) (cs:'cs) : 'cs option =
      case getc (StringCvt.skipWS getc cs)
	of SOME(#",",cs) => SOME cs
	 | _ => NONE

    fun scan_endlist (getc : (char,'cs)reader) (cs:'cs) (acc : 'a list) : ('a list * 'cs)option =
      let val cs = StringCvt.skipWS getc cs
      in case getc cs
	   of SOME(#"]",cs) => SOME (rev acc,cs)
	    | _ => NONE
      end

    fun scan_list scan_elem getc cs =
      let val cs = StringCvt.skipWS getc cs
	  fun loop(cs,acc) =
	    case scan_comma getc cs
	      of SOME cs => 
		(case scan_elem getc cs
		   of SOME(e,cs) => loop(cs,e::acc)
		    | NONE => NONE)
	       | NONE => scan_endlist getc cs acc
      in case getc cs
	   of SOME(#"[",cs) => 
	     (case scan_elem getc cs
		of SOME(e,cs) => loop(cs, [e])
		 | NONE => scan_endlist getc cs [])
	    | _ => NONE
      end

  fun scan_string getc cs =
    let fun loop (cs, acc) =
      case getc cs
	of SOME(#"\"", cs) => SOME(implode(rev acc),cs)
         | SOME(c,cs) => loop(cs,c::acc)
	 | NONE => NONE
    in case getc (StringCvt.skipWS getc cs)
	 of SOME(#"\"", cs) => loop(cs, [])
	 | _ => NONE
    end

    fun scan_pair scan1 scan2 getc cs =
      case getc (StringCvt.skipWS getc cs)
	of SOME(#"(",cs) => 
	  (case scan1 getc cs
	     of SOME(e1,cs) =>
	       (case getc (StringCvt.skipWS getc cs)
		  of SOME(#",",cs) => 
		    (case scan2 getc (StringCvt.skipWS getc cs)
		       of SOME(e2,cs) => 
			 (case getc (StringCvt.skipWS getc cs)
			    of SOME(#")",cs) => SOME((e1,e2),cs)
			     | _ => NONE)
			| NONE => NONE)
		   | _ => NONE)
	      | NONE => NONE)
	 | _ => NONE


  fun getc [] = NONE
    | getc (c::cs) = SOME(c,cs)


  (*read_string r () = read a string from TextIO.stdIn.  If the input is some
   string in quotes, read_string assigns it to the ref r (and sets the global
   (whooa!) ref u_or_q_from_read_string is to false), and returns.  If the
   input is `u' or `quit', read_string does not update r, sets
   u_or_q_from_read_string to false, and returns.  For other inputs
   read_string gives an error message and lets the user try again.*)

  val u_or_q_from_read_string = ref false
  fun read_string r () =
      (u_or_q_from_read_string := false ;
       outLine "<string in double quotes>, Up (u), or Quit (quit): >" ;
       let val cs = explode(TextIO.inputLine TextIO.stdIn)
	   val cs = StringCvt.skipWS getc cs
       in
	 case cs of 
	   [] => (help () ; read_string r ())
	 | #"q" :: #"u" :: #"i" :: #"t" :: _  => u_or_q_from_read_string := true
	 | #"u" :: _  => u_or_q_from_read_string := true
	 | #"\"" (*"*) :: _  => 
           (case scan_string getc cs
	      of SOME(s,_) => r := s 
	       | NONE => (help () ; read_string r ()))
	 | _ => (help () ; read_string r ())
       end)

  fun read_int r () =
    (outLine "<number> or up (u): >";
     let val cs = explode(TextIO.inputLine TextIO.stdIn)
         val cs = StringCvt.skipWS getc cs
     in case cs 
	  of [] => (help(); read_int r ())
	   | #"q" :: #"u" :: #"i" :: #"t" :: _  => ()
	   | #"u" :: _  => ()
	   | _ => (case Int.scan StringCvt.DEC getc cs 
		     of SOME(i,_) => r:= i 
		      | _ => (help(); read_int r ()))
     end)

  fun mk_string_action key (r: string ref) txt =
    {key=key, text = txt, attr = VALUE (fn () => "(" ^ quote(!r) ^ ")"),
     below = ACTION (read_string r)};

  fun mk_int_action key (r: int ref) txt =
    {key=key, text = txt, attr = VALUE(fn _ => Int.toString(!r)),
     below = ACTION (read_int r)};

  fun add_path(path : string list, mk: string -> item, menu : menu) : menu =
    case path 
      of [] => menu (* don't add anything! *)
       | [txt] => (case menu 
		     of DISPLAY l=> DISPLAY(l @ [mk txt])
		      | NOMENU => DISPLAY[mk txt]
		      | _ => die "add_path.menu not DISPLAY or NOMENU.")
       | (txt::path') =>
	let fun extend [] : item list = [mk_header txt (add_path(path',mk,NOMENU))] (* insert new header *)
	      | extend ((item as {key,text,attr,below})::items) =
	       if text=txt then {key=key,text=text,attr=attr,below=add_path(path',mk,below)} :: items
	       else item :: extend items 
	in case menu 
	     of DISPLAY l => DISPLAY (extend l)
	      | NOMENU => DISPLAY (extend [])
	      | _ => die "add_path.menu not DISPLAY, or NOMENU." 
	end

    fun add_to_menu (mk: string -> 'a ref -> string -> item) 
      (key: string, path : string list, a : 'a ref) : unit =
      menu:= add_path(path,mk key a,!menu)
      
    val add_flag_to_menu = add_to_menu mk_toggle
    val add_string_to_menu = add_to_menu mk_string_action
    val add_int_to_menu = add_to_menu mk_int_action

    val noop_attr : attribute = VALUE (fn () => "")

    fun add_action_to_menu (key, path : string list, f : unit -> unit) : unit =
      let fun mk_action txt = {key=key, text = txt, attr = noop_attr, below = ACTION f}
      in menu:= add_path(path,mk_action,!menu)
      end

    fun add_action_with_value_to_menu (key, path : string list, f:unit->unit, v:unit->string) : unit =
      let fun mk_action_with_value txt = {key=key, text = txt, attr = VALUE v, below = ACTION f}
      in menu:= add_path(path,mk_action_with_value,!menu)
      end

    fun add_action_read_to_menu (key, path : string list, f:unit->unit, r:string ref) : unit =
      let fun mk_action_read txt = {key=key, text=txt, attr=noop_attr,
				    below= ACTION (fn _ => (read_string r ();
							    if !u_or_q_from_read_string then () 
							    else f ()))}
      in menu:= add_path(path,mk_action_read,!menu)
      end

    fun add_bool_action_to_menu (key, path : string list, f:bool->unit, r:unit->bool) : unit =
      let fun show true = "on"
	    | show false = "off"
	  fun mk_action txt = {key=key, text = txt, attr = VALUE (show o r), below = ACTION0 (f o r)}
      in menu:= add_path(path,mk_action,!menu)
      end


                    (******************)
                    (* Printing menus *)
                    (******************)
 
  fun display_lines(lines:  {key: string,
			     text: string,
                             attr: attribute,
                             below: menu}list) : unit = 
    let 
           val width = foldl
                       (fn ({text, ...}, acc:int) => max(size text, acc))
                       0
                       (lines)
  
           fun outText (n,text, s: string) =
	         outLine("\t" ^ Int.toString n ^ "\t" ^ pad width text ^ s)
           
           fun display_line (n, {key,text, attr, below}) =
             case (attr, below) of
               (SWITCH(ref true), NOMENU)     => outText(n, text, " on")
             | (SWITCH(ref true), _     )     => outText(n, text, " on >>>")
             | (SWITCH(ref false), NOMENU )   => outText(n, text, " off")
             | (SWITCH(ref false), _ )        => outText(n, text, " off (>>>)")
             | (VALUE show, NOMENU)           => outText(n, text, " " ^ show())
             | (VALUE show, ACTION0 _)        => outText(n, text, " " ^ show())
             | (VALUE show, _ )               => outText(n, text, " " ^ show() ^ " >>>")
  
           fun loop(n,[]) =()
           |   loop(n,line::rest) = (display_line(n, line); loop(n+1, rest))
                   
     in
       loop(0,lines);
       outLine "\nSelect (<number>), Help (h <number>), Up (u), or Quit (quit): "
    end;
  
  fun blanks n =
    let 
      fun loop  n = if n<= 0 then [] else  " " :: loop(n-1)
    in
      concat(loop n)
    end;
  
  fun show_menu menu = case menu of
    DISPLAY l => display_lines l
  | _ => ();
  
  
               (************************************************)
               (* Printing an overview of the entire menu tree *)
               (************************************************)

  (* this code uses leading newlines whereas normal
     printing of menus uses trailing newlines in output *)
  
  val delta = 4
  fun outLineInd indent s =
     (outLine ""; (*start new line*)
      print(blanks indent ^ s)
     )
  fun show_full_menu  indent menu = case menu of
    DISPLAY l => List.app (show_full_item indent) l  
  | ACTION _ => print " fn"
  | ACTION0 _ => print " fn"
  | NOMENU => ()
  and show_full_item indent {key:string, text: string, attr = VALUE show, below} =
        (outLineInd indent (text ^ " " ^ show());
         show_full_menu(indent+delta)below)
    | show_full_item indent {key, text, attr = SWITCH _, below} =
        (outLineInd indent (text ^ "(switch)");
         show_full_menu(indent+delta)below);
  
  val show_full_menu = (fn () => 
       (outLine "\n----------------------------------------------------\n";
        show_full_menu 5 (!menu); 
        outLine "\n----------------------------------------------------\n"))
  
                 (*************************************************)
                 (* Menu commands (entered by user interactively) *)
                 (*************************************************)
  
  datatype cmd =   SELECTION of int
                 | UP 
                 | QUIT 
                 | HELP_TOPIC of int
                 | HELP;
  
  fun read_display_cmd(): cmd =
    (print "\n>";
     let val cs = explode(TextIO.inputLine TextIO.stdIn)
         val cs = StringCvt.skipWS getc cs
     in case cs of
          [] => HELP
        | #"q" :: #"u" :: #"i" :: #"t" :: _ => QUIT
        | #"u" :: _ => UP
        | #"h" :: cs => (case Int.scan StringCvt.DEC getc cs 
			   of SOME(i, _) => HELP_TOPIC i
			    | _ => HELP)
        | _ =>  (case Int.scan StringCvt.DEC getc cs
		   of SOME(n,_) => SELECTION n
		    | _ => HELP)
     end);
    
                         (**********************************************)
                         (* Auxiliary functions for constructing menus *)
                         (**********************************************)
  
  fun read_int_list r () =
    (outLine "<type an int list, e.g. [4,3]> or up (u): >";
     let
       val s = TextIO.inputLine TextIO.stdIn
       val cs = explode s
     in
       case cs
	 of [] => (help(); read_int_list r ())
	  | #"u" :: _  => ()
	  | #"q" :: #"u" :: #"i" :: #"t" :: _  => ()
	  | #"["::_ => (case scan_list (Int.scan StringCvt.DEC) getc cs
			  of SOME(l',_) => (r := l')
			   | _ => (help(); read_int_list r ()))
	  | _ => (help(); read_int_list r ())
     end)

  fun read_int_pair_list r () =
    (outLine "<type an int pair list of region variables,\n\
	  \e.g. [(formal reg. var. at pp.,letregion bound reg. var.)]> or up (u): >" ;
     let
       val s = TextIO.inputLine TextIO.stdIn
       val cs = explode s
     in
       case cs
	 of [] => (help(); read_int_pair_list r ())
	  | #"u" :: _  => ()
	  | #"q" :: #"u" :: #"i" :: #"t" :: _  => ()
	  | #"["::_ => (case scan_list (scan_pair (Int.scan StringCvt.DEC) (Int.scan StringCvt.DEC)) getc cs
			  of SOME(l',_) => (r := l')
			   | NONE => (help(); read_int_pair_list r ()))
	  | _ => (help(); read_int_pair_list r ())
     end)

    fun mk_int_list_action key (r: int list ref) text =
      {key=key,
       text = text, 
       attr = VALUE 
       (fn _ => (Edlib.List.string 
		 (fn i => (Int.toString i))
		 (!r))),
       below = ACTION (read_int_list r)};
      
    fun mk_int_pair_list_action key (r: (int*int) list ref) text =
      {key=key,
       text = text, 
       attr = VALUE 
       (fn _ => (Edlib.List.string 
		 (fn (i1, i2) => 
		  "(" ^ Int.toString i1 ^ "," ^ Int.toString i2 ^ ")")
		 (!r))),
       below = ACTION (read_int_pair_list r)};

    val add_int_list_to_menu = add_to_menu mk_int_list_action
    val add_int_pair_list_to_menu = add_to_menu mk_int_pair_list_action

                         (**************************)
                         (* interact               *)
                         (**************************)

  exception Quit; 

  fun interact () = interact0 ()
        handle Quit => ()
	     | Crash.CRASH => (outLine "*** CRASH raised *" ; interact ()) 
	     | e as IO.Io _ => (outLine ("*** IO.Io raised: " ^ exnMessage e); (*interact ()*) raise e)
	     | Overflow => (outLine "*** Overflow raised *"; interact ())
	     | e => (outLine ("*** Uncaught exception " ^ General.exnName e ^ " ***");
		     outLine ("Exn message: " ^ General.exnMessage e);
		     outLine "I shall reraise it...";
		     raise e)

  and interact0 () = inter ([], !menu)
  
                        (*************************************************)
                        (* inter(path,menu):                             *)
                        (*                                               *)
                        (*    the read-eval loop of the interaction      *)
                        (*    'path' is the path from the root to 'menu' *)
                        (*    and is used for printing                   *)
                        (*************************************************)

  and inter (path, menu : menu) : unit = 
    (case menu of
       DISPLAY l => 
        (show_path path;
	 outLine "";
         show_menu menu; 
         case read_display_cmd () : cmd 
	   of SELECTION n =>
	      ((case List.nth (l,n) of 
		  {attr = SWITCH(r as ref false), ...} =>
		    (* assume toggle *) 
		    (r := not (!r) ; inter (path, menu))
		| {attr = SWITCH(r as ref true), below = NOMENU, ...} =>
		    (* assume toggle *) 
		    (r := not (!r) ; inter (path, menu))
		| {attr = VALUE _ , below, text,...} => 
		    (* assume activate *)
		    (inter (text::path, below) ; inter(path, menu))
		| _ => (help () ; inter (path, menu))
		    ) handle Subscript =>
		               (outLine "\n***Number out of range - try again" ;
				inter (path, menu)))
	    | HELP_TOPIC n => 
		  (let val {key,...} = List.nth(l,n)
		   in if key = "" then (outLine "\n***No help available for this item - try again";
					inter(path, menu))
		      else 
			(print"\n"; print (help_topic key);
			 print"\n"; inter(path,menu))
		   end handle Subscript =>
		               (outLine "\n***Number out of range - try again";
				inter (path, menu)))
	    | UP => (case path of (*ignore u when at top-level menu*)
		       [] => inter (path,menu)
		     | _ => ())
	    | QUIT => raise Quit
	    | _ => (help () ; inter (path,menu)))
     | ACTION f => f ()
     | ACTION0 f => f ()
     | NOMENU => ())

  and show_path nil = ()
    | show_path (l : string list) = 
        outLine(Edlib.List.stringSep "\t" "" "/" (fn s => s) (rev l));

end (*functor Menu*)
