signature SCS_DICT =
  sig
    (* The SMLserver Dictionary is a mechanism to produce 
       multilingual web-sites. First of all it defines the
       set of languages supported (e.g., Danish and English
       in ScsLang.sml).
       
       You can tell the dictionary how to obtain personal
       language preferences for the user logged into the
       web-site. 

       The dictionary is stored in the database (file 
       ScsDict.sql). If the database is not accessible,
       or the user has no preferences (e.g., may not be
       logged in), then a default language is choosen. 

       The language support is divided in small texts and 
       larger texts. For larger texts, you will probably 
       include all the language versions in the 
       script-files. You can get the language 
       preference, and then use a case expression to
       choose the right text. If you use the language
       datatype then you get warnings on all files
       where a language is not supported; unless you use
       the wild card pattern _.

       For small texts, you can use the cached dictionary
       functionality. You pass a text in the source language
       and the sentence is translated into the
       language preferred (e.g., Danish). If the English
       phrase is not in the cache, then it is looked up
       in the database. If it is not found in the database,
       then the missing phrase is logged. It is then up to 
       the maintainer to include the phrase in the database.

       The Design Requirements:

         * Phrases from the database must be cached.
         * It should work even though the database is
           offline or that the phrase simply does not
	   exist in the preferred language - the 
	   original language is then used.
	 * The functions used to access the dictionary 
	   must be small - we choose the letter d that
	   works on strings and d' that works on
	   quotations

       Canonical Representation: All source texts are 
       stored with words separated by one space; new 
       lines etc. has been removed.
     *)
    val d  : ScsLang.lang -> string -> string
    val d' : ScsLang.lang -> quot -> quot

     (* [d1 source_lang phrase arg] translates the phrase written in
        source_lang into the language preferred by the logged in
        user. Then replaces any %1 pattern surrounded with white space
        with arg. *)
    val d1 : ScsLang.lang -> string -> string -> string
    val d1': ScsLang.lang -> quot -> string -> quot

    val d2 : ScsLang.lang -> string -> string -> string -> string
    val d2': ScsLang.lang -> quot -> string -> string -> quot

    val d3 : ScsLang.lang -> string -> string -> string -> string -> string
    val d3': ScsLang.lang -> quot -> string -> string -> string -> quot

    val dl : ScsLang.lang -> string -> string list -> string
    val dl': ScsLang.lang -> quot -> string list -> quot

    val cacheName  : ScsLang.lang * ScsLang.lang -> string
    val cacheFlush : ScsLang.lang * ScsLang.lang -> unit
  end

structure ScsDict :> SCS_DICT =
  struct 
    fun canonical s = String.concatWith " " (String.tokens Char.isSpace s)

 fun subst (s,arg,pattern) =
      String.concatWith " " (List.map (fn frag => if frag = pattern then arg else frag) (String.tokens Char.isSpace s))

      fun foldVec (f: (int * 'a) -> int * 'a) (b: 'a) (v: CharVector.vector) : 'a =
	let
	  fun foldVec' (i,acc) =
	    if i < CharVector.length v then
	      foldVec' (f(i,acc))
	    else
	      (i,acc)
	in
	  #2(foldVec' (0,b))
	end

      fun subst' (v: CharVector.vector) (m:(int*string) Array.array) =
	let
	  fun lookup a i = 
	    if Char.isDigit i then 
	      let val i = Option.valOf (Int.fromString (Char.toString i)) 
	      in
		Array.sub(a,i) handle _ => (2,Int.toString i)
	      end
	    else (2,Char.toString i)

	  fun len (i:int,l:int) =
	    case CharVector.sub(v,i) of
	      (#"%") => 
		(if i+1 < CharVector.length v then
		   case CharVector.sub(v,i+1) of
		     (#"%") => (i+2,l+1)
		   | x => (i+2,l + #1(lookup m x))
		 else (i+1,l+1))
	    | _ => (i+1,l+1)

	  fun sub (s:CharArray.array) (v_i:int,(s_i:int)) =
	    case CharVector.sub(v,v_i) of
	      (#"%") => 
		(if v_i+1 < CharVector.length v then
		   case CharVector.sub(v,v_i+1) of
		     (#"%") => (CharArray.update(s,s_i,#"%");(v_i+2,s_i+1))
		   | x => (CharArray.copyVec{src= #2 (lookup m x),si=0,len=NONE,dst=s,di=s_i};
			   (v_i+2,s_i + #1(lookup m x)))
		 else (CharArray.update(s,s_i,#"%");(v_i+1,s_i+1)))
	    | x => (CharArray.update(s,s_i,x);(v_i+1,s_i+1))

	  val s = CharArray.array ((foldVec len 0 v), #" ")
	  val _ = foldVec (sub s) 0 v
	in
	  CharArray.extract(s,0,NONE)
	end
      
    (* Looks up the source string s in the db. Returns the original
       string s, if no one is found in the db and also inserts the
       missing row in the db (i.e., a query into the db can show which
       texts needs to be translated.) *)
    fun lookup target_lang source_lang source_text =
      if target_lang = source_lang then source_text 
      else
	case Db.zeroOrOneField `
          select t.phrase from scs_dict_source s, scs_dict_target t
           where s.lang='^(ScsLang.toString source_lang)' 
             and s.phrase = '^(Db.qq (canonical source_text))'
             and s.phrase_id = t.phrase_id 
             and t.lang = '^(ScsLang.toString ScsLogin.user_lang)'` of
	       SOME target_phrase => target_phrase
	     | NONE => ((* Make sure that it is defined in the source table *)
			Db.maybeDml `insert into scs_dict_source (phrase_id,lang,phrase,script,entry_date)
			values (^(Db.seqNextvalExp "scs_dict_phrase_seq"),
				'^(ScsLang.toString source_lang)', '^(canonical source_text)',
				'^(Ns.Conn.url())',sysdate)`;
			source_text)

    fun cacheName (source_lang,target_lang) = "scs_dict"^ScsLang.toString source_lang ^ ScsLang.toString target_lang
    fun cacheFlush (source_lang,target_lang) = 
      case Ns.Cache.find (cacheName (source_lang,target_lang)) of
	NONE => ()
      | SOME c => Ns.Cache.flush c

    val d = fn source_lang => 
      Ns.Cache.cacheWhileUsed 
        (lookup ScsLogin.user_lang source_lang,cacheName(source_lang,ScsLogin.user_lang),7200)

    val d' = fn source_lang => Quot.fromString o (d source_lang) o Quot.toString

    val dl = fn source_lang =>
      let
	val dict = d source_lang
      in
	fn source => fn args => subst' (dict source) (Array.fromList (List.map (fn s => (String.size s,s)) args))
      end
    val d1 = fn source_lang =>
      let
	val dict = d source_lang
      in
	fn source => fn arg => subst' (dict source) (Array.fromList [(String.size arg,arg)])
      end
    val d2 = fn source_lang =>
      let
	val dict = d source_lang
      in
	fn source => fn arg1 => fn arg2 => subst' (dict source) (Array.fromList[(String.size arg1,arg1),
										(String.size arg2,arg2)])
      end
    val d3 = fn source_lang =>
      let
	val dict = d source_lang
      in
	fn source => fn arg1 => fn arg2 => fn arg3 => subst' (dict source) (Array.fromList[(String.size arg1,arg1),
											   (String.size arg2,arg2),
											   (String.size arg3,arg3)])
      end

    val dl' = fn source_lang =>
      let
	val dict = d source_lang
      in
	fn source => fn args => Quot.fromString (subst' (dict (Quot.toString source))
						(Array.fromList (List.map (fn s => (String.size s,s)) args)))
      end
    val d1' = fn source_lang =>
      let
	val dict = d source_lang
      in
	fn source => fn arg => Quot.fromString (subst' (dict (Quot.toString source))
						(Array.fromList [(String.size arg,arg)]))
      end
    val d2' = fn source_lang =>
      let
	val dict = d source_lang
      in
	fn source => fn arg1 => fn arg2 => Quot.fromString (subst' (dict (Quot.toString source))
							    (Array.fromList [(String.size arg1,arg1),
									     (String.size arg2,arg2)]))
      end
    val d3' = fn source_lang =>
      let
	val dict = d source_lang
      in
	fn source => fn arg1 => fn arg2 => fn arg3 => Quot.fromString (subst' (dict (Quot.toString source))
								       (Array.fromList [(String.size arg1,arg1),
											(String.size arg2,arg2),
											(String.size arg3,arg3)]))
      end
  end

