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
       scs-dict-create.sql). If the database is not accessible,
       or the user has no preferences (e.g., may not be
       logged in), then a default language is chosen. 

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

     (* [d source_lang module file_name phrase] translates the
        phrase written in source_lang into the language preferred by
        the logged in user. *)
    val d  : ScsLang.lang -> string -> string -> string -> string
    val d' : ScsLang.lang -> string -> string -> quot -> quot

     (* [d1 source_lang module file_name arg phrase] translates the
        phrase written in source_lang into the language preferred by
        the logged in user. Then replaces any %0 pattern surrounded
        with white space with arg. It uses the translations recorded
        for the module/file_name only. *)
    val d1 : ScsLang.lang -> string -> string -> string -> string -> string
    val d1': ScsLang.lang -> string -> string -> string -> quot -> quot

     (* [d2 source_lang module file_name arg0 arg1 phrase] translates
        the phrase written in source_lang into the language preferred
        by the logged in user. Then replaces any %0 pattern surrounded
        with white space with arg0 and %1 pattern with arg1. It uses
        the translations recorded for the module/file_name only. *)
    val d2 : ScsLang.lang -> string -> string -> string -> string -> string -> string
    val d2': ScsLang.lang -> string -> string -> string -> string -> quot -> quot

     (* [d3 source_lang module file_name arg0 arg1 arg2 phrase] translates
        the phrase written in source_lang into the language preferred
        by the logged in user. Then replaces any %0,%1,%2 pattern surrounded
        with white space with arg0,arg1,arg2. It uses
        the translations recorded for the module/file_name only. *)
    val d3 : ScsLang.lang -> string -> string -> string -> string -> string -> string -> string
    val d3': ScsLang.lang -> string -> string -> string -> string -> string -> quot -> quot

     (* [dl source_lang module file_name args phrase] translates the
        phrase written in source_lang into the language preferred by
        the logged in user. Then replaces any %n pattern surrounded
        with white space with the n'th arg in the list args. It uses the 
	translations recorded for the module/file_name only. *)
    val dl : ScsLang.lang -> string -> string -> string list -> string -> string
    val dl': ScsLang.lang -> string -> string -> string list -> quot -> quot

    val cacheName  : ScsLang.lang * ScsLang.lang -> string
    val cacheFlush : ScsLang.lang * ScsLang.lang -> unit
  end

structure ScsDict :> SCS_DICT =
  struct 
    fun canonical s = String.concatWith " " (String.tokens Char.isSpace s)

    fun subst (s,arg,pattern) =
      String.concatWith " " 
        (List.map (fn frag => if frag = pattern then arg else frag) 
	 (String.tokens Char.isSpace s))

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
       texts needs to be translated.) 
       The database is updated with the date for the last access.*)
    fun lookup source_lang module file_name canonical_source_phrase module_file_phrase target_lang =
      if target_lang = source_lang then canonical_source_phrase 
      else
	case Db.zeroOrOneField `
	  select t.phrase 
            from scs_dict_sources s, scs_dict_targets t
           where s.lang='^(ScsLang.toString source_lang)' 
             and s.module_file_phrase = ^(Db.qqq module_file_phrase)
             and s.phrase_id = t.phrase_id 
             and t.lang = '^(ScsLang.toString ScsLogin.user_lang)'` of
	       SOME target_phrase => (Db.dml `update scs_dict_sources
                                                 set last_read_date = sysdate
                                               where module_file_phrase = ^(Db.qqq module_file_phrase)
                                                 and lang = '^(ScsLang.toString source_lang)'`;
                                      target_phrase)
	     | NONE => ((* Make sure that it is defined in the source table 
                           Important to use maybeDml as it may fail if we are
                           translating to more than one language. *)
			Db.maybeDml `insert into scs_dict_sources 
                                       (phrase_id,lang,phrase,module,file_name,module_file_phrase,create_date)
                                     values (scs.new_obj_id,'^(ScsLang.toString source_lang)', 
					     ^(Db.qqq canonical_source_phrase),^(Db.qqq module),
                                             ^(Db.qqq file_name),^(Db.qqq module_file_phrase),sysdate)`;
			canonical_source_phrase)

    fun cacheName (source_lang,target_lang) = "scs_dict"^ScsLang.toString source_lang ^ ScsLang.toString target_lang
    fun cacheFlush (source_lang,target_lang) = 
      case Ns.Cache.find (cacheName (source_lang,target_lang)) of
	NONE => ()
      | SOME c => Ns.Cache.flush c

    fun d source_lang module file_name phrase =
      let
	val can_phrase = canonical phrase
        val module_file_phrase = module ^ "-" ^ file_name ^ "-" ^ can_phrase 
	val cn = cacheName(source_lang,ScsLogin.user_lang)
	val t = 7200
      in
	case Ns.Cache.find cn of
	  NONE => let 
		    val v = lookup source_lang module file_name can_phrase module_file_phrase ScsLogin.user_lang
		  in
		    (Ns.Cache.set (Ns.Cache.createTm(cn,t),module_file_phrase,v);
		     v) 
		  end
	| SOME c => (case Ns.Cache.get (c,module_file_phrase) of 
		       NONE => let 
				 val v = lookup source_lang module file_name can_phrase 
                                           module_file_phrase ScsLogin.user_lang
			       in 
				 (Ns.Cache.set (c,module_file_phrase,v);
				  v) 
			       end 
		     | SOME v => v)
      end
    fun d' source_lang module file_name = Quot.fromString o (d source_lang module file_name) o Quot.toString

    fun dl source_lang module file_name args =
      let
	val dict = d source_lang module file_name
      in
	fn phrase => subst' (dict phrase) (Array.fromList (List.map (fn s => (String.size s,s)) args))
      end
    fun dl' source_lang module file_name args =
      let
	val dict = d source_lang module file_name
      in
	fn phrase => Quot.fromString (subst' (dict (Quot.toString phrase))
				      (Array.fromList (List.map (fn s => (String.size s,s)) args)))
      end

    fun d1 source_lang module file_name arg = dl source_lang module file_name [arg]
    fun d1' source_lang module file_name arg = dl' source_lang module file_name [arg]

    fun d2 source_lang module file_name arg0 arg1 = dl source_lang module file_name [arg0,arg1]
    fun d2' source_lang module file_name arg0 arg1 = dl' source_lang module file_name [arg0,arg1]

    fun d3 source_lang module file_name arg0 arg1 arg2 = dl source_lang module file_name [arg0,arg1,arg2]
    fun d3' source_lang module file_name arg0 arg1 arg2 = dl' source_lang module file_name [arg0,arg1,arg2]
  end


