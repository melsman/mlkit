signature SCS_DICT =
  sig
    (* The SMLserver Dictionary is a mechanism to produce 
       multilingual web-sites. First of all it defines the
       set of languages supported (e.g., Danish and English
       in ScsLang.sml).
       
       You can tell the dictionary how to obtain personal
       language preferences for the user logged into the
       web-site. 

       Some of the functions will store the dictionary in the database
      (file scs-dict-create.sql). If the database is not accessible,
       or the user has no preferences (e.g., may not be
       logged in), then a default language is chosen. 

       The language support is divided in a dictionary stored in the
       database and a dictionary coded in the ML code. The functions
       below works on either the database or on translation options
       passed as arguments.

       For larger texts, you will probably
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

       Using the database to store translations is different from what
       most people do, and we still need experience with its use to
       see whether its a good idea or not.

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
         * It must be possible to move a (part of a) dictionary from
           one database to another database

       Canonical Representation: All source texts stored in the
       database are stored with words separated by one space; new
       lines etc. has been removed.
     *)

     (*********************************************)
     (* The functions below works on the database *)
     (*********************************************)

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

    (* [cacheName (source,target)] returns the name of the case used to
       store translations from source language to target language *)
    val cacheName  : ScsLang.lang * ScsLang.lang -> (string,string) Ns.Cache.cache

    (* [cacheFlush (source,target)] flushes the cache used to 
       store translations from source language to target language *)
    val cacheFlush : ScsLang.lang * ScsLang.lang -> unit

    (* [dict] type describing one phrase in many languages *)
    type dict = (ScsLang.lang * quot) list

    (* [getDictFromDb id val_id_p] returns a ScsDict.dict representing the 
       tid in the table scs_texts. If val_id_p is true then the tid is
       looked up first *)
    val getDictFromDb : int -> bool -> dict

    (*******************************************************)
    (* The functions below works on a list of translations *)
    (* passed explicitly to each function (type dict).     *)
    (*******************************************************)

    (* [ScsDict] exception raised on error *)
    exception ScsDict of string

    (* [s dict] returns the first phrase in the dictionary dict
       corresponding to the preferred language by the logged in user. 
       The first phrase in dict is returned if the preferred language
       is not in dict. Raises ScsDict if the dictionary is empty. *)
    val s : dict -> string

    (* [s' dict] returns the first phrase in the dictionary dict
       corresponding to the preferred language by the logged in user. 
       The first phrase in dict is returned if the preferred language
       is not in dict. Raises ScsDict if the dictionary is empty. *)
    val s' : dict -> quot
 
    (* [sl dict args] returns the first phrase in the dictionary dict
       corresponding to the preferred language by the logged in
       user. The first phrase in dict is used if the preferred
       language is not in dict. Any %n pattern in the phrase surrounded
       with white space is replaced by the n'th arg in the list
       args. *)
    val sl : dict -> string list -> string

    (* [sl' dict args] returns the first phrase in the dictionary dict
       corresponding to the preferred language by the logged in
       user. The first phrase in dict is used if the preferred
       language is not in dict. Any %n pattern in the phrase surrounded
       with white space is replaced by the n'th arg in the list
       args. *)
    val sl' : dict -> string list -> quot

    (* [dictWithArgsToDict dict args] substitutes args into dict and
       returns a new dict with any %n pattern substituted with some
       arg from args. The following

          val str = sl dict args
          val str' = sl' dict args

       is similar to
  
          val str = s (dictWithArgsToDict dict args)
          val str = s' (dictWithArgsToDict dict args) *)
    val dictWithArgsToDict : dict -> string list -> dict

    (* [subst phase args] returns a quotations where all %0,%1,... are
       replaced by the respective arguments in args. *)
    val subst : string -> string list -> quot

    (* [getQuot dict lang] returns the quotation in language lang.
       Raises ScsDict if the dictionary is empty or if no quotation exists
       in language lang
    *)
    val getQuot : dict -> ScsLang.lang -> quot

    (* [getString dict lang] returns the string in language lang.
       Raises ScsDict if the dictionary is empty or if no quotation exists
       in language lang
    *)
    val getString : dict -> ScsLang.lang -> string

  end

structure ScsDict :> SCS_DICT =
  struct 
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

    fun cacheName (source_lang,target_lang) = 
      Ns.Cache.get(Ns.Cache.String,
		   Ns.Cache.String,
		   "scs_dict"^ScsLang.toString source_lang ^ ScsLang.toString target_lang,
		   Ns.Cache.TimeOut 7200)

    fun cacheFlush (source_lang,target_lang) = 
      Ns.Cache.flush (cacheName (source_lang,target_lang))

    fun getDictFromDb id val_id_p = 
      let
        val id_str = if val_id_p then 
            Quot.toString `scs_enumeration.getTID(^(Int.toString id))` 
          else Int.toString id
        val text_list_opt = Db.zeroOrOneRow `
          select scs_text.getText(^id_str, '^(ScsLang.toString ScsLang.da)'),
                 scs_text.getText(^id_str, '^(ScsLang.toString ScsLang.en)')
            from dual`
        fun f (x::y::[]) = [(ScsLang.da, Quot.fromString x),
			    (ScsLang.da, Quot.fromString y)]
          | f xs         = raise Option
      in
	f (valOf text_list_opt)
        handle Option => [(ScsLang.da, `Databasefejl i ScsDict.getDictFromDb`)]
      end

    fun d source_lang module file_name phrase =
      let
	val can_phrase = ScsString.canonical phrase
        val module_file_phrase = module ^ "-" ^ file_name ^ "-" ^ can_phrase 
	val cn = cacheName(source_lang,ScsLogin.user_lang)
      in
	case Ns.Cache.lookup cn module_file_phrase of
	  NONE => let
		    val v = lookup source_lang module file_name can_phrase module_file_phrase ScsLogin.user_lang
		  in
		    Ns.Cache.insert (cn,module_file_phrase,v);
		    v
		  end
	| SOME v => v
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

    (*******************************************************)
    (* The functions below works on a list of translations *)
    (* passed explicitly to each function (type dict).     *)
    (*******************************************************)

    type dict = (ScsLang.lang * quot) list

    exception ScsDict of string

    fun getQuot [] language = raise ScsDict "ScsDict.s: dictionary missing"
      | getQuot ((lang,phrase)::xs) language =
	if language = lang then phrase 
	else
	  case List.find (fn (lang,phrase) => lang = language) xs of
            NONE => phrase (* We return the first phrase in the list if user-preferred language does not exists *)
          | SOME xs => #2(xs)

    fun s' dict = getQuot dict ScsLogin.user_lang

    fun getString dict lang = Quot.toString ( getQuot dict lang )

    val s = Quot.toString o s'

    fun subst phrase args =
      Quot.fromString (subst' phrase
		       (Array.fromList (List.map (fn s => (String.size s,s)) args)))

    fun sl' dict args =
      let
	val phrase = s dict
      in
	subst phrase args
      end          

    fun sl dict args = Quot.toString(sl' dict args)   
      
    fun dictWithArgsToDict dict args =
      List.map (fn (lang,phrase) => (lang, subst (Quot.toString phrase) args)) dict


  end


