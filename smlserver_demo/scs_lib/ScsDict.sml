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
  end

structure ScsDict :> SCS_DICT =
  struct 
    fun canonical s = String.concatWith " " (String.tokens Char.isSpace s)
      
    (* Looks up the source string s in the db. Returns
       the original string s, if no one is found in 
       the db and also inserts the missing row in
       the db (i.e., a query into the db can show
       which texts needs to be translated.) *)
    fun lookup target_lang source_lang source_text =
      if target_lang = source_lang then source_text 
      else
	case Db.zeroOrOneField `
          select d2.text from scs_dict d1, scs_dict d2 
           where d1.lang='^(ScsLang.toString source_lang)' 
             and d1.text = '^(canonical source_text)'
             and d1.phrase_id = d2.phrase_id 
             and d2.lang = '^(ScsLang.toString ScsLogin.user_lang)'` of
	       SOME target_t => target_t
	     | NONE => (Db.maybeDml `insert into scs_dict (dict_id,phrase_id,lang,text)
			values (^(Db.seqNextvalExp "dict_seq"),^(Db.seqNextvalExp "phrase_seq"),
				'^(ScsLang.toString source_lang)', '^(canonical source_text)')`;
			source_text)

    val d = fn source_lang => 
      Ns.Cache.cacheWhileUsed 
        (lookup ScsLogin.user_lang source_lang,
	 "scs_dict"^ScsLang.toString source_lang ^ ScsLang.toString ScsLogin.user_lang,7200)

    val d' = fn source_lang => Quot.fromString o (d source_lang) o Quot.toString
  end

