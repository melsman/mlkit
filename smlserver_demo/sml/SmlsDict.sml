signature SMLS_DICT =
  sig
    (* The SMLserver Dictionary is a mechanism to produce 
       multilingual web-sites. First of all it defines the
       set of languages supported (e.g., Danish and English).
       
       You can tell the dictionary how to obtain personal
       language preferences for the user logged into the
       web-site. 

       The dictionary is stored in the database (file 
       smlsDict.sql). If the database is not accessible,
       or the user has no preferences (e.g., may not be
       logged in), then a default language is choosen. 

       The language support is divided in small texts and 
       larger texts. For larger texts, you will probably 
       include all the language versions in the 
       script-files. You can get the language 
       preference, and the use a case expression to
       choose the right text. If you use the language
       datatype then you get warnings on all files
       where a language is not supported.

       For small texts, you can use the caches dictionary
       functionality. You pass a text in the source language
       (English) and the sentence is translated into the
       language preferred (e.g., Danish). If the English
       phrase is not in the cache, then it is looked up
       in the database. If it is not found in the database,
       then the missing phrase is logged together with 
       information about the SML-file where the phrase
       is used. It is then up to the maintainer to include
       the phrase in the database.

       The Design Requirements:

         * Phrases from the database must be cached.
         * It should work even though the database is
           offline or that the phrase simply does not
	   exist on the preferred language - the 
	   original language is then used.
	 * The functions used to access the dictionary 
	   must be small - we choose the letter d that
	   works on strings and d' that works on
	   quotations

       Canonical Representation: All source texts are 
       stored with words separated by one space; new 
       lines etc. has been removed.
     *)
    val d : string -> string
  end

structure SmlsDict :> SMLS_DICT =
  struct 
    fun canonical s = String.concatWith " " (String.tokens Char.isSpace s)
      
    (* Looks up the source string s in the db. Returns
       the original string s, if no one is found in 
       the db and also inserts the missing row in
       the db (i.e., a query into the db can show
       which texts needs to be translated.) *)
    fun lookup source_lang target_lang source_text =
      case Db.zeroOrOneField `
          select d2.text from smls_dict d1, smls_dict d2 
           where d1.lang='^(SmlsLang.toString source_lang)' 
             and d1.text = '^(canonical source_text)'
             and d1.phrase_id = d2.phrase_id 
             and d2.lang = '^(SmlsLang.toString SmlsLogin.user_lang)'` of
        SOME target_t => target_t
      | NONE => (Db.dml `insert into smls_dict (dict_id,phrase_id,lang,text)
                         values (^(Db.seqNextvalExp "dict_seq"),^(Db.seqNextvalExp "phrase_seq"),
				 '^(SmlsLang.toString source_lang)', '^(canonical source_text)')`;
		 source_text)

    val d = Ns.Cache.cacheWhileUsed (lookup SmlsLogin.default_lang SmlsLogin.user_lang,"smls_dict",10)
  end

