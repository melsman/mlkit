-- $Id$

-- This code i a down-sized and modified version of the acs-lang module found
-- in openACS (www.openacs.org): file ad-locales.sql.

/* ======================================================================
   package scs_text

   multi language text string support

   History:
   141102 Kennie Nybo Pontoppidan <kennie@it-c.dk> added comments
   011002 Kennie Nybo Pontoppidan <kennie@it-c.dk> created package
====================================================================== */ 
create or replace package scs_text
as
  /* ------------
     function new
     ------------
     creates an entry in the table scs_texts and returns the text_id
  */
  function new (
    object_id in scs_texts.text_id%TYPE default null
  ) return scs_texts.text_id%TYPE;

  /* -----------------
     procedure destroy
     -----------------
     (physically) deletes one or more entries in scs_text_lang referencing 
     text_id. 
     The procedure never fails.
  */ 
  procedure destroy (
    text_id	in scs_texts.text_id%TYPE,
    language	in scs_lang.language%TYPE default null
  );

  /* -------------------
     function updateText
     -------------------
     updates one or two (language,text) pairs for text_id.
     If text_id is null, then a new entry in scs_texts is created first.
     Throws a ScsDbExn exception if 'language' or 'language2' is illegal
  */
  function updateText(
    text_id in scs_texts.text_id%TYPE default null,
    language in scs_lang.language%TYPE,
    text in scs_text_lang.text%TYPE,
    language2 in scs_lang.language%TYPE default null,
    text2 in scs_text_lang.text%TYPE default null
  ) return scs_texts.text_id%TYPE;

  /* -------------------
     function getText
     -------------------
     returns the text in language 'language' for text_id.
     
     Throws a ScsDbExn exception if 
        'language' is illegal 
     or 
        no text exists in language 'language'
  */  
  function getText(
    text_id in scs_texts.text_id%TYPE,
    language in scs_lang.language%TYPE
  ) return scs_text_lang.text%TYPE;

  /* -----------------
     function exists_p
     -----------------
     returns 't'/'f' whether or not there exists an entry in the 
       table scs_text_lang in language 'language'
     if language is null, then 
       returns 't'/'f' whether or not there exists an entry in the 
       table scs_texts with text_id='text_id'
  */  
  function exists_p(
    text_id in scs_texts.text_id%TYPE,
    language in scs_lang.language%TYPE default null
  ) return char;

end scs_text;
/
show errors


/* ======================================================================
   package bodies start here
====================================================================== */
create or replace package body scs_text
as
  function new(
    object_id in scs_texts.text_id%TYPE default null
  ) return scs_texts.text_id%TYPE
  is
    text_id scs_texts.text_id%TYPE;
  begin
    text_id := new.object_id;
    if text_id is null then
      text_id := scs.new_obj_id( new.object_id );
      insert into scs_texts ( text_id ) values ( text_id );
    end if;
    return text_id;
  end new;

  procedure destroy (
    text_id	in scs_texts.text_id%TYPE,
    language	in scs_lang.language%TYPE default null
  )
  is
    v_lang_id scs_lang.lang_id%TYPE;
  begin
    if language is null then
      -- delete the entire text in all languages
      -- and delete text_id from scs_texts
      delete scs_text_lang 
       where text_id = scs_text.destroy.text_id;

      delete scs_texts 
       where text_id = scs_text.destroy.text_id;
    else
      -- delete the text in one language only
      select lang_id 
        into v_lang_id 
        from scs_lang 
       where language = scs_text.destroy.language;      

      delete scs_text_lang 
       where text_id = scs_text.destroy.text_id 
         and lang_id = v_lang_id;
    end if;    
  exception
    when NO_DATA_FOUND then -- just in case language was illegal: do nothing
      return;
  end destroy;

  function updateText(
    text_id in scs_texts.text_id%TYPE default null,
    language in scs_lang.language%TYPE,
    text in scs_text_lang.text%TYPE,
    language2 in scs_lang.language%TYPE default null,
    text2 in scs_text_lang.text%TYPE default null
  ) return scs_texts.text_id%TYPE
  is
    new_text_id scs_texts.text_id%TYPE;
    lang_id scs_lang.lang_id%TYPE;
  begin
    new_text_id := scs_text.new( text_id ); -- might be unchanged

    select lang_id into updateText.lang_id 
    from scs_lang 
    where language = updateText.language;

    -- updates a text for a given language 
    -- or inserts (text,language) in scs_text_lang
    update scs_text_lang 
      set text = updateText.text
      where text_id = new_text_id and lang_id = updateText.lang_id;
    if sql%notfound then
       insert into scs_text_lang( text_id, lang_id, text ) 
       values ( new_text_id, lang_id, text );
    end if;

    if not language2 is null then
       new_text_id := updateText( new_text_id, language2, text2 );
    end if;

    return new_text_id;
  exception
    when no_data_found then
      raise_application_error( scs.ScsDbExn, 
			       'unknown language: '||updateText.language );
  end updateText;

  function getText(
    text_id in scs_texts.text_id%TYPE,
    language in scs_lang.language%TYPE
  ) return scs_text_lang.text%TYPE
  is
    text scs_text_lang.text%TYPE;
  begin
    select text into getText.text 
    from scs_text_lang tl, scs_lang l
    where text_id = getText.text_id 
    and l.language = getText.language 
    and tl.lang_id = l.lang_id;

    return text;
  exception 
    when no_data_found then
      raise_application_error( scs.ScsDbExn, 'no text in '||language||' for text_id '||to_char(text_id) );
  end getText;  

  function exists_p(
    text_id in  scs_texts.text_id%TYPE,
    language in scs_lang.language%TYPE default null
  ) return char
  is
    n integer;
  begin
    select count(*) 
      into n
      from scs_texts, scs_text_lang, scs_lang
     where scs_texts.text_id = exists_p.text_id
       and scs_texts.text_id = scs_text_lang.text_id(+)
       and scs_text_lang.lang_id = scs_lang.lang_id(+)
       and (exists_p.language is null 
            or scs_lang.language = exists_p.language);

    if n = 0 then
      return 'f';
    else
      return 't';
    end if;
  end exists_p;

end scs_text;
/ 
show errors






