create table scs_texts(
  text_id integer
    constraint scs_texts_text_id_nn not null
    constraint scs_texts_text_id_pk primary key
);

-- NB! the four commented lines are added in the scs-locales-create file
create table scs_text_lang(
  text_id integer
    constraint scs_text_lang_text_id_nn not null
    constraint scs_text_lang_text_id_fk references scs_texts( text_id ),
--  lang_id integer
--    constraint scs_text_lang_lang_id_nn not null
--    constraint scs_text_lang_lang_id_fk references scs_languages( language_id ),
  text varchar2(100)
--  constraint scs_text_un unique(text_id, language_id)
);


---------------------------------
-- scs_text package prototypes --
---------------------------------
create or replace package scs_text
as
  function new (
    object_id in scs_texts.text_id%TYPE default null
  ) return scs_texts.text_id%TYPE;

  function updateText(
    text_id in scs_texts.text_id%TYPE default null,
    language in scs_lang.language%TYPE,
    text in scs_text_lang.text%TYPE,
    language2 in scs_lang.language%TYPE default null,
    text2 in scs_text_lang.text%TYPE default null
  ) return scs_texts.text_id%TYPE;

  function getText(
    text_id in scs_texts.text_id%TYPE,
    language in scs_lang.language%TYPE
  ) return scs_text_lang.text%TYPE;

end scs_text;
/
show errors

---------------------------
-- scs_text package body --
---------------------------
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

    select lang_id into updateText.lang_id from scs_lang where language = updateText.language;

    -- updates a text for a given language or inserts (text,language) in scs_text_lang
    update scs_text_lang 
      set text = updateText.text
      where text_id = new_text_id and lang_id = updateText.lang_id;
    if sql%notfound then
       insert into scs_text_lang( text_id, lang_id, text ) values ( new_text_id, lang_id, text );
    end if;

    if not language2 is null then
       new_text_id := updateText( new_text_id, language2, text2 );
    end if;

    return new_text_id;
  exception
    when no_data_found then
      raise_application_error( -20000, 'unknown language: '||updateText.language );
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
    where text_id = getText.text_id and l.language = getText.language and tl.lang_id = l.lang_id;

    return text;
  exception 
    when no_data_found then
      raise_application_error( -20001, 'no text in '||language||' for text_id '||to_char(text_id) );
  end getText;  

end scs_text;
/ 
show errors





