-- $Id$
-- This code i a down-sized and modified version of the acs-lang module found
-- in openACS (www.openacs.org): file ad-locales.sql.
--
-- The package scs_texts was written by Kennie Nybo Pontoppidan

create table scs_texts(
  text_id integer
    constraint scs_texts_text_id_nn not null
    constraint scs_texts_text_id_pk primary key
);

create table scs_lang (
  lang_id integer
    constraint scs_lang_lang_id_pk primary key
    constraint scs_lang_lang_id_nn not null,
  language char(2) 
    constraint scs_lang_language_nn not null
    constraint scs_lang_language_un unique,
  language_name_tid integer
    constraint scs_lang_language_name_tid_fk references scs_texts(text_id)
);

create table scs_text_lang(
  text_id integer
    constraint scs_text_lang_text_id_nn not null
    constraint scs_text_lang_text_id_fk references scs_texts( text_id ),
  lang_id integer
    constraint scs_text_lang_lang_id_nn not null
    constraint scs_text_lang_lang_id_fk references scs_lang( lang_id ), 
  text varchar2(100),
  constraint scs_text_un unique( text_id, lang_id )
);

create table scs_locales (
  locale varchar2(30)
    constraint scs_locales_locale_nn not null
    constraint scs_locales_locale_pk primary key,
  language char(2) 
    constraint scs_locales_language_nn not null
    constraint scs_locales_language_fk references scs_lang(language),
  country  char(2) 
    constraint scs_locales_country_nn not null,
  label	varchar2(200)
    constraint scs_locales_label_nn not null
    constraint scs_locates_label_un unique,
  nls_language varchar2(30)
    constraint scs_locales_nls_lang_nn not null,
  nls_territory varchar2(30),
  nls_charset varchar2(30),
  mime_charset varchar2(30),
  -- is this the default locale for its language
  default_p char(1) default 'f'
    constraint scs_locales_default_p_nn not null
    constraint scs_locales_default_p_ck check(default_p in ('t','f'))
);

comment on table scs_locales is ' An SCS locale is identified by a
  language and country.  Locale definitions in Oracle consist of a
  language, and optionally territory and character set.  (Languages
  are associated with default territories and character sets when not
  defined).  The formats for numbers, currency, dates, etc. are
  determined by the territory.  language is two letter abbrev is ISO
  639 language code country is two letter abbrev is ISO 3166 country
  code mime_charset is IANA charset name nls_charset is Oracle charset
  name ';


---------------------------------
-- scs_text package prototypes --
---------------------------------
create or replace package scs_text
as
  function new (
    object_id in scs_texts.text_id%TYPE default null
  ) return scs_texts.text_id%TYPE;

  procedure delete (
    text_id	in scs_texts.text_id%TYPE,
    language	in scs_lang.language%TYPE default null
  );

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

  function exists_p(
    text_id in scs_texts.text_id%TYPE,
    language in scs_lang.language%TYPE default null
  ) return char;

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

  procedure delete (
    text_id	in scs_texts.text_id%TYPE,
    language	in scs_lang.language%TYPE default null
  )
  is
    v_lang_id scs_lang.lang_id%TYPE;
  begin
    if language <> null then
      select lang_id 
        into v_lang_id 
        from scs_lang 
       where language = scs_text.delete.language;      
      -- delete the text in one language only.
      delete scs_text_lang 
       where text_id = scs_text.delete.text_id 
         and lang_id = v_lang_id;
    else
      -- delete the entire text in all languages.
      delete scs_text_lang 
       where text_id = scs_text.delete.text_id;
    end if;    

    -- we delete the text_id only if there is no rows in 
    -- scs_text_lang. Notice the reference constraint in 
    -- table scs_text_lang.
    delete scs_texts 
     where text_id = scs_text.delete.text_id;
    return;
  end delete;

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
      raise_application_error( scs.ScsDbExn, 'unknown language: '||updateText.language );
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




/* 
======================================================================
 Initial data for languages and locales
======================================================================
*/
insert into scs_lang (lang_id, language) values (scs_object_id_seq.nextval, 'en');
insert into scs_lang (lang_id, language) values (scs_object_id_seq.nextval, 'da');

-- NB! (knp): remember to put language_name_tid here for en, da


/* these languages are not used at IT-c
insert into scs_lang (lang_id, language) values (scs_object_id_seq.nextval, 'no');
insert into scs_lang (lang_id, language) values (scs_object_id_seq.nextval, 'sv');
insert into scs_lang (lang_id, language) values (scs_object_id_seq.nextval, 'fi');
insert into scs_lang (lang_id, language) values (scs_object_id_seq.nextval, 'de');
insert into scs_lang (lang_id, language) values (scs_object_id_seq.nextval, 'es');
insert into scs_lang (lang_id, language) values (scs_object_id_seq.nextval, 'fr');
insert into scs_lang (lang_id, language) values (scs_object_id_seq.nextval, 'ja');
*/

insert into scs_locales
  (locale, label, language, country, nls_language, 
   nls_territory, nls_charset, mime_charset, default_p) 
values 
  ('en_US', 'American', 'en', 'US',
   'AMERICAN', 'AMERICA', 'WE8ISO8859P1', 'ISO-8859-1', 't');

insert into scs_locales
  (locale, label, language, country, nls_language, 
   nls_territory, nls_charset, mime_charset, default_p) 
values 
  ('da_DK', 'Danish', 'da', 'DK',
   'DANISH', 'DENMARK', 'WE8ISO8859P1', 'ISO-8859-1', 't');

/* these locales are not used at IT-c
insert into scs_locales
  (locale, label, language, country, nls_language, 
   nls_territory, nls_charset, mime_charset, default_p) 
values 
  ('no_NO', 'Norwegian', 'no', 'NO',
   'NORWEGIAN', 'NORWAY', 'WE8ISO8859P1', 'ISO-8859-1', 't');

insert into scs_locales
  (locale, label, language, country, nls_language, 
   nls_territory, nls_charset, mime_charset, default_p) 
values 
  ('sv_SE', 'Swedish', 'sv', 'SE',
   'SWEDISH', 'SWEDEN', 'WE8ISO8859P1', 'ISO-8859-1', 't');

insert into scs_locales
  (locale, label, language, country, nls_language, 
   nls_territory, nls_charset, mime_charset, default_p) 
values 
  ('fi_FI', 'Finnish', 'fi', 'FI',
   'FINNISH', 'FINLAND', 'WE8ISO8859P1', 'ISO-8859-1', 't');

insert into scs_locales
  (locale, label, language, country, nls_language, 
   nls_territory, nls_charset, mime_charset, default_p)
values
  ('de_DE', 'German', 'de', 'DE',
   'GERMAN', 'GERMANY', 'WE8ISO8859P1', 'ISO-8859-1', 't');

insert into scs_locales
  (locale, label, language, country,
   nls_language, nls_territory, nls_charset, mime_charset, default_p)
values
  ('es_ES', 'Spain', 'es', 'ES',
   'SPANISH', 'SPAIN', 'WE8ISO8859P1', 'ISO-8859-1', 't');

insert into scs_locales
  (locale, label, language, country,
   nls_language, nls_territory, nls_charset, mime_charset, default_p)
values
  ('fr_FR', 'French', 'fr', 'FR',
   'FRENCH', 'France', 'WE8ISO8859P1', 'ISO-8859-1', 't');

insert into scs_locales
  (locale, label, language, country, 
   nls_language, nls_territory, nls_charset, mime_charset, default_p)
values
  ('ja_JP', 'Japanese', 'ja', 'JP',
   'JAPANESE', 'JAPAN', 'JA16SJIS', 'Shift_JIS', 't');
*/

commit;






