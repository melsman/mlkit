-- $Id$

-- This code i a down-sized and modified version of the acs-lang module found
-- in openACS (www.openacs.org): file ad-locales.sql.

-- NB (knp) text field in scs_text_lang only has room for 4k data!

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
  text varchar2(4000),
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


/* 
======================================================================
 Initial data for languages and locales
======================================================================
*/

declare 
  lang_id_en integer;
  lang_id_da integer;
  tid integer;
begin
  select scs_object_id_seq.nextval
    into lang_id_en
    from dual;
  insert into scs_lang (lang_id, language) values (lang_id_en,'en');

  select scs_object_id_seq.nextval
    into lang_id_da
    from dual;
  insert into scs_lang (lang_id, language) values (lang_id_da,'da');

  tid := scs_text.updateText(language => 'da',
                             text => 'Engelsk',
                             language2 => 'en',
                             text2 => 'English');
  update scs_lang
     set language_name_tid = tid
   where scs_lang.lang_id = lang_id_en;

  tid := scs_text.updateText(language => 'da',
                             text => 'Dansk',
                             language2 => 'en',
                             text2 => 'Danish');
  update scs_lang
     set language_name_tid = tid
   where scs_lang.lang_id = lang_id_da;
end;
/
show errors

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








