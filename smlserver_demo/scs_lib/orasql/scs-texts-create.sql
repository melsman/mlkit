create table scs_texts(
  text_id integer
    constraint scs_texts_text_id_nn not null
    constraint scs_texts_text_id_pk primary key
);

create table scs_languages(
  language_id integer
    constraint scs_lang_language_id_nn not null
    constraint scs_lang_language_id_pk primary key,
  language_text_id integer
    constraint scs_lang_language_text_id_nn not null,
  constraint scs_lang_language_text_id_fk foreign key (language_text_id) references scs_texts(text_id)
);

create table scs_text_lang(
  text_id integer
    constraint scs_text_lang_text_id_nn not null
    constraint scs_text_lang_text_id_fk references scs_texts( text_id ),
  language_id integer
    constraint scs_text_lang_language_id_nn not null
    constraint scs_text_lang_language_id_fk references scs_languages( language_id ),
  text varchar2(100),
  constraint scs_text_un unique(text_id, language_id)
);



------------------------------
-- texts package prototypes --
------------------------------
create or replace package texts
as
  function new(
    object_id in integer default null
  ) return scs_texts.text_id%TYPE;

--  function new(
--    language_id in scs_languages.language_id%TYPE,
--    text in scs_text_lang.text%TYPE
--  ) return scs_texts.text_id%TYPE;

end texts;
/
show errors

------------------------
-- texts package body --
------------------------
create or replace package body texts
as
  function new(
    object_id in integer default null
  ) return scs_texts.text_id%TYPE
  is
    text_id scs_texts.text_id%TYPE;
-- might put exception declaration here
  begin
    text_id := scs.new_obj_id( object_id );
    insert into scs_texts values ( text_id );
    return text_id;
-- might put exception handling here
  end new;

end texts;
/ 
show errors


