-- All phrases gets its own number, phrase_id. One phase may 
-- be translated into several languages.
create table scs_dict_sources (
  phrase_id integer 
    constraint scs_dict_source_nn not null
    constraint scs_dict_source_pk primary key,
  lang char(2) 
    constraint scs_dict_source_lang_nn not null
    constraint scs_dict_source_lang_fk references scs_lang(language), 
  phrase varchar(2500)
    constraint scs_dict_source_phrase_nn not null,
  module varchar(300)
    constraint scs_dict_source_module_nn not null,
  file_name varchar(300) 
    constraint scs_dict_source_file_name_nn not null,
  module_file_phrase varchar(3200)
    constraint scs_dict_source_mod_file_ph_nn not null,
  create_date date default sysdate
    constraint scs_dict_source_create_date_nn not null,
  last_read_date date default sysdate
    constraint scs_dict_source_l_read_date_nn not null,
  -- for source phrases, we want all phrases within 
  -- the same module/file in one language to be unique
  -- This is implemented by having a column module_file_phrase
  -- that is the concatenation of the three. This is necessary in
  -- order to use the string -> string cache.
  constraint scs_dict_source_un unique (lang,module_file_phrase)
);

create table scs_dict_targets (
  target_id integer 
    constraint scs_dict_target_id_nn not null
    constraint scs_dict_target_id_pk primary key,
  phrase_id integer 
    constraint scs_dict_target_ph_id_nn not null
    constraint scs_dict_target_ph_id_fk
      references scs_dict_sources(phrase_id) on delete cascade,
  lang char(2)
    constraint scs_dict_target_lang_nn not null
    constraint scs_dict_target_lang_fk references scs_lang(language), 
  phrase varchar(3000) 
    constraint scs_dict_target_phrase_nn not null,
  last_modified date default sysdate 
    constraint scs_dict_target_last_mod_nn not null,
  modifying_user integer
    constraint scs_dict_target_mod_user_fk references scs_users(user_id)
    constraint scs_dict_target_mod_user_nn not null,
  -- we don't want lang and phrase to be unique as different source 
  -- phrases may translate into the same target phrase; however 
  -- phrase_id and lang must be unique
  constraint scs_dict_target_un unique (phrase_id,lang)
);

------------------------------------
-- Scs Dict Package Specification --
------------------------------------
create or replace package scs_dict
as
  procedure upd_last_read(
    phrase_id in scs_dict_sources.phrase_id%TYPE
  );

  function concat_module_file_phrase(
    module    in scs_dict_sources.module%TYPE,
    file_name in scs_dict_sources.file_name%TYPE,
    phrase    in scs_dict_sources.phrase%TYPE
  ) return scs_dict_sources.module_file_phrase%TYPE;
end scs_dict;
/
show errors

---------------------------
-- Scs Dict Package Body --
---------------------------
create or replace package body scs_dict
as
  procedure upd_last_read(
    phrase_id in scs_dict_sources.phrase_id%TYPE
  )
  is
  begin
    update scs_dict_sources
       set last_read_date = sysdate
     where phrase_id = upd_last_read.phrase_id;

  exception
    when others then
      raise_application_error(scs.ScsDbExn,'Can''t update scs_dict_sources.last_read for phrase_id ' || 
                                           phrase_id || '.');
  end upd_last_read;

  function concat_module_file_phrase(
    module    in scs_dict_sources.module%TYPE,
    file_name in scs_dict_sources.file_name%TYPE,
    phrase    in scs_dict_sources.phrase%TYPE
  ) return scs_dict_sources.module_file_phrase%TYPE
  is
  begin
    return module || '-' || file_name || '-' || phrase;
  end concat_module_file_phrase;

end scs_dict;
/
show errors

