-- $Id$


create table scs_enumerations(
  enum_id integer
    constraint scs_enumerations_enum_id_nn not null
    constraint scs_enumerations_enum_id_pk primary key,
  name varchar2(30)
    constraint scs_enumerations_name_nn not null
    constraint scs_enumerations_name_un unique
);

create table scs_enum_values(
  val_id integer
    constraint scs_enum_values_val_id_nn not null
    constraint scs_enum_values_val_id_pk primary key,
  enum_id integer
    constraint scs_enum_values_enum_id_nn not null
    constraint scs_enum_values_enum_id_fk references scs_enumerations(enum_id),
  text_id integer
    constraint scs_enum_values_text_id_fk references scs_texts(text_id),
  value varchar2(100)
    constraint scs_enum_values_value_nn	not null,
  active_p char(1) default ('t')
    constraint scs_enum_values_active_p_nn not null
    constraint scs_enum_values_active_p_ck
      check (active_p in ('t','f')),
  ordering integer
    constraint scs_enum_values_order_nn not null,
  constraint scs_enum_values_enu_val_un unique(enum_id,value),
  constraint scs_enum_values_enu_ord_un unique(enum_id,ordering)
);

-----------
-- Views --
-----------

create or replace view scs_enums as
select scs_enumerations.enum_id, scs_enumerations.name, 
       scs_enum_values.val_id, scs_enum_values.value, scs_enum_values.active_p, 
       scs_texts.text_id, 
       scs_text_lang.lang_id, scs_text_lang.text,
       scs_lang.language
  from scs_enumerations, scs_enum_values, scs_texts, scs_lang, scs_text_lang
 where scs_enumerations.enum_id = scs_enum_values.enum_id
   and scs_enum_values.text_id = scs_texts.text_id
   and scs_texts.text_id = scs_text_lang.text_id
   and scs_text_lang.lang_id = scs_lang.lang_id;
