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
    constraint scs_enum_values_enum_id_nn not null,
  constraint scs_enum_values_enum_id_fk foreign key(enum_id) references scs_enumerations(enum_id),
  text_tid integer
    constraint scs_enum_values_text_tid_fk references scs_texts(text_id),
  value varchar2(30),
  constraint scs_enum_values_un unique(enum_id,value)
);

/
show errors


-------------------------
-- ENUMERATION PACKAGE --
-------------------------
create or replace package scs_enumeration
as
  function new(
    id		in scs_enumerations.enum_id%TYPE default null,
    name	in scs_enumerations.name%TYPE
  ) return scs_enumerations.enum_id%TYPE;

  procedure addValue(
    enum_id	in scs_enumerations.enum_id%TYPE,
    text_id	in scs_texts.text_id%TYPE,
    value	in scs_enum_values.value%TYPE
  ) ;

end scs_enumeration;
/
show errors

create or replace package body scs_enumeration
as
  function new (
    id	       in scs_enumerations.enum_id%TYPE default null,
    name       in scs_enumerations.name%TYPE
  ) return scs_enumerations.enum_id%TYPE
  is
    enum_id scs_enumerations.enum_id%TYPE;
  begin
    enum_id := scs.new_obj_id( new.id );
    insert into scs_enumerations( enum_id, name ) values (enum_id,name);
    return enum_id;
  end new;

  procedure addValue(
    enum_id	in scs_enumerations.enum_id%TYPE,
    text_id	in scs_texts.text_id%TYPE,
    value	in scs_enum_values.value%TYPE
  ) 
  is
    val_id scs_enum_values.val_id%TYPE;
  begin
    val_id := scs.new_obj_id;
    insert into scs_enum_values( val_id, enum_id, text_tid, value ) values ( val_id, enum_id, text_id, value );
  end addValue;

end scs_enumeration;
/
show errors

