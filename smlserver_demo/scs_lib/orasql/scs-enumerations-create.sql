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
  text_id integer
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
    enum_id	in scs_enumerations.enum_id%TYPE default null,
    name	in scs_enumerations.name%TYPE
  ) return scs_enumerations.enum_id%TYPE;

  procedure updateValue(
    enum_id	in scs_enumerations.enum_id%TYPE,
    text_id	in scs_texts.text_id%TYPE,
    value	in scs_enum_values.value%TYPE
  ) ;

  function getTID(
    enum_id	in scs_enumerations.enum_id%TYPE,
    value	in scs_enum_values.value%TYPE    
  ) return scs_texts.text_id%TYPE;

end scs_enumeration;
/
show errors

create or replace package body scs_enumeration
as
  function new (
    enum_id    in scs_enumerations.enum_id%TYPE default null,
    name       in scs_enumerations.name%TYPE
  ) return scs_enumerations.enum_id%TYPE
  is
    new_enum_id scs_enumerations.enum_id%TYPE;
  begin
    new_enum_id := scs.new_obj_id( new.enum_id );
    insert into scs_enumerations( enum_id, name ) values (new_enum_id,name);
    return new_enum_id;
  end new;


  procedure updateValue(
    enum_id	in scs_enumerations.enum_id%TYPE,
    text_id	in scs_texts.text_id%TYPE,
    value	in scs_enum_values.value%TYPE
  ) 
  is
    val_id scs_enum_values.val_id%TYPE;
  begin
    val_id := scs.new_obj_id;

    update scs_enum_values 
    set text_id = updateValue.text_id
    where enum_id = updateValue.enum_id
      and value = updateValue.value;
    if sql%notfound then
      insert into scs_enum_values( val_id, enum_id, text_id, value ) 
      values ( val_id, enum_id, text_id, value );
    end if;
  end updateValue;


  function getTID(
    enum_id	in scs_enumerations.enum_id%TYPE,
    value	in scs_enum_values.value%TYPE    
  ) return scs_texts.text_id%TYPE
  is
    text_id integer;
  begin
    select text_id into getTID.text_id 
      from scs_enum_values 
      where enum_id = getTID.enum_id 
        and value = getTID.value;
    if getTID.text_id is null then 
      raise_application_error( -20000, 'No text_id found for value='
				       || value || ' and enum_id=' || to_char(enum_id) ); 
    end if;

    return text_id; 
  end getTID;


end scs_enumeration;
/
show errors

