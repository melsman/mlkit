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
  value varchar2(30),
  constraint scs_enum_values_un unique(enum_id,value)
);

-------------------------
-- ENUMERATION PACKAGE --
-------------------------
create or replace package scs_enumeration
as
  function new(
    enum_id	in scs_enumerations.enum_id%TYPE default null,
    name	in scs_enumerations.name%TYPE
  ) return scs_enumerations.enum_id%TYPE;

  function updateValue(
    enum_id	in scs_enumerations.enum_id%TYPE,
    text_id	in scs_texts.text_id%TYPE,
    value	in scs_enum_values.value%TYPE
  ) return scs_enum_values.val_id%TYPE;

  procedure delete (
    id		in scs_enumerations.enum_id%TYPE default null,
    name	in scs_enumerations.name%TYPE default null
  );

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


  function updateValue(
    enum_id	in scs_enumerations.enum_id%TYPE,
    text_id	in scs_texts.text_id%TYPE,
    value	in scs_enum_values.value%TYPE
  ) return scs_enum_values.val_id%TYPE
  is
    val_id scs_enum_values.val_id%TYPE;
  begin
    -- anonymous block needed here to handle
    -- case where no record was found
    -- (select statement throws an exception)
    begin
      select val_id into updateValue.val_id 
        from scs_enum_values
       where enum_id = updateValue.enum_id
         and value = updateValue.value;
    exception 
      when no_data_found then 
        commit;
    end;

    if val_id is null then
      val_id := scs.new_obj_id;
      insert into scs_enum_values( val_id, enum_id, text_id, value ) 
      values ( val_id, enum_id, text_id, value );
    else
      update scs_enum_values 
         set text_id = updateValue.text_id
       where enum_id = updateValue.enum_id
         and val_id = val_id;
    end if;
    return val_id;
  end updateValue;


  procedure delete (
    id		in scs_enumerations.enum_id%TYPE default null,
    name	in scs_enumerations.name%TYPE default null
  )
  is
    v_id scs_enumerations.enum_id%TYPE;
  begin
    if scs_enumeration.delete.name <> null then
      select enum_id
        into v_id
        from scs_enumerations
       where name = scs_enumeration.delete.name;
    else
      v_id := scs_enumeration.delete.id;
    end if;

    if v_id = null then
      return;
    end if;
   
    -- delete all texts describing enumerations
    for row in (select text_id 
                  from scs_enum_values 
                 where enum_id = v_id) loop
      scs_text.delete(row.text_id);
    end loop;

    -- delete all enumeration values.
    delete scs_enum_values where enum_id = v_id;

    -- delete the enumeration
    delete scs_enumerations where enum_id = v_id;
    return;
  end delete;


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





