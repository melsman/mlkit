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

--------------
-- TRIGGERS --
--------------

-- The trigger makes sure that the affiliated scs-texts are deleted
-- when we delete an enumeration.
create or replace trigger scs_enum_values_before_del_tr
before delete on scs_enum_values
for each row
declare
begin
  scs_text.delete(:old.text_id);
end scs_enum_values_before_del_tr;
/
show errors;

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

  procedure destroy (
    enum_id     in scs_enumerations.enum_id%TYPE
  );
 
  procedure destroy (
    name	in scs_enumerations.name%TYPE
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
    val_id        scs_enum_values.val_id%TYPE;
    v_text_id_old scs_texts.text_id%TYPE;
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
      begin
        select text_id
          into v_text_id_old
          from scs_enum_values
         where val_id = val_id;
      exception
        when no_data_found then 
          commit;
      end;
      
      update scs_enum_values 
         set text_id = updateValue.text_id
       where val_id = val_id;
      
      if v_text_id_old is not null and v_text_id_old <> updateValue.text_id then
        scs_text.delete(v_text_id_old);
      end if;
    end if;
    return val_id;
  end updateValue;

  procedure destroy (
    enum_id in scs_enumerations.enum_id%TYPE
  )
  is
  begin
    -- delete all enumeration values.
    -- the affiliated scs_texts are deleted by trigger
    -- scs_enum_values_before_del_tr
    delete scs_enum_values where enum_id = destroy.enum_id;

    -- delete the enumeration
    delete scs_enumerations where enum_id = destroy.enum_id;
    return;
  end destroy;

  procedure destroy (
    name	in scs_enumerations.name%TYPE
  )
  is
    v_enum_id scs_enumerations.enum_id%TYPE;
  begin
    select enum_id
      into v_enum_id
      from scs_enumerations
     where name = scs_enumeration.destroy.name;

    destroy(v_enum_id);
    return;

  -- If name does not exist then we silently return
  exception 
    when NO_DATA_FOUND then
      return;
  end destroy;

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
      raise_application_error( scs.ScsDbExn, 'No text_id found for value='
				       || value || ' and enum_id=' || to_char(enum_id) ); 
    end if;

    return text_id; 

  exception
    when NO_DATA_FOUND then
      raise_application_error( scs.ScsDbExn, 'No text_id found for value='
				       || value || ' and enum_id=' || to_char(enum_id) ); 
  end getTID;

end scs_enumeration;
/
show errors





