-- $Id$

-- Also add an ordering used in selections: default increasing ordering

------------
-- Tables --
------------

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
  value varchar2(100),
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

  function addValue (
    name        in scs_enumerations.name%TYPE,
    value       in scs_enum_values.value%TYPE,
    language    in scs_lang.language%TYPE,
    text        in scs_text_lang.text%TYPE
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


  function getTID(
    val_id	in scs_enum_values.val_id%TYPE
  ) return scs_texts.text_id%TYPE;

  function getVID(
    enum_id	in scs_enumerations.enum_id%TYPE,
    value	in scs_enum_values.value%TYPE    
  ) return scs_enum_values.val_id%TYPE;

  function getEnumId (
    name	in scs_enumerations.name%TYPE
  ) return scs_enumerations.enum_id%TYPE;     

  function getName (
    enum_id	in scs_enumerations.enum_id%TYPE
  ) return scs_enumerations.name%TYPE;

  procedure swapOrdering(
    val_id1	in scs_enum_values.val_id%TYPE,
    val_id2	in scs_enum_values.val_id%TYPE
  ) ;

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

    exception
      when others then
        raise_application_error(scs.ScsDbExn,'scs_enumeration.new: Can''t create unum_id: ' || 
                                enum_id || ' with name: ' || name);
  end new;


  function updateValue(
    enum_id	in scs_enumerations.enum_id%TYPE,
    text_id	in scs_texts.text_id%TYPE,
    value	in scs_enum_values.value%TYPE
  ) return scs_enum_values.val_id%TYPE
  is
    val_id        scs_enum_values.val_id%TYPE;
    v_text_id_old scs_texts.text_id%TYPE;
    ordering	  scs_enum_values.ordering%TYPE;
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
      select count (*) into ordering from scs_enum_values where enum_id = updateValue.enum_id;
      ordering := ordering +1 ; 
      insert into scs_enum_values( val_id, enum_id, text_id, value, ordering ) 
      values ( val_id, enum_id, text_id, value, ordering );
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

  function getName (
    enum_id	in scs_enumerations.enum_id%TYPE
  ) return scs_enumerations.name%TYPE
  is
    v_name scs_enumerations.name%TYPE;
  begin
    select name
      into v_name
      from scs_enumerations
     where scs_enumerations.enum_id = getName.enum_id;

    return v_name;

  exception
    when NO_DATA_FOUND then
      raise_application_error(scs.ScsDbExn,'scs_enumeration.getName: can''t find enum_id: ' || enum_id);
  end getName;

  function getEnumId (
    name in scs_enumerations.name%TYPE
  ) return scs_enumerations.enum_id%TYPE
  is
    v_enum_id scs_enumerations.enum_id%TYPE;
  begin
    select enum_id
      into v_enum_id
      from scs_enumerations
     where scs_enumerations.name = getEnumId.name;

    return v_enum_id;

  exception
    when NO_DATA_FOUND then
      raise_application_error(scs.ScsDbExn,'scs_enumeration.getEnumId: can''t find name: ' || name);        
  end getEnumId;

  function addValue (
    name        in scs_enumerations.name%TYPE,
    value       in scs_enum_values.value%TYPE,
    language    in scs_lang.language%TYPE,
    text        in scs_text_lang.text%TYPE
  ) return scs_enum_values.val_id%TYPE
  is
  begin
    return scs_enumeration.updateValue(enum_id => getEnumId(addValue.name),
                                       value => addValue.value,
                                       text_id => scs_text.updateText(language => addValue.language,
                                                                      text => addValue.text));
  exception
    when others then
      raise_application_error(scs.ScsDbExn,'scs_enumeration.addValue: can''t add value ' || value);
  end addValue;

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

  function getVID(
    enum_id	in scs_enumerations.enum_id%TYPE,
    value	in scs_enum_values.value%TYPE    
  ) return scs_enum_values.val_id%TYPE
  is
    v_val_id scs_enum_values.val_id%TYPE;
  begin
    select distinct val_id
      into v_val_id
      from scs_enums
     where enum_id = getVID.enum_id
       and value = getVID.value;

    return v_val_id;
  exception
    when NO_DATA_FOUND then
      raise_application_error( scs.ScsDbExn, 'getVID: No val_id found for value='
				       || value || ' and enum_id=' || to_char(enum_id) ); 
  end getVID;

  function getTID(
    val_id	in scs_enum_values.val_id%TYPE
  ) return scs_texts.text_id%TYPE
  is
    text_id integer;
  begin
    select text_id into getTID.text_id 
      from scs_enum_values 
      where val_id = getTID.val_id ;

    if getTID.text_id is null then 
      raise_application_error( scs.ScsDbExn, 'No text_id found for val_id=' || to_char(val_id) ); 
    end if;

    return text_id; 

  exception
    when NO_DATA_FOUND then
      raise_application_error( scs.ScsDbExn, 'No text_id found for val_id=' || to_char(val_id) ); 
  end getTID;


  procedure swapOrdering(
    val_id1	in scs_enum_values.val_id%TYPE,
    val_id2	in scs_enum_values.val_id%TYPE
  )
  is 
    ordering1 integer;
    ordering2 integer;
  begin
    select ordering into ordering1 from scs_enum_values where val_id = val_id1;
    select ordering into ordering2 from scs_enum_values where val_id = val_id2;
    
    /* this is the nice way to do it. Unfortunately it only works from sqlplus
       (without variables) and cannot be parsed as a procedure by the plsql 
       parser:
    update scs_enum_values
    set ordering = 
      case when val_id = swapOrdering.val_id1 then swapOrdering.ordering2
           when val_id = swapOrdering.val_id2 then swapOrdering.ordering1
      end 
    where val_id in (val_id1, val_id2);
    */

    update scs_enum_values
    set ordering = -1
    where val_id = val_id1;

    update scs_enum_values
    set ordering = ordering1
    where val_id = val_id2;

    update scs_enum_values
    set ordering = ordering2
    where val_id = val_id1;

  end swapOrdering;


end scs_enumeration;
/
show errors
