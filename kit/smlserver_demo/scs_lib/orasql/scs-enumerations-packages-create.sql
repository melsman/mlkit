-- $Id$

-- KNP: isn't this here? 
--   Also add an ordering used in selections: default increasing ordering

/* ======================================================================
   package scs_enumeration

   History:
   310103 Niels Hallenberg <nh@it-c.dk> moved trigger into this file
   281102 Kennie Nybo Pontoppidan <kennie@it-c.dk> added comments
   261102 Kennie Nybo Pontoppidan <kennie@it-c.dk> code review
   111002 Kennie Nybo Pontoppidan <kennie@it-c.dk> created package
====================================================================== */
create or replace package scs_enumeration
as
  /* ------------
     function new
     ------------
     creates a new enumeration with enum_id 'enum_id' and name 'name'
     returns the enum_id for the created row

     throws a ScsDbExn exception if there already exists an enumeration
       with this id or this name 
  */
  function new(
    enum_id	in scs_enumerations.enum_id%TYPE default null,
    name	in scs_enumerations.name%TYPE
  ) return scs_enumerations.enum_id%TYPE;

  /* -----------------
     function exists_p
     -----------------
     returns 't' if there is a row in scs_enumerations with id 'enum_id'
	     'f' if not
  */
  function exists_p(
    enum_id	in scs_enumerations.enum_id%TYPE
  ) return char;

  /* --------------------
     function updateValue
     --------------------
     for 
       legal enum_id, known value
         updates the text_id (and physically deletes the old text_id) 
         for the enumeration value identified by (enum_id, value)
       legal enum_id, unknown value
         a new row is inserted in scs_enum_values
     returns val_id on success

     throws a ScsDbExn exception if enum_id is unknown
				 or text_id is unknown
  */
  function updateValue(
    enum_id	in scs_enumerations.enum_id%TYPE,
    text_id	in scs_texts.text_id%TYPE,
    value	in scs_enum_values.value%TYPE
  ) return scs_enum_values.val_id%TYPE;

  /* -----------------
     function addValue
     -----------------
     adds a value in scs_enum_values 
     
     throws a ScsDbExn exception if 
          name is unknown in scs_enumerations
       or 
          value is null
       or
          there exists a (enum_id, value) pair 
            (for the enum_id associated with 'name')
       or
          language is unknown       
  */
  function addValue (
    name        in scs_enumerations.name%TYPE,
    value       in scs_enum_values.value%TYPE,
    language    in scs_lang.language%TYPE,
    text        in scs_text_lang.text%TYPE
  ) return scs_enum_values.val_id%TYPE;

  /* -----------------
     procedure destroy
     -----------------
     physically deletes the enumeration with enum_id 'enum_id'
     and all enum_values, texts associated with it

     never fails
  */
  procedure destroy (
    enum_id     in scs_enumerations.enum_id%TYPE
  );

  /* -----------------
     procedure destroy
     -----------------
     physically deletes the enumeration with name 'name'
     and all enum_values, texts associated with it

     never fails
  */ 
  procedure destroy (
    name	in scs_enumerations.name%TYPE
  );

  /* ---------------
     function getTID
     ---------------
     returns the text_id for a (enum_id, value) pair
	     NB: the return value can be null

     throws a ScsDbExn exception if there doesn't exist 
       any (enum_id, value) pair
  */
  function getTID(
    enum_id	in scs_enumerations.enum_id%TYPE,
    value	in scs_enum_values.value%TYPE    
  ) return scs_texts.text_id%TYPE;

  /* ---------------
     function getTID
     ---------------
     returns the text_id for a val_id
	     NB: the return value can be null

     throws a ScsDbExn exception if there doesn't exist 
       any row with val_id
  */
  function getTID(
    val_id	in scs_enum_values.val_id%TYPE
  ) return scs_texts.text_id%TYPE;

  /* ------------------
     function getVID
     ------------------
     looks up the pair (enum_id, value) in scs_enum_values 
     and returns the corresponding val_id
     
     if no row exists with (enum_id, value), null is returned
  */
  function getVID(
    enum_id	in scs_enumerations.enum_id%TYPE,
    value	in scs_enum_values.value%TYPE    
  ) return scs_enum_values.val_id%TYPE;

  /* ------------------
     function getVID
     ------------------
     looks up the pair (name, value) in scs_enumerations, scs_enum_values 
     and returns the corresponding val_id
     
     if no row exists with (enum_id, value), null is returned
  */
  function getVID(
    name	in scs_enumerations.name%TYPE,
    value	in scs_enum_values.value%TYPE    
  ) return scs_enum_values.val_id%TYPE;


  /* ------------------
     function getEnumId
     ------------------
     looks up name in scs_enumerations and returns the corresponding enum_id
     
     if no row exists with this name, null is returned
  */
  function getEnumId (
    name	in scs_enumerations.name%TYPE
  ) return scs_enumerations.enum_id%TYPE;     

  /* ------------------
     function getName
     ------------------
     looks up enum_id in scs_enumerations and returns the corresponding name
     
     if no row exists with this enum_id, null is returned
  */
  function getName (
    enum_id	in scs_enumerations.enum_id%TYPE
  ) return scs_enumerations.name%TYPE;

  /* ------------------
     function getVal
     ------------------
     looks up val_id in scs_enum_values and returns the corresponding value
     
     if no row exists with this val_id, null is returned
  */
  function getVal (
    val_id in scs_enum_values.val_id%TYPE
  ) return scs_enum_values.value%TYPE;

  /* ----------------------
     procedure swapOrdering
     ----------------------
     swaps the ordering for two rows in scs_enum_values

     throws a ScsDbExn if
          there is an unknown val_id
       or 
          val_ids are associated with different enumerations
  */  
  procedure swapOrdering(
    val_id1	in scs_enum_values.val_id%TYPE,
    val_id2	in scs_enum_values.val_id%TYPE
  ) ;

  /* --------------------
     function vidToString
     --------------------
     returns the text field (possibly null) from the scs_text_lang
     row identified by text_id (associated with val_id) and language 

     throws a ScsDbExn exception if
          val_id is unknown
       or
          language is unknown
       or
          no text exists on language 'language'
  */
  function vidToString (
    val_id in scs_enum_values.val_id%TYPE,
    language    in scs_lang.language%TYPE
  ) return varchar2;
end scs_enumeration;
/
show errors


/* ======================================================================
   package bodies start here
====================================================================== */ 
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
        raise_application_error(
	  scs.ScsDbExn, 'scs_enumeration.new: Can''t create enum_id: ' || 
			enum_id || ' with name: ' || name);
  end new;

  function exists_p(
    enum_id	in scs_enumerations.enum_id%TYPE
  ) return char
  is
    counter	integer;
  begin
    select count(*) into exists_p.counter from scs_enumerations
     where enum_id = exists_p.enum_id;

    if counter = 0 then
      return 'f';
    else 
      return 't';
    end if;
  end exists_p;

  function updateValue(
    enum_id	in scs_enumerations.enum_id%TYPE,
    text_id	in scs_texts.text_id%TYPE,
    value	in scs_enum_values.value%TYPE
  ) return scs_enum_values.val_id%TYPE
  is
    val_id			scs_enum_values.val_id%TYPE;
    v_text_id_old		scs_texts.text_id%TYPE;
    ordering			scs_enum_values.ordering%TYPE;
  begin
    if exists_p( enum_id ) = 'f' then
      raise_application_error(
        scs.ScsDbExn, 'scs_enumeration.updateValue: can''t find enum_id: ' 
		      || to_char(enum_id));
    end if;

    -- anonymous block needed here to handle
    -- case where no record was found
    -- (select statement throws an exception)
    begin
      select val_id into updateValue.val_id 
        from scs_enum_values
       where enum_id = updateValue.enum_id
         and value = updateValue.value;
    exception 
      when NO_DATA_FOUND then 
        begin -- create a new enum_value
          val_id := scs.new_obj_id;
          select count (*) into ordering 
            from scs_enum_values 
           where enum_id = updateValue.enum_id;
          ordering := ordering +1 ; 
          insert into scs_enum_values( val_id, enum_id, text_id, value, ordering )
          values ( val_id, enum_id, text_id, value, ordering );
  	exception
	  when others then 
	    if SQLCODE = 2291 then -- unknown text_id
  	      raise_application_error(
	        scs.ScsDbExn, 'scs_enumeration.updateValue: can''t find text_id: ' 
			      || to_char(text_id));
	    else 
	      raise_application_error(
	        scs.ScsDbExn, 'scs_enumeration.updateValue: unknown error' );
	    end if;
	end;
    end;

    -- get old text_id
    begin
      select text_id
        into v_text_id_old
        from scs_enum_values
       where val_id = updateValue.val_id;
    exception
      when NO_DATA_FOUND then 
        v_text_id_old := null;
    end;
      
    update scs_enum_values 
       set text_id = updateValue.text_id
     where val_id = updateValue.val_id;

    -- destroy old text_id     
    if v_text_id_old is not null and 
       v_text_id_old <> updateValue.text_id then
      scs_text.destroy(v_text_id_old);
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
      return null;
  end getName;

  function getVal (
    val_id in scs_enum_values.val_id%TYPE
  ) return scs_enum_values.value%TYPE
  is
    v_value scs_enum_values.value%TYPE;
  begin
    select value
      into v_value
      from scs_enum_values
     where val_id = getVal.val_id;

    return v_value;
  exception
    when NO_DATA_FOUND then
      return null;
  end getVal;

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
      return null;
  end getEnumId;

  function addValue (
    name        in scs_enumerations.name%TYPE,
    value       in scs_enum_values.value%TYPE,
    language    in scs_lang.language%TYPE,
    text        in scs_text_lang.text%TYPE
  ) return scs_enum_values.val_id%TYPE
  is
    eid			scs_enum_values.enum_id%TYPE;
    value_exists	exception;
  begin
    eid := getEnumId(addValue.name); 

    -- only create new entry if no (enum_id, value) pair exists
    if getVID( enum_id => eid, value => addValue.value ) is null then
      return scs_enumeration.updateValue(
        enum_id => eid,
        value => addValue.value,
        text_id => scs_text.updateText( language => addValue.language,
					text => addValue.text) );
    else 
      raise value_exists;
    end if;
  exception
    when value_exists then 
      raise_application_error(
        scs.ScsDbExn,'scs_enumeration.addValue: 
		      ('|| eid ||','||value||') exists)' );
    when others then
      raise_application_error(
        scs.ScsDbExn, 'scs_enumeration.addValue: can''t add value ' || value
		      || ',' || SQLERRM);
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
  exception   -- If name does not exist then we silently return
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
    return text_id; 

  exception
    when NO_DATA_FOUND then
      raise_application_error( 
        scs.ScsDbExn, 'No text_id found for value='
		      || value || ' and enum_id=' || to_char(enum_id) ); 
  end getTID;

  function getVID(
    enum_id	in scs_enumerations.enum_id%TYPE,
    value	in scs_enum_values.value%TYPE    
  ) return scs_enum_values.val_id%TYPE
  is
    v_val_id scs_enum_values.val_id%TYPE;
  begin
    select val_id
      into v_val_id
      from scs_enum_values
     where enum_id = getVID.enum_id
       and value = getVID.value;

    return v_val_id;
  exception
    when NO_DATA_FOUND then
      return null;
  end getVID;


  function getVID(
    name	in scs_enumerations.name%TYPE,
    value	in scs_enum_values.value%TYPE    
  ) return scs_enum_values.val_id%TYPE
  is
    v_val_id	scs_enum_values.val_id%TYPE;
    v_enum_id	scs_enumerations.enum_id%TYPE;
  begin
    v_enum_id := getEnumId( name => getVID.name );

    select val_id
      into v_val_id
      from scs_enum_values
     where enum_id = v_enum_id
       and value = getVID.value;

    return v_val_id;
  exception
    when NO_DATA_FOUND then
      return null;
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

    return text_id; 

  exception
    when NO_DATA_FOUND then
      raise_application_error( 
        scs.ScsDbExn, 'No text_id found for val_id=' || to_char(val_id) );
  end getTID;

  procedure swapOrdering(
    val_id1	in scs_enum_values.val_id%TYPE,
    val_id2	in scs_enum_values.val_id%TYPE
  )
  is 
    ordering1			integer;
    enum_id1			integer;
    ordering2			integer;
    enum_id2			integer;
    not_in_same_enumeration	exception;
  begin
    select enum_id, ordering into enum_id1, ordering1 
      from scs_enum_values 
     where val_id = val_id1;

    select enum_id, ordering into enum_id2, ordering2 
      from scs_enum_values 
     where val_id = val_id2;

    if enum_id1 <> enum_id2 then
      raise not_in_same_enumeration;
    end if;
    
    -- NB (knp) this is the nice way to do it: 
    --   update scs_enum_values
    --   set ordering = 
    --     case when val_id = swapOrdering.val_id1 then swapOrdering.ordering2
    --          when val_id = swapOrdering.val_id2 then swapOrdering.ordering1
    --     end 
    --   where val_id in (val_id1, val_id2);
    --
    -- Unfortunately it only works from sqlplus (without variables) 
    -- and cannot be parsed as a procedure by the plsql parser:

    update scs_enum_values
    set ordering = -1
    where val_id = val_id1;

    update scs_enum_values
    set ordering = ordering1
    where val_id = val_id2;

    update scs_enum_values
    set ordering = ordering2
    where val_id = val_id1;
  exception
    when NO_DATA_FOUND then
      raise_application_error( 
        scs.ScsDbExn, 'scs_enumeration.swapOrdering: illegal val_ids ' 
		      || val_id1 || ',' || val_id2 );
    when not_in_same_enumeration then
      raise_application_error( 
        scs.ScsDbExn, 'scs_enumeration.swapOrdering: val_ids ' || val_id1 || 
		      ',' || val_id2 || 'must belong to same the enumeration');
  end swapOrdering;

  function vidToString (
    val_id      in scs_enum_values.val_id%TYPE,
    language    in scs_lang.language%TYPE
  ) return varchar2
  is
    v_text varchar2(200);
  begin
    select scs_text.getText(getTID(vidToString.val_id),vidToString.language)
      into v_text
      from dual;
     
    return v_text;
  exception
    when others then
      raise_application_error( scs.ScsDbExn, 
        'scs_enumeration.vidToString(' || to_char(val_id) || ')'); 
  end vidToString;

end scs_enumeration;
/
show errors

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
  scs_text.destroy(:old.text_id);
end scs_enum_values_before_del_tr;
/
show errors;
