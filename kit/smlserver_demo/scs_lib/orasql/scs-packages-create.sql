-- $Id$

/* ======================================================================
   package scs

   History:
   141102 Kennie Nybo Pontoppidan <kennie@it-c.dk> Added comments
   091002 Niels Hallenberg <nh@it.edu> Created package
====================================================================== */
create or replace package scs
as
  -- Notice, this constant is also hard-coded in scs-test-create!
  ScsDbExn constant integer := -20000;

  /* -------------------
     function new_obj_id
     -------------------
     given an integer returns the integer
     given null returns a new id
  */
  function new_obj_id (
    obj_id in integer default null
  ) return integer;

  /* ---------------
     function swap_p
     ---------------
     given 't' returns 'f'
     given 'f' returns 't'

     throws a ScsDbExn exception for all other characters
  */
  function swap_p (
    p in char
  ) return char;

  /* -------------------------
     function invalidate_field
     -------------------------
     used in destroy methods in scs_party, scs_users

     returns id '-' || field
     field will be shortened 
     if id '-' || field is longer than field_length characters
  */
  function invalidate_field(
    field		in varchar,
    field_length	in integer,
    id			in integer
  ) return varchar2;
end scs;
/
show errors



/* ======================================================================
   package bodies start here
====================================================================== */
create or replace package body scs
as

  function new_obj_id (
    obj_id in integer default null
  ) return integer
  is
    v_obj_id integer;
  begin
    if obj_id is null then
      select scs_object_id_seq.nextval
        into v_obj_id
        from dual;
    else
      v_obj_id := scs.new_obj_id.obj_id;
    end if;
    return v_obj_id;
  end new_obj_id;

  function swap_p (
    p in char
  ) return char
  is
  begin
    if swap_p.p = 't' then
      return 'f';
    elsif swap_p.p = 'f' then
      return 't';
    else
      raise_application_error(ScsDbExn, 'scs.swap_p. Can''t swap ' || swap_p.p || '.');
    end if;
  end swap_p;

  function invalidate_field(
    field		in varchar,
    field_length	in integer,
    id			in integer
  ) return varchar2
  is
  begin
    return substr( to_char(id) || '-' || field, 1, field_length);
  exception
    when others then
      return null;
  end invalidate_field;

end scs;
/
show errors
