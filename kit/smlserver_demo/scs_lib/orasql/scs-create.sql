-- $Id$

create sequence scs_object_id_seq start with 10;

create or replace package scs
as

  -- Notice, this is also hard-coded in scs-test-create!
  ScsDbExn constant integer := -20000;

  function new_obj_id (
    obj_id in integer default null
  ) return integer;

end scs;
/
show errors

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

end scs;
/
show errors

------------------------------------
-- LOAD THE ENTIRE SCS DATA MODEL --
------------------------------------

@scs-math-create.sql;
@scs-random-create.sql;
@scs-logs-create.sql;
@scs-locales-create.sql;
@scs-enumerations-create.sql;
@scs-parties-create.sql;
@scs-roles-create.sql;
@scs-persons-create.sql;
@scs-users-create.sql;
@scs-groups-create.sql;

select table_name from user_tables; -- debugging information
set serveroutput on; -- used by test files

