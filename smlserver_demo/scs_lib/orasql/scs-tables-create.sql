-- $Id$

-- this sequences is used by the function scs.new_obj_id
-- to create unique ids throughout the database
create sequence scs_object_id_seq start with 50000;


------------------------------------
-- LOAD THE ENTIRE SCS DATA MODEL --
------------------------------------
@scs-test-create.sql;
@scs-math-create.sql;
@scs-random-create.sql;
@scs-logs-create.sql;
@scs-locales-create.sql;
@scs-enumerations-create.sql;
@scs-parties-create.sql;
@scs-persons-create.sql;
@scs-users-create.sql;
@scs-groups-create.sql;
@scs-roles-create.sql;
@scs-default-users-create.sql;
@scs-dict-create.sql;


-- debugging information
select table_name from user_tables; 
set serveroutput on

