-- $Id$


------------------------------------
-- LOAD THE ENTIRE SCS DATA MODEL --
------------------------------------

-- this sequences is used by the function scs.new_obj_id
-- to create unique ids throughout the database
create sequence scs_object_id_seq start with 50000;


@scs-packages-create.sql
@scs-math-packages-create.sql;
@scs-test-packages-create.sql;
@scs-logs-tables-create.sql;
@scs-logs-packages-create.sql;
@scs-locales-tables-create.sql;
@scs-locales-packages-create.sql;
@scs-enumerations-tables-create.sql;
@scs-enumerations-packages-create.sql;
@scs-parties-tables-create.sql;
@scs-parties-packages-create.sql;
@scs-persons-tables-create.sql;
@scs-persons-packages-create.sql;
@scs-users-tables-create.sql;
@scs-users-packages-create.sql;
@scs-users-initialdata-create.sql;
@scs-groups-tables-create.sql;

@scs-roles-tables-create.sql;
@scs-roles-packages-create.sql;
@scs-default-users-create.sql;

@scs-groups-packages-create.sql;
@scs-groups-initialdata-create.sql;






--@scs-dict-create.sql;

select table_name from user_tables; -- debugging information
set serveroutput on

