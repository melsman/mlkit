-- $Id$



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

