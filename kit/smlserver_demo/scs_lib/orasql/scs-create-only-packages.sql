-- $Id$


------------------------------------
-- LOAD THE ENTIRE SCS DATA MODEL --
------------------------------------
@scs-packages-create.sql
@scs-math-packages-create.sql;
@scs-test-packages-create.sql;

@scs-logs-packages-create.sql;

@scs-locales-packages-create.sql;

@scs-enumerations-packages-create.sql;

@scs-parties-packages-create.sql;

@scs-persons-packages-create.sql;

@scs-users-packages-create.sql;

@scs-roles-packages-create.sql;
@scs-groups-packages-create.sql;

@fbw.sql
@fast_md5.sql






--@scs-dict-create.sql;

select table_name from user_tables; -- debugging information
set serveroutput on

