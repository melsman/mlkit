drop package scs;
drop sequence scs_object_id_seq;

@scs-groups-drop.sql
@scs-users-drop.sql
@scs-persons-drop.sql
@scs-roles-drop.sql
@scs-parties-drop.sql
@scs-enumerations-drop.sql
@scs-locales-drop.sql
@scs-logs-drop.sql

select table_name from user_tables order by table_name;