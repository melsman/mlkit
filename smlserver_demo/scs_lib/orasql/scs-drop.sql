drop package scs;
drop sequence scs_object_id_seq;

@scs-periods-drop.sql
@scs-evaluations-drop.sql
@scs-approvals-drop.sql

@scs-study-programmes-drop.sql
@scs-curriculums-drop.sql
@scs-boards-of-studies-drop.sql

@scs-groups-drop.sql
@scs-users-drop.sql
@scs-teachers-drop.sql
@scs-persons-drop.sql
@scs-roles-drop.sql
@scs-parties-drop.sql
@scs-locales-drop.sql
@scs-enumerations-drop.sql
@scs-texts-drop.sql
@scs-logs-drop.sql

select table_name from user_tables;