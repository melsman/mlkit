/* 
Patch 004 for scs_persons

Date:   2004-10-28
Author: Niels Hallenberg
Purpose: 

* Removing constraint scs_person_rels_un, see file
  scs-persons-tables-create.sql

  Se scs_users.html for an explanation of why we do not maintain this
  constraint anymore.

*/

alter table scs_person_rels drop
  constraint scs_person_rels_un;

