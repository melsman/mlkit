-- $Id$

-- This code i a down-sized and modified version of the persons module
-- found in openACS version 4.5b1 (www.openacs.org): file
-- community-core-create.sql.

/* ========================================
   table scs_persons
======================================== */
create table scs_persons (
  person_id integer
    constraint scs_persons_person_id_nn not null
    constraint scs_persons_person_id_fk references scs_parties(party_id)
    constraint scs_persons_pk primary key,
  first_names varchar2(100) 
    constraint scs_persons_first_names_nn not null,
  last_name varchar2(100)
    constraint scs_persons_last_name_nn  not null,
  norm_name varchar2(200)
    constraint scs_persons_norm_name_nn not null,
  security_id varchar2(50),
  last_modified date default sysdate 
    constraint scs_persons_last_modified_nn not null,
  modifying_user integer,
  deleted_p char(1) 
    constraint scs_persons_deleted_p_nn not null
    constraint scs_persons_deleted_p_ck check (deleted_p in ('t','f'))
);

/* ========================================
   table scs_person_rels

   relates entries in scs_persons to entries in external data sources
   (eg. hsas, personal roster at it-c)

   We enforce that an external row may only relate to one person!
======================================== */
create table scs_person_rels (
  person_id integer
    constraint scs_person_rels_person_id_nn not null
    constraint scs_person_rels_person_id_fk references scs_persons(person_id),
  on_what_table varchar(100)
    constraint scs_person_rels_on_w_tab_nn not null,
  on_which_id integer
    constraint scs_person_rels_on_w_id_nn not null,
  constraint scs_person_rels_pk primary key (on_what_table,on_which_id)
);

-----------
-- Views --
-----------
create or replace view scs_persons_active as
select * 
  from scs_persons
 where deleted_p = 'f';

