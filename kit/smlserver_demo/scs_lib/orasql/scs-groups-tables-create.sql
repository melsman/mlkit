-- $Id$

-- This code i a down-sized and modified version of the party module
-- found in openACS (www.openacs.org): files groups-create.sql and
-- groups-body-create.sql

-- TO-DO
-- 2002-09-16, nh: who is allowed to approve group membership?

create table scs_grp_types (
  grp_type_id integer
    constraint scs_grp_types_grp_type_id_nn not null
    constraint scs_grp_types_grp_type_id_pk primary key,
  grp_type varchar2(100) -- unique group type name identifier
    constraint scs_grp_types_grp_type_nn not null
    constraint scs_grp_types_grp_type_un unique,
  default_join_policy varchar2(30) default 'open' 
    constraint scs_grp_types_join_policy_nn not null
    constraint scs_grp_types_join_policy_ck 
      check (default_join_policy in ('open', 'needs approval', 'closed')),
  default_grp_type char(1) default 'f'
    constraint scs_grp_types_default_grp_nn not null
    constraint scs_grp_types_default_grp_ck 
      check (default_grp_type in ('t','f')),
  last_modified date default sysdate 
    constraint scs_grp_types_last_mod_nn not null,
  modifying_user integer
    constraint scs_grp_types_mod_user_fk references scs_users(user_id)
    constraint scs_grp_types_mod_user_nn not null
);

create table scs_groups (
  grp_id integer
    constraint scs_groups_grp_id_nn not null
    constraint scs_groups_grp_id_fk references scs_parties(party_id)
    constraint scs_groups_pk primary key,
  grp_type_id integer
    constraint scs_groups_grp_type_id_nn not null
    constraint scs_groups_grp_type_id_fk references scs_grp_types(grp_type_id),
  grp_name varchar2(100) 
    constraint scs_groups_grp_name_nn not null
    constraint scs_groups_grp_name_un unique,
  join_policy varchar2(30) default 'open' 
    constraint scs_groups_join_policy_nn not null
    constraint scs_groups_join_policy_ck
      check (join_policy in ('open', 'needs approval', 'closed')),
  last_modified date default sysdate 
    constraint scs_groups_last_mod_nn not null,
  modifying_user integer
    constraint scs_groups_mod_user_fk references scs_users(user_id)
    constraint scs_groups_mod_user_nn not null
);

create table scs_grp_composition_rels (
  rel_id integer
    constraint scs_grp_comp_rels_rel_id_pk primary key
    constraint scs_grp_comp_rels_rel_id_nn not null,
  grp_id_one integer
    constraint scs_grp_comp_rels_gid_one_fk references scs_groups(grp_id)
    constraint scs_grp_comp_rels_gid_one_nn not null,
  grp_id_two integer
    constraint scs_grp_comp_rels_gid_two_fk references scs_groups(grp_id)
    constraint scs_grp_comp_rels_gid_two_nn not null,
  last_modified date default sysdate 
    constraint scs_grp_comp_rels_last_mod_nn not null,
  modifying_user integer
    constraint scs_grp_comp_rels_mod_user_fk references scs_users(user_id)
    constraint scs_grp_comp_rels_mod_user_nn not null,
  constraint scs_grp_comp_rels_one_two_un unique(grp_id_one, grp_id_two)
);

create table scs_grp_member_rels (
  rel_id integer
    constraint scs_grp_member_rels_rel_id_pk primary key
    constraint scs_grp_member_rels_rel_id_nn not null,
  grp_id integer
    constraint scs_grp_member_rels_gid_fk references scs_groups(grp_id)
    constraint scs_grp_member_rels_gid_nn not null,
  party_id integer
    constraint scs_grp_member_rels_pid_fk references scs_parties(party_id)
    constraint scs_grp_member_rels_pid_nn not null,
  member_state varchar2(20) 
    constraint scs_grp_member_rels_mem_nn not null
    constraint scs_grp_member_rels_mem_ck
      check (member_state in ('approved', 'needs approval',
                              'banned', 'rejected', 'deleted')),
  last_modified date default sysdate 
    constraint scs_grp_member_rels_lmod_nn not null,
  modifying_user integer
    constraint scs_grp_member_rels_muser_fk references scs_users(user_id)
    constraint scs_grp_member_rels_muser_nn not null,
  constraint scs_grp_mem_rels_grp_party_un unique(grp_id,party_id)
);

------------------------------------------
-- DENORMALIZATION: group_element_index --
------------------------------------------

-- group_element_index is an internal mapping table maintained by
-- triggers on insert and delete on membership and composition
-- relationships. This is for optimizaiton of the views in the "VIEWS"
-- section further below.

-- Instead of writing a complicated trigger to keep this map up to
-- date when people edit membership or composition relationships, I
-- think I'm going to make it illegal to mutate membership or
-- composition relationships, or at least the grp_id and party_id
-- columns, since I don't know that it makes sense anyways. Also, by
-- making this constraint we can probably do some nifty optimizaitons
-- at some point in the future.

-- This means, you can't edit a membership or composition relation.
-- Instead, you have to delete the relation and recreate it. By doing
-- this, we only have "on insert" and "on delete" triggers and avoid
-- maintaining the more complex "on update" trigger"

create table scs_grp_party_index (
  rel_id integer
    constraint scs_grp_party_idx_rel_id_nn not null,
  grp_id integer
    constraint scs_grp_party_idx_nn not null
    constraint scs_grp_party_idx_grp_id_fk references scs_groups(grp_id),
  party_id integer
    constraint scs_grp_party_idx_party_id_nn not null
    constraint scs_grp_party_idx_party_id_fk references scs_parties(party_id),
  container_id	integer
    constraint scs_grp_party_idx_cont_id_nn not null
    constraint scs_grp_party_idx_cont_id_fk references scs_groups(grp_id),
  ancestor_rel_type varchar2(100) 
    constraint scs_grp_pty_idx_ancstr_typ_nn not null
    constraint scs_grp_pty_idx_ancstr_typ_ck
      check (ancestor_rel_type in ('composition_rel','membership_rel')),
  constraint scs_grp_party_index_pk primary key (rel_id, grp_id, party_id)
);

-----------
-- VIEWS --
-----------

create or replace view scs_grp_element_map
as select grp_id, party_id as element_id, container_id, rel_id, ancestor_rel_type
   from scs_grp_party_index;

create or replace view scs_grp_component_map
as select distinct grp_id, party_id as component_id, container_id, rel_id, ancestor_rel_type
     from scs_grp_party_index
    where ancestor_rel_type='composition_rel';

create or replace view scs_grp_member_map
as select distinct grp_id, party_id as member_id, container_id, rel_id, ancestor_rel_type
     from scs_grp_party_index
    where ancestor_rel_type='membership_rel';

create or replace view scs_grp_approved_member_map
as select distinct gm.grp_id, gm.member_id, gm.container_id
     from scs_grp_member_map gm, scs_grp_member_rels mr
   where gm.rel_id = mr.rel_id
     and mr.member_state = 'approved';

create or replace view scs_grp_distinct_member_map
as select distinct grp_id, member_id
     from scs_grp_member_map;

-- party_member_map can be used to expand any party into its members.  
-- Every party is considered to be a member of itself.

create or replace view scs_party_member_map
as select grp_id as party_id, member_id
     from scs_grp_distinct_member_map
    union
   select party_id, party_id as member_id
     from scs_parties;

create or replace view scs_party_approved_member_map
as select distinct grp_id as party_id, member_id
     from scs_grp_approved_member_map
    union
   select party_id, party_id as member_id
     from scs_parties;






