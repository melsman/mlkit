-- $Id$

/* ========================================
   table scs_roles

   defines roles in the system
======================================== */
create table scs_roles(
  role_id integer
    constraint scs_roles_role_id_nn not null
    constraint scs_roles_role_id_pk primary key,
  abbreviation varchar2(30)
    constraint scs_roles_abbreviation_nn not null
    constraint scs_roles_abbreviation_un unique,
  role_description_tid integer
    constraint scs_roles_role_desc_tid_nn not null
    constraint scs_roles_role_desc_tid_fk references scs_texts(text_id)
);

/* ========================================
   table scs_role_rels

   defines (role,party) relations in the system
======================================== */
create table scs_role_rels(
  role_id integer
    constraint scs_role_rels_role_id_nn not null
    constraint scs_role_rels_role_id_fk references scs_roles(role_id),
  party_id integer
    constraint scs_role_rels_party_id_nn not null
    constraint scs_role_rels_party_id_fk references scs_parties(party_id),
  constraint scs_role_rels_un unique (role_id,party_id)
);
