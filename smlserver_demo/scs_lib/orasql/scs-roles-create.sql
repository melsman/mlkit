create table scs_roles(
  role_id integer
    constraint scs_roles_role_id_nn not null
    constraint scs_roles_role_id_pk primary key,
  abbreviation varchar2(30)
    constraint scs_roles_abbreviation_nn not null
    constraint scs_roles_abbreviation_un unique,
  role_description_tid integer
    constraint scs_roles_role_desc_tid_nn not null,
  constraint scs_roles_role_desc_tid_fk foreign key (role_description_tid) references scs_texts(text_id)
);

create table scs_role_rels(
  role_id integer
    constraint scs_role_rels_role_id_nn not null,
  constraint scs_role_rels_role_id_fk foreign key(role_id) references scs_roles(role_id),
  party_id integer
    constraint scs_role_rels_party_id_nn not null,
  constraint scs_role_rels_party_id_fk foreign key(party_id) references scs_parties(party_id)
);