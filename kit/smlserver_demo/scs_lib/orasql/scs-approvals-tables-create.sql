-- $Id$

create table scs_approvals(
  approval_id integer
    constraint ucs_approvals_approval_id_nn not null
    constraint ucs_approvals_approval_id_pk primary key,
  on_what_table varchar2(30),
  on_what_id integer,
  party_id integer
    constraint ucs_approvals_party_id_nn not null
    constraint ucs_approvals_party_id_fk references scs_parties(party_id),
  decision char(1)
    constraint ucs_approvals_decision_nn not null
    constraint ucs_approvals_decision_ck check ( decision in ('t','f') ),
  created_on date default sysdate
    constraint ucs_approvals_date_nn not null,
  note_text varchar(4000)
);


