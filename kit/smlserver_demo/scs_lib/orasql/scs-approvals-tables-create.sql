-- $Id$

create table scs_approvals (
  approval_id integer
    constraint ucs_approvals_approval_id_nn not null
    constraint ucs_approvals_approval_id_pk primary key,
  on_what_table varchar2(30)
    constraint ucs_approvals_on_what_table_nn not null,
  on_which_id integer
    constraint ucs_approvals_on_which_id_nn not null,
  user_id integer
    constraint ucs_approvals_user_id_nn not null
    constraint ucs_approvals_user_id_fk references scs_users(user_id),
  decision char(1)
    constraint ucs_approvals_decision_nn not null
    constraint ucs_approvals_decision_ck check ( decision in ('t','f') ),
  note_text varchar(4000),
  created_on date default sysdate
    constraint ucs_approvals_created_on_nn not null,
  last_modified date default sysdate 
    constraint scs_approvals_last_modified_nn not null,
  modifying_user integer
);


