create table scs_approval_status(
  approval_status_id integer
    constraint scs_as_approval_status_id_nn not null
    constraint scs_as_approval_status_id_pk primary key,
  status_text_id integer
    constraint scs_as_status_text_id_nn not null,
  constraint scs_as_status_text_id_fk foreign key (status_text_id) references scs_texts(text_id)
);

create table scs_approvals(
  approval_id integer
    constraint scs_approvals_approval_id_nn not null
    constraint scs_approvals_approval_id_pk primary key,
  on_what_table varchar2(30),
  on_what_id integer,
  person_id integer
    constraint scs_approvals_person_id_nn not null,
  constraint scs_approvals_person_id_fk foreign key(person_id) references scs_persons(person_id),
  decision char(1)
    constraint scs_approvals_decision_nn not null
    constraint scs_approvals_decision_ck check ( decision in ('t','f') ),
  created_on date default sysdate
    constraint scs_approvals_date_nn not null,
  note_text_id integer,
  constraint scs_approvals_note_text_id_fk foreign key(note_text_id) references scs_texts(text_id)
);


