create table scs_boards_of_studies(
  board_of_studies_id integer
    constraint b_o_s_board_of_studies_id_nn not null
    constraint b_o_s_board_of_studies_id_pk primary key,
  head_of_studies_id integer
    constraint b_o_s_head_of_studies_id_nn not null,
  constraint b_o_s_head_of_studies_id_fk foreign key (head_of_studies_id) references scs_persons(person_id),
  board_of_studies_name_tid integer
    constraint b_o_s_board_of_s_name_tid_nn not null, 
  abbreviation varchar2(30)
    constraint b_o_s_abbreviation_nn not null
    constraint b_o_s_abbreviation_un unique,
  credit_approval char(1)
    constraint b_o_s_credit_approval_ck check ( credit_approval in ('t','f') ),
  members integer
    constraint b_o_s_members_nn not null
    constraint b_o_s_members_un unique,
  constraint b_o_s_members_fk foreign key (members) references scs_groups(grp_id)
);


