create table scs_curriculums(
  curriculum_id integer
    constraint scs_cur_curriculum_id_pk primary key
    constraint scs_cur_curriculum_id_nn not null,
  curriculum_name_tid integer
    constraint scs_cur_curriculum_name_tid_nn not null,
  constraint scs_cur_curriculum_name_tid_fk foreign key(curriculum_name_tid) references scs_texts(text_id),
  abbreviation varchar2(30)
    constraint scs_cur_abbreviation_nn not null
    constraint scs_cur_abbreviation_un unique,
  board_of_studies_id integer
    constraint scs_cur_board_of_studies_id_nn not null,
  constraint scs_cur_board_of_studies_id_fk foreign key (board_of_studies_id) references scs_boards_of_studies(board_of_studies_id)
);