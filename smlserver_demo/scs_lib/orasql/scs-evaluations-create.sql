create table scs_evaluation_questions(
  question_id integer
    constraint scs_eval_q_question_id_nn not null
    constraint scs_eval_q_question_id_pk primary key,
  answer_type varchar2(6)
    constraint scs_eval_q_question_id_ck check ( answer_type in ('text','number') ),
  question_text_id integer,
  constraint scs_eval_q_question_id_fk foreign key (question_text_id) references scs_texts(text_id)
);

create table scs_evaluation_answers(
  on_what_table varchar2(30),
  on_which_id integer,
  question_id integer,
  constraint scs_eval_ans_question_id_fk foreign key (question_id) references scs_evaluation_questions(question_id),
  answer_text_id integer,
  constraint scs_eval_ans_answer_text_id_fk foreign key (answer_text_id) references scs_texts(text_id),
  answer_number number
);