create table scs_teachers(
  teacher_id integer
    constraint scs_teach_teacher_id_nn not null
    constraint scs_teach_teacher_id_pk primary key,
  constraint scs_teach_teacher_id_fk foreign key(teacher_id) references scs_persons(person_id),
  research_experience char(1)
    constraint scs_teach_research_exp_nn not null
    constraint scs_teach_research_exp_ck check ( research_experience in ('t','f') )
);

create table scs_topics(
  topic_id integer
    constraint scs_topics_topic_id_nn not null
    constraint scs_topics_topic_id_pk primary key,
  topic_text_id integer
);

create table scs_levels(
  level_id integer
    constraint scs_levels_level_id_nn not null
    constraint scs_levels_level_id_pk primary key,
  level_text_id integer
);


create table scs_teacher_topic_rel(
  teacher_id integer
    constraint scs_teach_top_r_teach_id_nn not null,
  constraint scs_teach_top_r_teach_id_fk foreign key(teacher_id) references scs_teachers(teacher_id),
  topic_id integer
    constraint scs_teach_top_r_topic_id_nn not null,
  constraint scs_teach_top_r_topic_id_fk foreign key(topic_id) references scs_topics(topic_id),
  level_id integer
    constraint scs_teach_top_r_level_id_nn not null,
  constraint scs_teach_top_r_level_id_fk foreign key(level_id) references scs_levels(level_id)
);


