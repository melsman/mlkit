create table scs_study_programmes(
  study_programme_id integer
    constraint scs_study_prgs_st_pr_id_nn not null
    constraint scs_study_prgs_st_pr_id_pk primary key,
  study_programme_name_tid integer
  constraint scs_study_prgs_st_pr_name_id_fk foreign key(study_programme_name_tid) references scs_texts(text_id),
  abbreviation varchar2(30)
    constraint scs_study_prgs_abbreviation_un unique
)