create table scs_enumerations(
  enum_id integer
    constraint scs_enumerations_enum_id_nn not null
    constraint scs_enumerations_enum_id_pk primary key,
  name varchar2(30)
    constraint scs_enumerations_name_nn not null
    constraint scs_enumerations_name_un unique
);

create table scs_enum_values(
  val_id integer
    constraint scs_enum_values_val_id_nn not null
    constraint scs_enum_values_val_id_pk primary key,
  enum_id integer
    constraint scs_enum_values_enum_id_nn not null,
  constraint scs_enum_values_enum_id_fk foreign key(enum_id) references scs_enumerations(enum_id),
  text_tid integer,
  constraint scs_enum_values_text_tid_fk foreign key(text_tid) references scs_texts(text_id),
  value integer
);