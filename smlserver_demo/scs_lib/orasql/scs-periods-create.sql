-- currently not used 2003-01-27, nh

create table scs_period_types(
  period_type_id integer
    constraint scs_period_t_period_type_id_nn not null
    constraint scs_period_t_period_type_id_pk primary key,
  period_type_tid integer,
  constraint scs_period_t_per_type_tid_fk foreign key (period_type_tid) references scs_texts(text_id) 
);

create table scs_periods(
  period_id integer
    constraint scs_periods_period_id_nn not null
    constraint scs_periods_period_id_pk primary key,
  period_type_id integer
    constraint scs_periods_period_type_id_nn not null,
  constraint scs_periods_period_type_id_fk foreign key(period_type_id) references scs_period_types(period_type_id),
  period_name_tid integer,
  abbreviation varchar(30)
    constraint scs_periods_abbreviation_un unique,
  start_date date
    constraint scs_periods_start_date_nn not null,
  end_date date
    constraint scs_periods_end_date_nn not null
);