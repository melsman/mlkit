-- Log on printed documents using ScsPrint

drop table scs_print_log;
drop sequence scs_print_id_seq;
create sequence scs_print_id_seq;
drop sequence scs_print_batch_id_seq;
create sequence scs_print_batch_id_seq;
create table scs_print_log (
  batch_id integer not null,
  print_id integer primary key,
  user_id integer references auth_user(user_id),
  on_what_table varchar(100),
  on_what_id int,
  category varchar(100),
  clob_id integer not null,
  print_cmd varchar(1000),
  target_file varchar(200),
  doc_type varchar(20),
  note varchar(4000),
  time_stamp date,
  deleted_p char(1) check(deleted_p in ('t','f')));

create index scs_print_log_on_what_idx on scs_print_log (on_what_table,on_what_id);

create or replace view scs_print_log_active as
 select * from scs_print_log 
  where deleted_p = 'f';
