--drop table db_clob;
--drop sequence db_clob_id_seq;
create sequence db_clob_id_seq;
create table db_clob (
  clob_id integer,
  idx integer not null,
  text varchar(4000),
  primary key (clob_id,idx));
