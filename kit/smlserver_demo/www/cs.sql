drop sequence cs_seq;
create sequence cs_seq start 2; -- PostgreSQL
create sequence cs_seq start with 2; -- Oracle

drop table cs;
create table cs (
  id integer primary key,
  text varchar(200) not null
);

insert into cs (id, text) values (1, 'æøåÅØÆüÃê');
commit;
