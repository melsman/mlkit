drop sequence user_seq;
create sequence user_seq start with 10;  -- Oracle
create sequence user_seq start 10;  -- PostgreSQL

drop table user_group;
drop table groups;
drop table users;
create table users (
  id integer primary key not null,
  name varchar(100) not null,
  email varchar(100) unique not null
);

drop sequence group_seq;
create sequence group_seq start with 10; -- Oracle
create sequence group_seq start 10; -- PostgreSQL

create table groups (
  id integer primary key not null,
  name varchar(100) not null
);

create table user_group (
  user_id integer references users(id),
  group_id integer references groups(id),
  unique(user_id,group_id)
);
  
insert into groups (id, name) values (1, 'VIP');
insert into groups (id, name) values (2, 'TAP');
insert into groups (id, name) values (3, 'Student');

insert into users (id, name, email) values (1, 'Hans', 'hans@school.edu');
insert into users (id, name, email) values (2, 'Grethe', 'grethe@school.edu');
insert into users (id, name, email) values (3, 'Viggo', 'viggo@school.edu');
insert into users (id, name, email) values (4, 'Susanne', 'sus@school.edu');
insert into users (id, name, email) values (5, 'Albert', 'albert@school.edu');

insert into user_group (user_id, group_id) values (1,1);
insert into user_group (user_id, group_id) values (2,2);
insert into user_group (user_id, group_id) values (3,3);
insert into user_group (user_id, group_id) values (4,1);
insert into user_group (user_id, group_id) values (5,2);
insert into user_group (user_id, group_id) values (1,3);
insert into user_group (user_id, group_id) values (2,1);
insert into user_group (user_id, group_id) values (3,2);
insert into user_group (user_id, group_id) values (4,3);
insert into user_group (user_id, group_id) values (5,1);

commit;  -- Oracle