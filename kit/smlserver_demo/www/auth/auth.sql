drop table auth_user;

create table auth_user (
  user_id int primary key,
  password varchar(100) not null,
  login varchar(20) unique not null,
  name varchar(100) not null
);

insert into auth_user (user_id, password, login, name) values (1, 'Martin', 'mail', 'Martin Elsman');
insert into auth_user (user_id, password, login, name) values (2, 'Niels', 'nh', 'Niels Hallenberg');


commit;

-- For sqlplus
-- column password format a20;
-- column name format a20;
-- column login format a20;
