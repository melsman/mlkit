--drop table auth_user;

create table auth_user (
  user_id int primary key,
  password varchar(100) not null,
  login varchar(20) unique not null,
  name varchar(100) not null,
  lang varchar(10) not null check (lang in ('Danish','English'))
);

--insert into auth_user (user_id, password, login, name, lang) values (1, 'Martin', 'mael', 'Martin Elsman','English');
--insert into auth_user (user_id, password, login, name, lang) values (2, 'Niels', 'nh', 'Niels Hallenberg','Danish');



commit;

-- For sqlplus
-- column password format a20;
-- column name format a20;
-- column login format a20;
