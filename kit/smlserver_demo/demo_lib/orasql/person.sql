drop table person;

create table person (
  user_id int primary key,
  password varchar(100) not null,
  login varchar(20) unique not null,
  name varchar(100) not null
);

insert into person (user_id, password, login, name, lang) 
values (1, 'Martin', 'mael', 'Martin Elsman');

insert into person (user_id, password, login, name, lang) 
values (2, 'Niels', 'nh', 'Niels Hallenberg');
