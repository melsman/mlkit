drop sequence person_seq;

create sequence person_seq start with 3;

create table person (
  person_id int primary key,
  password varchar(100) not null,
  email varchar(20) unique not null,
  name varchar(100) not null,
  url varchar(200)
);

insert into person (person_id, password, email, name, url) 
values (1, 'Martin', 'mael@it.edu', 'Martin Elsman',
        'http://www.dina.kvl.dk/~mael');

insert into person (person_id, password, email, name, url) 
values (2, 'Niels', 'nh@it.edu', 'Niels Hallenberg',
       'http://www.it.edu/~nh');
