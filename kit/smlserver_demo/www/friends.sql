drop table friends;

create table friends (
  name varchar(200), 
  email varchar(200), 
  phone varchar(20)
);

insert into friends (name, email, phone)
values ('Ken Friis Larsen', 'kfl@it.edu', 'not known');

insert into friends (name, email, phone)
values ('Niels Hallenberg', 'nh@it.edu', '26186892');

insert into friends (name, email, phone)
values ('Mads Tofte', 'tofte@it.edu', 'not known');

insert into friends (name, email, phone)
values ('Martin Elsman', 'mael@dina.kvl.dk', '26122212');

insert into friends (name, email, phone)
values ('Peter Sestoft', 'sestoft@dina.kvl.dk', 'not known');
