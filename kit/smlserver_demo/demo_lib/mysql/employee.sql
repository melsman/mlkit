-- Load datamodel:
--   mysql dbname -u dbuser < employee.mysql

  drop table if exists employee;

  create table employee (
    email          varchar(200) primary key not null,
    name           varchar(200) not null,
    passwd         varchar(200) not null,
    note           text,
    last_modified  date   
  );

  insert into employee (name, email, passwd)
  values ('Martin Elsman', 'mael@it.edu', 'don''t-forget');

  insert into employee (email, name, passwd, note)
  values ('nh@it.edu', 'Niels Hallenberg', 'hi', 'meeting');
