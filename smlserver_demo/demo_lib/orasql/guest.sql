create table guest (
  email    varchar(100) not null,
  name     varchar(100) not null,
  comment  varchar(2000) not null
);

insert into guest (email, name, comment)
values ('homer@simpsons.net', 
        'Homer Simpson', 
        'Quick, give me the number to 911!');
