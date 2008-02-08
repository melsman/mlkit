drop table guest;
drop sequence guest_seq;

create table guest (
  gid      integer primary key not null,
  email    varchar(100) not null,
  name     varchar(100) not null,
  comments varchar(2000) not null
);

insert into guest (gid, email, name, comments)
values (1,
        'homer@simpsons.net', 
        'Homer Simpson', 
        'Quick, give me the number to 911!');

create sequence guest_seq start 2;
