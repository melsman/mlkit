drop sequence wid_sequence;
create sequence wid_sequence;

create table wine (
       wid       integer primary key,
       name      varchar(100) not null,
       year      integer,
       check     ( 0 <= year and year <= 3000 ),
       unique    ( name, year )
);

create table rating (
       wid       integer references wine,
       comments  varchar(1000),
       fullname  varchar(100),
       email     varchar(100),
       rating    integer,
       check     ( 0 <= rating and rating <= 6 )
);


