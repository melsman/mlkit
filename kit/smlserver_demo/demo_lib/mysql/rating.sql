-- Load datamodel:
--   mysql dbname -u dbuser < rating.mysql

drop table if exists rating;
drop table if exists wine;
drop table if exists wid_sequence;

create table wid_sequence (
  seqId integer primary key auto_increment
);

create table wine (
       wid       integer primary key,
       name      varchar(100) not null,
       year      integer,
       unique    ( name, year )
);

create table rating (
       wid       integer not null,
       comments  text,
       fullname  varchar(100),
       email     varchar(100),
       rating    integer
);


