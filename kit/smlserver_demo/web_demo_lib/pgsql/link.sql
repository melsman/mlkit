drop table link;
drop sequence link_seq;

create table link (
  link_id int primary key,
  person_id int references person not null,
  url varchar(200) not null,
  text varchar(200)
);

insert into link (link_id, person_id, url, text)
values (1, 1, 'http://www.smlserver.org', 'The SMLserver web-site');

create sequence link_seq start 2;
