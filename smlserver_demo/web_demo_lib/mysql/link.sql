-- Load datamodel:
--   mysql dbname -u dbuser < link.sql

-- We do not use a sequence separately in this
-- example - we only insert into table link
-- each time a new link_id is created, hense,
-- we do not create a link_seq table as used in
-- file add.sml
drop table if exists link;

create table link (
  link_id int primary key auto_increment,
  person_id int not null,
  url varchar(200) not null,
  text varchar(200)
);

insert into link (link_id, person_id, url, text)
values (null, 1, 'http://www.smlserver.org', 'The SMLserver web-site');
