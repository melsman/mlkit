-- All phrases gets its own number, phrase_id. One phase may 
-- be translated into several languages.
drop table smls_dict;
create table smls_dict (
  dict_id integer primary key,
  phrase_id integer not null,
  lang varchar(10) not null check (lang in ('English','Danish')),
  text varchar(3000) not null,
  unique (phrase_id,lang),
  unique (lang,text));

drop sequence dict_seq;
create sequence dict_seq start with 20;
drop sequence phrase_seq;
create sequence phrase_seq start with 20;

-- Necessary for the email-example
insert into smls_dict (dict_id, phrase_id, lang, text) values (1,1,'English','Type your email');
insert into smls_dict (dict_id, phrase_id, lang, text) values (4,1,'Danish','Indtast din mail');
insert into smls_dict (dict_id, phrase_id, lang, text) values (2,2,'English','Type subject');
insert into smls_dict (dict_id, phrase_id, lang, text) values (5,2,'Danish','Indtast emne');
insert into smls_dict (dict_id, phrase_id, lang, text) values (3,3,'English','Type body');
insert into smls_dict (dict_id, phrase_id, lang, text) values (6,3,'Danish','Indtast besked');
insert into smls_dict (dict_id, phrase_id, lang, text) values (7,4,'English','Send Message');
insert into smls_dict (dict_id, phrase_id, lang, text) values (8,4,'Danish','Send Besked');
insert into smls_dict (dict_id, phrase_id, lang, text) values (9,5,'English','Logout');
insert into smls_dict (dict_id, phrase_id, lang, text) values (10,5,'Danish','Log Ud');
insert into smls_dict (dict_id, phrase_id, lang, text) values (11,6,'English','authentication example');
insert into smls_dict (dict_id, phrase_id, lang, text) values (12,6,'Danish','valideringseksemplet');
insert into smls_dict (dict_id, phrase_id, lang, text) values (13,7,'English','SMLserver Home Page');
insert into smls_dict (dict_id, phrase_id, lang, text) values (14,7,'Danish','SMLserver Start Side');
insert into smls_dict (dict_id, phrase_id, lang, text) values (15,8,'English','"Email Sent"');
insert into smls_dict (dict_id, phrase_id, lang, text) values (16,8,'Danish','Email Sendt');
insert into smls_dict (dict_id, phrase_id, lang, text) values (17,9,'English','Your email has been sent');
insert into smls_dict (dict_id, phrase_id, lang, text) values (18,9,'Danish','Din email er sendt');
insert into smls_dict (dict_id, phrase_id, lang, text) values (19,10,'English','Thank You');
insert into smls_dict (dict_id, phrase_id, lang, text) values (20,10,'Danish','Mange Tak');

commit;
column text format a30;
