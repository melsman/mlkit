-- Catalog of Queries accessible through the ScsPrint interface

drop table scs_query;
drop sequence scs_query_id_seq;
create sequence scs_query_id_seq;
create table scs_query (
  id integer not null primary key,
  query varchar(4000) not null,
  arity integer not null,
  category varchar(100) not null,
  name varchar(100) not null,
  description varchar(4000),
  create_date date not null,
  create_user integer not null references auth_user(user_id)
);

create index scs_query_category_name_idx on scs_query (category,name);
