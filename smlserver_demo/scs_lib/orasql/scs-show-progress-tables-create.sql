create table scs_sp_content (
  sp_id integer
    constraint scs_sp_c_sp_id_nn not null
    constraint scs_sp_c_sp_id_pk primary key,
  user_id integer
    constraint scs_sp_c_user_id_nn not null
    constraint scs_sp_c_user_id_fk references scs_persons(person_id),
  status varchar(100)
    constraint scs_sp_c_status_ck 
      check (status in ('computing', 'done_show_content', 
                        'done_redirect','timed_out')),
  start_time date 
    constraint scs_sp_c_start_time_nn not null,
  time_out_sec integer
    constraint scs_sp_c_time_out_sec_nn not null,
  content_head varchar(4000),
  content_body varchar(4000)
    constraint scs_sp_c_content_b_nn not null,
  redirect_url_on_success varchar(4000),
  redirect_url_on_time_out varchar(4000)
    constraint scs_sp_c_redirect_time_out_nn not null,
  refresh_rate_sec integer
    constraint scs_sp_c_refresh_rate_sec_nn not null);
