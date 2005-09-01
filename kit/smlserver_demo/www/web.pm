import ../web_lib/lib.pm
in
  local
../web_sys/begin.sml
       ../web_demo_lib/Page.sml
       ../web_demo_lib/FormVar.sml
       ../web_demo_lib/Auth.sml
       ../web_demo_lib/RatingUtil.sml
../web_sys/end.sml
  in 
    [
	../web_sys/init.sml

    web/cookie_delete.sml
    web/cookie_set.sml
    web/cookie.sml
    web/counter.sml
    web/server.sml
    web/testRedirect.sml
    web/testsendfile.sml
    web/exchange.sml
    web/testinternalredirect.sml
    web/test.sml
    web/formvar.sml
    web/formvar_chk.sml
    web/guess.sml
    web/log_time.sml
    web/recipe.sml
    web/regexp.sml
    web/temp.sml
    web/time_of_day.sml
    web/index.sml
    web/return_file.sml
    web/hello.msp
    web/calendar.msp
    web/mul.msp
(*    web/mail.sml *)
    web/mail_form.sml
    web/test.msp
    web/encode.sml

    web/currency_cache.sml

    web/cache.sml
    web/cache_add.sml
    web/cache_lookup.sml
    web/cache_flush.sml
    web/cache_fib.sml
    web/cache_add_list.sml
    web/cache_lookup_list.sml
    web/cache_add_triple.sml
    web/cache_lookup_triple.sml


    web/upload/upload_form.sml
    web/upload/upload.sml
    web/upload/return_file.sml

    web/dnsmx.sml

    web/lowmail.sml
  (*  web/lmail.sml *)
   
    web/secret/server.sml

    web/schedule.sml

    web/guest.sml
    web/guest_add.sml

     web/employee/index.sml
     web/employee/update.sml
     web/employee/search.sml

     web/link/index.sml
     web/link/add_form.sml
     web/link/add.sml
     web/link/delete.sml
    
     web/db_test.sml
     web/db_test1.sml

     web/auth_form.sml
     web/auth_logout.sml
     web/auth.sml
     web/auth_new_form.sml
     web/auth_new.sml
     web/auth_send_form.sml
     web/auth_send.sml

     web/rating/index.sml
     web/rating/add.sml
     web/rating/add0.sml
     web/rating/wine.sml

    web/pwcheck.sml

   ] 
  end
end
