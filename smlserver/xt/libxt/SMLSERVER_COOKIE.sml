signature SMLSERVER_COOKIE = sig
  exception CookieError of string
  type cookiedata = {name   : string, 
		     value  : string, 
		     expiry : Date.date option, 
		     domain : string option, 
		     path   : string option, 
		     secure : bool}
  val allCookies     : unit -> (string * string) list
  val getCookie      : string -> (string * string) option
  val getCookieValue : string -> string option
  val setCookie      : cookiedata -> string
  val setCookies     : cookiedata list -> string
  val deleteCookie   : {name: string, path: string option} 
                       -> string
end

(*
 [CookieError s] exception raised on error with message s.

 [cookiedata] type of cookie.

 [allCookies()] returns a list [(n1,v1), (n2,v2), ..., 
 (nm,vm)] of all the name=value pairs of defined cookies.

 [getCookie cn] returns SOME(value) where value is the 
 'cn=value' string for the cookie cn, if any; otherwise 
 returns NONE.

 [getCookieValue cn] returns SOME(v) where v is the value 
 associated with the cookie cn, if any; otherwise returns 
 NONE.

 [setCookie {name,value,expiry,domain,path,secure}] returns 
 a string which (when transmitted to a browser as part of 
 the HTTP response header) sets a cookie with the given name, 
 value, expiry date, domain, path, and security level.

 [setCookies ckds] returns a string which (when transmitted 
 to a browser as part of the HTTP response header) sets the 
 specified cookies.

 [deleteCookie {name,path}] returns a string that (when 
 transmitted to a browser as part of the HTTP response 
 header) deletes the specified cookie by setting its expiry 
 to some time in the past.
*)
