signature NS_COOKIE =
  sig
    exception CookieError of string
    type cookiedata = {name   : string, 
		       value  : string, 
		       expiry : Date.date option, 
		       domain : string option, 
		       path   : string option, 
		       secure : bool}

    (* [allCookies()] returns a list [(n1,v1), (n2,v2), 
     * ..., (nm,vm)] of all the name=value pairs of 
     * defined cookies. *)
    val allCookies     : unit -> (string * string) list

    (* [getCookie cn] returns SOME(value) where value 
     * is the 'cn=value' string for the cookie cn, 
     * if any; otherwise returns NONE. *)
    val getCookie      : string -> (string * string) option

    (* [getCookieValue cn] returns SOME(v) where v is 
     * the value associated with the cookie cn, if any; 
     * otherwise returns NONE. *)
    val getCookieValue : string -> string option

    (* [setCookie {name,value,expiry,domain,path,secure}]
     * returns a string which (when transmitted to a 
     * browser as part of the HTTP response header) sets 
     * a cookie with the given name, value, expiry date, 
     * domain, path, and security level. *)
    val setCookie    : cookiedata -> string

    (* [setCookies ckds] returns a string which (when 
     * transmitted to a browser as part of the HTTP 
     * response header) sets the specified cookies. *)
    val setCookies   : cookiedata list -> string

    (* [deleteCookie {name,path}] returns a string that 
     * (when transmitted to a browser as part of the 
     * HTTP response header) deletes the specified 
     * cookie by setting its expiry to some time in
     * the past. *)
    val deleteCookie : {name: string, path: string option} 
                       -> string
  end
