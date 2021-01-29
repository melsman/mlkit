(** OAuth2 authorization

This functionality supports the OAuth2 implicit flow where an access
token is possibly provided in the page location string after a
#-character.

The flow uses cookies to store a random state between the requests and
to avoid repeated logins once an access token is obtained.

Here is how the implicit flow works; see also

https://aaronparecki.com/oauth-2-simplified/#single-page-apps

1) Sign up the client (your app) with the resource provider
   (e.g. Dropbox) and get a client_id.

2) Identify the resource provider's authorize API endpoint.

3) Make your application initiate a client object.

4) When the page loads, call the function `token` to identify if the
   user is logged in already. If the user is not logged in, present a
   login button (otherwise present a logout button).

5) Connect the login button to an event that will call the `authorize`
   function and connect the logout button to an event that will call
   the `logout` function.

6) If the access token is obtained, the resource provider can be
   accessed by adding the header

    "Authorization: Bearer <access token>"

*)

signature OAUTH = sig
  type client
  type token = string

  val client : {authorize: string,         (* authorize url *)
                client_id: string,
                redirect_uri: string} -> client

  val getLocation : unit -> string         (* get window.location.href *)

  (* Implicit flow *)
  val authorize : client -> unit
  val token     : client -> token option
  val logout    : client -> unit
end

(**

[client c] creates a client object with the provided information.

[authorize c] initiate authorization process.

[token c] returns SOME t, where t is an access token if c is
authorized (if there is an access token cookie or if there is an
access_token value after a #-character in the window.location.href
string.)

[logout c] set window.location to the raw redirect_uri and delete the
cookies.

*)
