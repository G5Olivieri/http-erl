-module(http_request_uri_test).
-include_lib("eunit/include/eunit.hrl").

%% Request-URI    = "*" | absoluteURI | abs_path | authority
request_asterisk_test_() ->
  ?_assert(http_request:request_uri("OPTIONS * HTTP/1.1\r\n") =:= <<"*">>).

request_abs_uri_without_path_test_() ->
  ?_assert(http_request:request_uri("GET http://www.google.com HTTP/1.1\r\n") =:= {http_uri,
                                                                                   {scheme, "http", host, "www.google.com", path, "/", query, {}}}).
