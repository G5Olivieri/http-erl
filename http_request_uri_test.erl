-module(http_request_uri_test).
-include_lib("eunit/include/eunit.hrl").

%% Request-URI    = "*" | absoluteURI | abs_path | authority
request_asterisk_test_() ->
  ?_assertEqual(<<"*">>, http_request:request_uri("OPTIONS * HTTP/1.1\r\n")).
