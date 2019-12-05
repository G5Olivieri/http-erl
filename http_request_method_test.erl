-module(http_request_method_test).
-include_lib("eunit/include/eunit.hrl").

%% Request-Line   = Method SP Request-URI SP HTTP-Version CRLF
%% Method         = "OPTIONS"
%%                   | "GET"
%%                   | "HEAD"
%%                   | "POST"
%%                   | "PUT"
%%                   | "DELETE"
%%                   | "TRACE"
%%                   | "CONNECT"
%%                   | extension-method
%%    extension-method = token

method_connect_test_() ->
  ?_assert(http_request:method("CONNECT / HTTP/1.1\r\n") =:= <<"CONNECT">>).

method_delete_test_() ->
  ?_assert(http_request:method("DELETE / HTTP/1.1\r\n") =:= <<"DELETE">>).

method_get_test_() ->
  ?_assert(http_request:method("GET / HTTP/1.1\r\n") =:= <<"GET">>).

method_head_test_() ->
  ?_assert(http_request:method("HEAD / HTTP/1.1\r\n") =:= <<"HEAD">>).

method_options_test_() ->
  ?_assert(http_request:method("OPTIONS / HTTP/1.1\r\n") =:= <<"OPTIONS">>).

method_post_test_() ->
  ?_assert(http_request:method("POST / HTTP/1.1\r\n") =:= <<"POST">>).

method_put_test_() ->
  ?_assert(http_request:method("PUT / HTTP/1.1\r\n") =:= <<"PUT">>).

method_trace_test_() ->
  ?_assert(http_request:method("TRACE / HTTP/1.1\r\n") =:= <<"TRACE">>).

%% Custom method not allowed
method_custom_test_() ->
  ?_assert(http_request:method("CUSTOM / HTTP/1.1\r\n") =:= {error, <<"Method not found">>}).

invalid_request_line_test_() ->
  ?_assert(http_request:method("flkadjfldksaj") =:= {error, <<"Method not found">>}).
