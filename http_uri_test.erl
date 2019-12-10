-module(http_uri_test).
-include_lib("eunit/include/eunit.hrl").

parse_http_scheme_test_() ->
  [
   ?_assertEqual({scheme, <<"http">>}, http_uri:parse_scheme("http://google.com")),
   ?_assertEqual({scheme, <<"http">>}, http_uri:parse_scheme("google.com"))
  ].

parse_http_host_test_() ->
  [
    ?_assertEqual({host, <<"google.com">>}, http_uri:parse_host("http://google.com")),
    ?_assertEqual({host, <<"google.com">>}, http_uri:parse_host("http://google.com?path")),
    ?_assertEqual({host, <<"google.com">>}, http_uri:parse_host("http://google.com/path?query")),
    ?_assertEqual({host, <<"google.com">>}, http_uri:parse_host("http://google.com:80/path?query")),
    ?_assertEqual({host, <<"google.com">>},
                  http_uri:parse_host("http://username@google.com:8080/path?query")),
    ?_assertEqual({host, <<"google.com">>}, http_uri:parse_host("google.com")),
    ?_assertEqual({host, <<"google.com">>},
                  http_uri:parse_host("http://username@google.com:8080?path/query"))
  ].

parse_http_path_test_() ->
  [
   ?_assertEqual({path, <<"/">>}, http_uri:parse_path("http://google.com")),
   ?_assertEqual({path, <<"/">>}, http_uri:parse_path("google.com")),
   ?_assertEqual({path, <<"/">>}, http_uri:parse_path("http://google.com/")),
   ?_assertEqual({path, <<"/path">>}, http_uri:parse_path("http://google.com/path")),
   ?_assertEqual({path, <<"/path">>}, http_uri:parse_path("http://google.com/path?query")),
   ?_assertEqual({path, <<"/">>}, http_uri:parse_path("http://google.com?path/query")),
   ?_assertEqual({path, <<"/path/query">>}, http_uri:parse_path("http://google.com/path/query")),
   ?_assertEqual({path, <<"/path/query/fragment">>}, http_uri:parse_path("http://google.com/path/query/fragment?query"))
  ].

parse_http_query_test_() ->
  [ ].
