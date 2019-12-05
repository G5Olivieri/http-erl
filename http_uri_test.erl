-module(http_uri_test).
-include_lib("eunit/include/eunit.hrl").

parse_http_scheme_test_() ->
  [ ?_assert(http_uri:parse_scheme("http://google.com") =:= {scheme, <<"http">>}),
    ?_assert(http_uri:parse_scheme("google.com") =:= {scheme, <<"http">>})
  ].

parse_http_host_test_() ->
  [
    ?_assert(http_uri:parse_host("http://google.com") =:= {host, <<"google.com">>}),
    ?_assert(http_uri:parse_host("http://google.com?path") =:= {host, <<"google.com">>}),
    ?_assert(http_uri:parse_host("http://google.com/path?query") =:= {host, <<"google.com">>}),
    ?_assert(http_uri:parse_host("http://google.com:80/path?query") =:= {host, <<"google.com">>}),
    ?_assert(http_uri:parse_host("http://username@google.com:8080/path?query") =:= {host, <<"google.com">>})
  ].
