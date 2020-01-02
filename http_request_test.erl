-module(http_request_test).
-include_lib("eunit/include/eunit.hrl").
-include("http_request.hrl").

parse_http_request_line_test_() ->
  [
   ?_assertEqual(#http_request{
                    method = <<"GET">>,
                    uri = #http_uri{
                             scheme   = <<>>,
                             username = <<>>,
                             host     = <<>>,
                             port     = <<>>,
                             path     = <<"/">>,
                             query    = <<>>
                            },
                    version = <<"1.1">>
                   },
                 http_request:parse_request_line("GET / HTTP/1.1\n\r"))
  ].
