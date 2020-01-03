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
                 http_request:parse_request_line("GET / HTTP/1.1\n\r")),
   ?_assertEqual(#http_request{
                    method = <<"POST">>,
                    uri = #http_uri{
                             scheme   = <<>>,
                             username = <<>>,
                             host     = <<>>,
                             port     = <<>>,
                             path     = <<"/path">>,
                             query    = <<>>
                            },
                    version = <<"1.1">>
                   },
                 http_request:parse_request_line("post /path HTTP/1.1\n\r")),
   ?_assertEqual(#http_request{
                    method = <<"PUT">>,
                    uri = #http_uri{
                             scheme   = <<>>,
                             username = <<>>,
                             host     = <<>>,
                             port     = <<>>,
                             path     = <<"/path">>,
                             query    = <<>>
                            },
                    version = <<"1.2">>
                   },
                 http_request:parse_request_line("PUt /path HTTP/1.2\n\r")),
   ?_assertEqual(#http_request{
                    method = <<"PUT">>,
                    uri = #http_uri{
                             scheme   = <<>>,
                             username = <<>>,
                             host     = <<>>,
                             port     = <<>>,
                             path     = <<"/path">>,
                             query    = <<>>
                            },
                    version = <<"1.2">>
                   },
                 http_request:parse_request_line("PUt /path  HTTP/1.2\n\r")),
   ?_assertEqual(#http_request{
                    method = <<"PUT">>,
                    uri = #http_uri{
                             scheme   = <<>>,
                             username = <<>>,
                             host     = <<>>,
                             port     = <<>>,
                             path     = <<"/path">>,
                             query    = <<>>
                            },
                    version = <<"v1.2">>
                   },
                 http_request:parse_request_line("PUt /path  HTTP/v1.2\n\r")),
   ?_assertEqual({error, "Invalid request line"},
                 http_request:parse_request_line("PUt /path cHTTP/1.1\n\r")),
   ?_assertEqual({error, "Invalid request line"},
                 http_request:parse_request_line("PUt HTTP/1.1\n\r")),
   ?_assertEqual({error, "Invalid request line"},
                 http_request:parse_request_line("PUt / 1.1\n\r")),
   ?_assertEqual({error, "Invalid request line"},
                 http_request:parse_request_line("PUt\n\r")),
   ?_assertEqual({error, "Invalid request line"},
                 http_request:parse_request_line("\n\r"))
  ].
