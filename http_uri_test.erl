-module(http_uri_test).
-include_lib("eunit/include/eunit.hrl").
-include("http_uri.hrl").

parse_http_uri_test_() ->
  [
   ?_assertEqual(
      #http_uri{
         scheme   = <<"http">>,
         username = <<"">>,
         host     = <<"google.com">>,
         port     = <<"80">>,
         path     = <<"/">>,
         query    = <<"">>
        },
      http_uri:parse("google.com")),
   ?_assertEqual(
      #http_uri{
         scheme   = <<"http">>,
         username = <<"">>,
         host     = <<"google.com">>,
         port     = <<"80">>,
         path     = <<"/">>,
         query    = <<"">>
        },
      http_uri:parse("http://google.com")),
   ?_assertEqual(
      #http_uri{
         scheme   = <<"http">>,
         username = <<"glayson">>,
         host     = <<"google.com">>,
         port     = <<"80">>,
         path     = <<"/">>,
         query    = <<"">>
        },
      http_uri:parse("glayson@google.com")),
   ?_assertEqual(
      #http_uri{
         scheme   = <<"http">>,
         username = <<"">>,
         host     = <<"google.com">>,
         port     = <<"8080">>,
         path     = <<"/">>,
         query    = <<"">>
        },
      http_uri:parse("google.com:8080")),
   ?_assertEqual(
      #http_uri{
         scheme   = <<"http">>,
         username = <<"">>,
         host     = <<"google.com">>,
         port     = <<"80">>,
         path     = <<"/">>,
         query    = <<"">>
        },
      http_uri:parse("google.com/")),
   ?_assertEqual(
      #http_uri{
         scheme   = <<"http">>,
         username = <<"">>,
         host     = <<"google.com">>,
         port     = <<"80">>,
         path     = <<"/path">>,
         query    = <<"">>
        },
      http_uri:parse("google.com/path")),
   ?_assertEqual(
      #http_uri{
         scheme   = <<"http">>,
         username = <<"">>,
         host     = <<"google.com">>,
         port     = <<"80">>,
         path     = <<"/path">>,
         query    = <<"query">>
        },
      http_uri:parse("google.com/path?query")),
   ?_assertEqual(
      #http_uri{
         scheme   = <<"http">>,
         username = <<"">>,
         host     = <<"google.com">>,
         port     = <<"80">>,
         path     = <<"/">>,
         query    = <<"query">>
        },
      http_uri:parse("google.com?query")),
   ?_assertEqual(
      #http_uri{
         scheme   = <<"http">>,
         username = <<"">>,
         host     = <<"google.com">>,
         port     = <<"80">>,
         path     = <<"/">>,
         query    = <<"query">>
        },
      http_uri:parse("google.com/?query")),
   ?_assertEqual(
      #http_uri{
         scheme   = <<"http">>,
         username = <<"">>,
         host     = <<"google.com">>,
         port     = <<"80">>,
         path     = <<"/">>,
         query    = <<"">>
        },
      http_uri:parse("google.com/?")),
   ?_assertEqual(
      #http_uri{
         scheme   = <<"http">>,
         username = <<"">>,
         host     = <<"google.com">>,
         port     = <<"80">>,
         path     = <<"/">>,
         query    = <<"">>
        },
      http_uri:parse("google.com:/?")),
   ?_assertEqual(
      #http_uri{
         scheme   = <<"http">>,
         username = <<>>,
         host     = <<>>,
         port     = <<"80">>,
         path     = <<"/path">>,
         query    = <<"">>
        },
      http_uri:parse("/path")),
   ?_assertEqual(
      #http_uri{
         scheme   = <<"http">>,
         username = <<>>,
         host     = <<>>,
         port     = <<"80">>,
         path     = <<"/path">>,
         query    = <<"query">>
        },
      http_uri:parse("/path?query"))
  ].
