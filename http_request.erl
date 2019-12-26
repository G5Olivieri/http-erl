-module(http_request).
-compile(export_all).

-define(ALLOWED_METHODS, [
  <<"CONNECT">>,
  <<"DELETE">>,
  <<"GET">>,
  <<"HEAD">>,
  <<"OPTIONS">>,
  <<"POST">>,
  <<"PUT">>,
  <<"TRACE">>
  ]
).

parse_request_line(R) when is_list(R) ->
  parse_request_line(list_to_binary(R));
parse_request_line(R) ->
  method(R, <<>>);

method(<<C, Rest/bitstring>>, Method) ->
  case C of
    $\s -> resource(Rest, string:to_upper(Method), <<>>)
    _ -> method(Rest, <<Method/bitstring, C>>)
  end.

resource(R, Method, Resource) ->
  [Uri, Rest] = string:split(R, <<" ">>),
  Resource = http_uri:parse(Uri),
  version(Rest, Method, Resource, <<>>).
