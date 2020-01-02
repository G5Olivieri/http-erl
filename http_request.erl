-module(http_request).
-compile(export_all).
-include("http_request.hrl").

parse_request_line(R) when is_list(R) ->
  parse_request_line(list_to_binary(R));
parse_request_line(R) ->
  method(R, <<>>).

method(<<C, Rest/bitstring>>, Method) ->
  case C of
    $\s -> resource(Rest, bitstring:to_upper(Method));
    _ -> method(Rest, <<Method/bitstring, C>>)
  end.

resource(R, Method) ->
  [Uri, Rest] = string:split(R, <<" ">>),
  Res = http_uri:parse(Uri),
  version(bitstring:to_upper(Rest), Method, Res, <<>>).

version(<<>>, Method, Uri, Version) ->
  #http_request{
     method = Method,
     uri = Uri,
     version = Version
    };
version(<<"HTTP/", Rest/bitstring>>, Method, Uri, Version) ->
  version(Rest, Method, Uri, Version);
version(<<C, Rest/bitstring>>, Method, Uri, Version) ->
  case C of
    $\s -> version(Rest, Method, Uri, Version);
    $\n -> version(<<>>, Method, Uri, Version);
    _ -> version(Rest, Method, Uri, <<Version/bitstring, C>>)
  end.
