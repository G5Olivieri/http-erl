-module(http_request).
-compile(export_all).
-include("http_request.hrl").

parse_request_line(R) when is_list(R) ->
  parse_request_line(list_to_binary(R));
parse_request_line(R) ->
  [ReqLine, _] = string:split(R, "\n\r"),
  method(ReqLine, <<>>).

method(<<>>, _)->
  {error, "Invalid request line"};
method(<<C, Rest/bitstring>>, Method) ->
  case C of
    $\s -> resource(Rest, bitstring:to_upper(Method));
    _ -> method(Rest, <<Method/bitstring, C>>)
  end.

resource(R, Method) ->
  case string:split(R, <<" ">>) of
    [Uri, Rest] -> version(string:trim(Rest), Method, http_uri:parse(Uri));
    [_] -> {error, "Invalid request line"}
  end.

version_2(Method, Uri, Version) ->
  #http_request{
     method = Method,
     uri = Uri,
     version = Version
    }.
version(<<"HTTP/", Rest/bitstring>>, Method, Uri) ->
  version_1(Rest, Method, Uri, <<>>);
version(_, _, _) ->
  {error, "Invalid request line"}.

version_1(<<>>, Method, Uri, Version) ->
  version_2(Method, Uri, Version);
version_1(<<C, Rest/bitstring>>, Method, Uri, Version) ->
  version_1(Rest, Method, Uri, <<Version/bitstring, C>>).
