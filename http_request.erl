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

-spec request_uri(RequestLine :: bitstring() | string()) -> bitstring() | {error, Why :: bitstring()}.

request_uri(RequestLine) when is_list(RequestLine) ->
  request_uri(list_to_binary(RequestLine));
request_uri(RequestLine) ->
  [_,RequestUri|_] = string:split(RequestLine, " ", all),
  request_uri1(string:uppercase(RequestUri)).

request_uri1(<<"*">> = T) ->
  T.

-spec method(RequestLine :: bitstring() | string()) -> bitstring() | {error, Why :: bitstring()}.

method(RequestLine) when is_list(RequestLine) ->
  method(list_to_binary(RequestLine));
method(RequestLine) ->
  [Method|_] = string:split(RequestLine, " ", leading),
  method1(string:uppercase(Method)).

method1(T) ->
  case lists:member(T, ?ALLOWED_METHODS) of
    true -> T;
    false -> {error, <<"Method not found">>}
  end.
