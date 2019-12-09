-module(http_uri).
-compile(export_all).

parse_path(Url) when is_list(Url) ->
  parse_path(list_to_binary(Url));
parse_path(<<"http://", Url/bitstring>>) ->
  parse_path(Url);
parse_path(Url) ->
  case string:split(Url, <<"?">>) of
    [_] -> parse_path_1(Url);
    [_, <<>>] -> parse_path_1(Url);
    [Host, _]-> parse_path_1(Host)
  end.

parse_path_1(Host) ->
  case string:split(Host, <<"/">>) of
    [_] -> {path, <<"/">>};
    [_, <<>>] -> {path, <<"/">>};
    [_, Path] -> {path, <<"/", Path/bitstring>>}
  end.

parse_host(Url) when is_list(Url) ->
  parse_host(list_to_binary(Url));
parse_host(<<"http://", Url/bitstring>>) ->
  parse_host(Url);
parse_host(Url) ->
  case bitstring:member(<<"@">>, Url) of
    true ->
      [_,Host] = string:split(Url, <<"@">>),
      parse_host_1(Host);
    false ->
      parse_host_1(Url)
  end.

parse_host_1(Url) ->
    [Host|_] = bitstring:tokens(Url, <<":?/">>),
    {host, Host}.

parse_scheme(_) ->
  {scheme, <<"http">>}.
