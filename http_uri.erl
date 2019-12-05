-module(http_uri).
-compile(export_all).

parse_host(Url) when is_list(Url) ->
  parse_host(list_to_binary(Url));
parse_host(<<"http://", Url/bitstring>>) ->
  case member(<<"@">>, Url) of
    true ->
      [_,Host] = string:split(Url, <<"@">>),
      parse_host_1(Host);
    false ->
      parse_host_1(Url)
  end.

parse_host_1(Url) ->
    [Host|_] = tokens(Url, <<":?/">>),
    {host, Host}.

parse_scheme(_) ->
  {scheme, <<"http">>}.
