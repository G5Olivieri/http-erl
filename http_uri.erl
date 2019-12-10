-module(http_uri).
-include("http_uri.hrl").
-export([parse/1]).

parse(Url) when is_list(Url) ->
  parse(list_to_binary(Url));
parse(<<$/, Uri>>) ->
  parse_path(Uri, <<"http">>, <<>>, <<>>, <<>>, <<"/">>).
parse(Url) ->
  parse_scheme(Url).

parse_scheme(<< H, T, T, P, "://", Rest/bits >>)
  when H =:= $h orelse H =:= $H, T =:= $t orelse T =:= $T;
       P =:= $p orelse P =:= $P ->
  parse_username(Rest, <<"http">>, <<>>);
parse_scheme(Url) ->
  parse_username(Url, <<"http">>, <<>>).

parse_username(<<>>, Scheme, Username) ->
  #http_uri{
     scheme=Scheme,
     host=Username
    };
parse_username(<<C, Rest/bitstring>>, Scheme, Username) ->
  case C of
    $: -> parse_port(Rest, Scheme, <<>>, Username, <<>>);
    $/ -> parse_path(Rest, Scheme, <<>>, Username, <<"80">>, <<"/">>);
    $? -> parse_query(Rest, Scheme, <<>>, Username, <<"80">>, <<"/">>, <<>>);
    $@ -> parse_host(Rest, Scheme, Username, <<>>);
    _ -> parse_username(Rest, Scheme, <<Username/bitstring, C>>)
  end.

parse_host(<<>>, _, _, <<>>) ->
  {error, "Missing host"};
parse_host(<<>>, Scheme, Username, Host) ->
  #http_uri{
     scheme=Scheme,
     username=Username,
     host=Host
    };
parse_host(<<C, Rest/bitstring>>, Scheme, Username, Host) ->
  case C of
    $: -> parse_port(Rest, Scheme, Username, Host, <<>>);
    $/ -> parse_path(Rest, Scheme, Username, Host, <<"80">>, <<"/">>);
    $? -> parse_query(Rest, Scheme, Username, Host, <<"80">>, <<"/">>, <<>>);
    _ -> parse_host(Rest, Scheme, Username, <<Host/bitstring, C>>)
  end.

parse_port(<<>>, Scheme, Username, Host, Port) ->
  #http_uri{
     scheme=Scheme,
     username=Username,
     host=Host,
     port=Port
    };
parse_port(<<C, Rest/bitstring>>, Scheme, Username, Host, Port) ->
  case C of
    $/ -> parse_path(Rest, Scheme, Username, Host, Port, <<"/">>);
    $? -> parse_query(Rest, Scheme, Username, Host, Port, <<"/">>, <<>>);
    _ -> parse_port(Rest, Scheme, Username, Host, <<Port/bitstring, C>>)
  end.

parse_path(<<>>, Scheme, Username, Host, Port, Path) ->
  #http_uri{
     scheme=Scheme,
     username=Username,
     host=Host,
     port=Port,
     path=Path
    };
parse_path(Rest, Scheme, Username, Host, <<>>, Path) ->
  parse_path(Rest, Scheme, Username, Host, <<"80">>, Path);
parse_path(<<C, Rest/bitstring>>, Scheme, Username, Host, Port, Path) ->
  case C of
    $? -> parse_query(Rest, Scheme, Username, Host, Port, Path, <<>>);
    _ -> parse_path(Rest, Scheme, Username, Host, Port, <<Path/bitstring, C>>)
  end.

parse_query(<<>>, Scheme, Username, Host, Port, Path, Query) ->
  #http_uri{
     scheme=Scheme,
     username=Username,
     host=Host,
     port=Port,
     path=Path,
     query=Query
    };
parse_query(Rest, Scheme, Username, Host, <<>>, Path, Query) ->
  parse_query(Rest, Scheme, Username, Host, <<"80">>, Path, Query);
parse_query(<<C, Rest/bitstring>>, Scheme, Username, Host, Port, Path, Query) ->
  parse_query(Rest, Scheme, Username, Host, Port, Path, <<Query/bitstring, C>>).
