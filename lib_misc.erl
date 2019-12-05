-module(lib_misc).
-compile(export_all).

show_string(T) ->
  io:format("~p~n", [T]).

show_bitstring(T) when is_list(T) ->
  show_bitstring(list_to_binary(T));
show_bitstring(T) ->
  io:format("~p~n", [T]).

pattern2var("*" = T) -> T;
pattern2var(_) -> error.
