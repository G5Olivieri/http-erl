-module(test_suite).
-export([init/0, test/0]).

init() ->
  clean(),
  compile_path(),
  add_modules().

test() ->
  init(),
  lists:foreach(fun(X) -> X:test() end, tests_modules()).

tests_modules() ->
  lists:filter(fun(X) ->
                   lists:suffix("_test", atom_to_list(X))
               end, path_modules()).

path_modules() ->
  lists:map(fun(X) ->
                "maeb." ++ Y = lists:reverse(X),
                list_to_atom(lists:reverse(Y))
            end, compiled_files()).

add_modules() ->
  Modules = path_modules(),
  lists:foreach(fun(X) ->
                    case code:is_loaded(X) of
                      false -> void;
                      _ -> code:purge(X)
                    end
                end, Modules),
  code:atomic_load(Modules).

clean() ->
  lists:foreach(fun(X) -> file:delete(X) end, compiled_files()).

compile_path() ->
  lists:foreach(fun(X) -> compile:file(X) end, all_erlang_files()).

compiled_files() ->
  lists:filter(fun(X) -> lists:suffix(".beam", X) end, all_files()).

all_erlang_files() ->
  lists:filter(fun(X) -> lists:suffix(".erl", X) end, all_files()).

all_files() ->
  {ok, CurrentDir} = file:get_cwd(),
  {ok, Files} = file:list_dir(CurrentDir),
  Files.

