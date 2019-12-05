-module(bitstring).
-export([tokens/2, reverse/1, member/2]).

-spec member(binary(), bitstring()) -> boolean().
member(<<_X/unsigned>>, <<_X/unsigned, _/bitstring>>) ->
  true;
member(X, <<_/unsigned, T/bitstring>>) ->
  member(X, T);
member(_, <<>>) ->
  false.

-spec reverse(bitstring()) -> bitstring().
reverse(<<>>) -> <<>>;
reverse(Bin) ->
  binary:encode_unsigned(binary:decode_unsigned(Bin, little)).

-spec tokens(bitstring(), bitstring()) -> list(bitstring()).
tokens(S, Seps) ->
  case Seps of
    <<>> ->
      case S of
        <<>> -> [];
        _ -> [S]
      end;
    <<C/unsigned>> ->
      tokens_single_1(reverse(S), C, []);
    _ ->
      tokens_multiple_1(reverse(S), Seps, [])
  end.

tokens_single_1(<<Sep/unsigned, S/bitstring>>, Sep, Toks) ->
  tokens_single_1(S, Sep, Toks);
tokens_single_1(<<C/unsigned, S/bitstring>>, Sep, Toks) ->
  tokens_single_2(S, Sep, Toks, <<C>>);
tokens_single_1(<<>>, _, Toks) ->
  Toks.

tokens_single_2(<<Sep/unsigned, S/bitstring>>, Sep, Toks, Tok) ->
  tokens_single_1(S, Sep, [Tok|Toks]);
tokens_single_2(<<C/unsigned, S/bitstring>>, Sep, Toks, Tok) ->
  tokens_single_2(S, Sep, Toks, <<C/unsigned, Tok/bitstring>>);
tokens_single_2(<<>>, _, Toks, Tok) ->
  [Tok|Toks].

tokens_multiple_1(<<C/unsigned, S/bitstring>>, Seps, Toks) ->
  case member(<<C>>, Seps) of
    true -> tokens_multiple_1(S, Seps, Toks);
    false -> tokens_multiple_2(S, Seps, Toks, <<C>>)
  end;
tokens_multiple_1(<<>>, _Seps, Toks) ->
  Toks.

tokens_multiple_2(<<C/unsigned, S/bitstring>>, Seps, Toks, Tok) ->
  case member(<<C>>, Seps) of
    true -> tokens_multiple_1(S, Seps, [Tok|Toks]);
    false -> tokens_multiple_2(S, Seps, Toks, <<C/unsigned, Tok/bitstring>>)
  end;
tokens_multiple_2(<<>>, _Seps, Toks, Tok) ->
  [Tok|Toks].
