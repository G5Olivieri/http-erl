-module(bitstring_test).
-include_lib("eunit/include/eunit.hrl").

reverse_test_() ->
  [
   ?_assertEqual(<<"">>, bitstring:reverse(<<"">>)),
   ?_assertEqual(<<"321">>, bitstring:reverse(<<"123">>)),
   ?_assertEqual(<<"elór">>, bitstring:reverse(<<"róle">>)),
   ?_assertEqual(<<"21 = 6 ", 16#D7, " 2">>, bitstring:reverse(<<"2 ", 16#D7, " 6 = 12">>))
  ].

member_test_() ->
  [
   ?_assert(bitstring:member(<<"t">>, <<"Lorem ipsum dolor sit amet">>)),
   ?_assert(bitstring:member(<<"u">>, <<"Lorem ipsum dolor sit amet">>)),
   ?_assertNot(bitstring:member(<<"ug">>, <<"Lorem ipsum dolor sit amet">>)),
   ?_assert(bitstring:member(<<16#D7>>, <<"21 = 6 ", 16#D7, " 2">>))
  ].

tokens_test_() ->
  [
   ?_assertEqual([<<"Lorem ipsum dolor sit amet">>],
                 bitstring:tokens(<<"Lorem ipsum dolor sit amet">>, <<>>)),
   ?_assertEqual([<<"Lorem ips">>, <<"m dolor sit amet">>],
                 bitstring:tokens(<<"Lorem ipsum dolor sit amet">>, <<"u">>)),
   ?_assertEqual([<<"Lorem ">>, <<"ps">>, <<"m dolor s">>, <<"t amet">>],
                 bitstring:tokens(<<"Lorem ipsum dolor sit amet">>, <<"ui">>)),
   ?_assertEqual([<<"ról">>], bitstring:tokens(<<"róle">>, <<"e">>)),
   ?_assertEqual([<<"ró">>], bitstring:tokens(<<"róle">>, <<"le">>)),
   ?_assertEqual([<<"óle">>], bitstring:tokens(<<"róle">>, <<"r">>))
  ].
