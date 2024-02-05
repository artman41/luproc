-module(luproc_lib_jsx).

-include_lib("luerl/src/luerl.hrl").

-export([lib/0, install/1]).
-export([encode/2, decode/2]).

-import(luerl_lib, [lua_error/2, badarg_error/3]).

lib() ->
    {<<"jsx">>, ?MODULE}.

install(State0) ->
    luerl_heap:alloc_table(table(), State0).

table() ->
    [
        {<<"encode">>, #erl_func{code = fun encode/2}},
        {<<"decode">>, #erl_func{code = fun decode/2}}
    ].

encode([_Self | Args], State0) ->
    Decoded = [Elem | _] = luerl:decode_list(Args, State0),
    try jsx:encode(Elem, []) of
        Json ->
            luerl:encode(Json, State0)
    catch error:badarg ->
        lua_error(Decoded, State0)
    end.

decode([_Self | Args], State0) ->
    Decoded = [Elem | _] = luerl:decode_list(Args, State0),
    try jsx:decode(Elem, []) of
        Term ->
            luerl:encode(Term, State0)
    catch error:badarg ->
        lua_error(Decoded, State0)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test_state() ->
    State0 = luerl:init(),
    {Tab, State1} = ?MODULE:install(State0),
    Key = element(1, lib()),
    State2 = luerl_emul:set_global_key(Key, Tab, State1),
    luerl_emul:set_table_keys([<<"package">>,<<"loaded">>,Key], Tab, State2).

encode_test() ->
    Expected = <<"{\"a\":1,\"b\":2}">>,

    State0 = test_state(),
    {Ret, _State} = luerl:do("return jsx:encode({a = 1, b = 2})", State0),
    ?assertEqual(Expected, Ret).

decode_test() ->
    Expected = [{<<"a">>,1},{<<"b">>,2}],

    State0 = test_state(),
    {Table, State} = luerl:do("return jsx:decode('{\"a\":1,\"b\":2}')", State0),
    ?assertEqual(element(1, Table), tref),
    ?assertEqual(Expected, luerl:decode(Table, State)).

-endif.