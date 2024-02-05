
define tpl_luerl_lib
-module($(n)).

-include_lib("luerl/src/luerl.hrl").

-export([lib/0, install/1]).

-import(luerl_lib, [lua_error/2, badarg_error/3]).

lib() ->
    {<<"$(n)">>, ?MODULE}.

install(State0) ->
    luerl_heap:alloc_table(table(), State0).

table() ->
    [
        {<<"func">>, #erl_mfa{m = ?MODULE, f = func}}
    ].

func(_DefaultArgs, [_Self | Args], State0) ->
    DecodedArgs = [Elem | _],
	lua_error(not_implemented, State0).
endef