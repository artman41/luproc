-module(luproc_lib_erlfunc).
-include_lib("luerl/src/luerl.hrl").

-export([install/1]).
-export([require_erlmod/3, erlmod_shim/3]).

-import(luerl_lib, [lua_error/2, badarg_error/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(debug_log(Str), ?debugMsg(Str)).
-define(debug_log(F, A), ?debugFmt(F, A)).
-else.
-define(debug_log(Str), ok).
-define(debug_log(F, A), ok).
-endif.

install(State0) ->
    luerl_emul:set_global_key(<<"require_erlmod">>, #erl_mfa{m=?MODULE, f=require_erlmod}, State0).

require_erlmod(_Args, LuaArgs, State0) ->
    case luerl_lib:conv_list(LuaArgs, [lua_string]) of
        [ModBin] -> 
            ?debug_log("ModBin: ~p~n", [ModBin]),
            case binary_to_mod(ModBin) of
                {ok, Mod} ->
                    ?debug_log("Mod: ~p~n", [Mod]),
                    require_erlmod_(Mod, State0);
                Err = {error, _} ->
                    badarg_error({require_erlmod, Err}, LuaArgs, State0)
            end;
        error -> 
            badarg_error(require_erlmod, LuaArgs, State0)
    end.

erlmod_shim([Fun], LuaArgs, State0) ->
    ?debug_log("erlmod_shim was called with Fun=~p, LuaArgs=~p~n", [Fun, LuaArgs]),
    Args = luerl:decode_list(LuaArgs, State0),
    ?debug_log("Args: ~p~n", [Args]),
    try erlang:apply(Fun, Args) of
        Ret ->
            ?debug_log("Ret: ~p~n", [Ret]),
            luerl:encode(Ret, State0)
    catch 
        error:badarg ->
            badarg_error(Fun, LuaArgs, State0);
        E:R ->
            lua_error({E, R}, State0)
    end.

%% Internal

require_erlmod_(Mod, State0) ->
    {Pt, State1} = luerl_emul:get_global_key(<<"package">>, State0),
    case luerl_emul:get_table_keys(Pt, [<<"loaded">>,Mod], State1) of
        {nil,State2} ->				%Not loaded
            {Tab, State3} = install_mod(Mod, State2),
            ?debug_log("Tab: ~p~n", [Tab]),
            Key = atom_to_binary(Mod),
            State4 = luerl_emul:set_global_key(Key, Tab, State3),
            ?debug_log("Set global key for ~p~n", [Mod]),
            Res = [true],
            State5 = luerl_emul:set_table_keys(Pt, [<<"loaded">>, Mod], Res, State4),
            ?debug_log("Added ~p to Loaded Packages~n", [Key]),
            {Res, State5};
        {Val,St2} -> 
            {[Val],St2}		%Already loaded
    end.

install_mod(Mod, State0) ->
    Table = 
        [
            {<<(atom_to_binary(Func))/binary, (integer_to_binary(Arity))/binary>>, #erl_mfa{m=?MODULE, f=erlmod_shim, a = [fun Mod:Func/Arity]}} 
        || {Func, Arity} <- Mod:module_info(exports)],
    ?debug_log("Table: ~p~n", [Table]),
    luerl_heap:alloc_table(Table, State0).

binary_to_mod(Bin) ->
    try binary_to_existing_atom(Bin, utf8) of
        Atom ->
            case code:ensure_loaded(Atom) of
                {module, Module} ->
                    {ok, Module};
                Err = {error, _} ->
                    Err
            end
    catch error:badarg ->
        {error, undef_atom}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test_state() ->
    State0 = luerl:init(),
    State1 = ?MODULE:install(State0).

require_erlmod_test() ->
    {module, jsx} = code:ensure_loaded(jsx),
    Expected = <<"{\"a\":1,\"b\":2}">>,

    State0 = test_state(),
    {_, State1} = luerl:do("require_erlmod(\"jsx\")", State0),
    {ModInfo, State2} = luerl:do("return jsx.module_info0();", State1),
    ?debugFmt("ModInfo: ~p~n", [ModInfo]),
    {Ret, _State3} = luerl:do("return jsx.encode1({a = 1, b = 2});", State2),
    ?assertEqual(Expected, Ret).

-endif.