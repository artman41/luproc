-module(luproc_env).

-include_lib("luerl/src/luerl.hrl").

-export([
    new_state/0,
    load_functions/2,
    funref_info/2
]).

-spec new_state() -> #luerl{}.
new_state() ->
    State0 = luerl:init(),
    State1 = install_packages(State0).

-spec load_functions(iodata(), #luerl{}) -> {ok, list(#funref{}), #luerl{}} | {error, list(tuple()), list(tuple())}.
load_functions(File, State) ->
    case luerl:loadfile(File, State) of
        Ok = {ok, FR = #funref{i=N}, State1} ->
            {#lua_func{funrefs = FunRefs}, State2} = luerl_heap:get_funcdef(FR, State1),
            {ok, FunRefs, delete_func(FR, State2)};
        Err = {error, _, _} ->
            Err
    end.

%% funref_info(#funref{}, #luerl{}) -> [{file, binary()}, {name, binary()}].
-spec funref_info(#funref{}, #luerl{}) -> list().
funref_info(#funref{i = N}, State = #luerl{fncs = Funcs}) ->
    #lua_func{anno = Anno} = luerl_heap:get_tstruct(N, Funcs),
    [{file, proplists:get_value(file, Anno)}, {name, proplists:get_value(name, Anno)}].

%% Internal Actions

install_packages(State) ->
    load_libs([
        luproc_lib_jsx:lib()
    ], State).

load_libs([], State) ->
    State;
load_libs([{Key, Mod}|Tail], State0) ->
    State1 = load_lib(Key, Mod, State0),
    load_libs(Tail, State1).

load_lib(Key, Mod, State0) ->
    {Tab, State1} = Mod:install(State0),
    %% Add key to global and to package.loaded.
    State2 = luerl_emul:set_global_key(Key, Tab, State1),
    luerl_emul:set_table_keys([<<"package">>, <<"loaded">>, Key], Tab, State2).

delete_func(#funref{i = N}, State = #luerl{fncs = Funcs0}) ->
    Funcs1 = luerl_heap:del_tstruct(N, Funcs0),
    State#luerl{fncs = Funcs1}.