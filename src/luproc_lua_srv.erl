-module(luproc_lua_srv).
-behaviour(gen_server).

-include_lib("luerl/include/luerl.hrl").

%% API.
-export([start_link/0]).
-export([
	load_file/1,
	get_functions/0,
	run_function/2
]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {}).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

%% API.
load_file(File) ->
	gen_server:call(?SERVER, {load_file, File}).

get_functions() ->
	ets:select(?TAB, [{{'$1','_'},[{'=/=','$1','__state'}],['$1']}]).

run_function(KV = {_File, _Name}, Args) ->
	case ets:lookup(?TAB, KV) of
		[] ->
			erlang:error({badpath, KV});
		[{_, FunRef}] ->
			luerl_emul:functioncall(FunRef, Args, get_state())
	end.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server.

init([]) ->
	erlang:process_flag(trap_exit, true),
	ets:new(?TAB, [protected, named_table]),
	put_state(luproc_env:new_state()),
	{ok, #state{}}.

handle_call({load_file, File}, _From, State) ->
	{reply, intern_load_file(File), State};
handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

put_state(State) ->
	ets:insert(?TAB, {'__state', State}).

get_state() ->
	[{'__state', State}] = ets:lookup(?TAB, '__state'),
	State.

intern_load_file(File) ->
	case luproc_env:load_functions(File, get_state()) of
		{ok, FuncRefs, State1 = #luerl{}} ->
			put_state(State1),
			{ok, [begin 
				Info = luproc_env:funref_info(FuncRef, State1),
				Key = {proplists:get_value(file, Info), proplists:get_value(name, Info)},
				ets:insert(?TAB, {Key, FuncRef}),
				Key
			end || FuncRef <- FuncRefs]};
		Err = {error, _, _} ->
			Err
	end.