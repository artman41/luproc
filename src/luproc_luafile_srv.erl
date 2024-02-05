-module(luproc_luafile_srv).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	luafile :: binary()
}).

%% API.

-spec start_link(iodata()) -> {ok, pid()}.
start_link(LuaFile) ->
	gen_server:start_link(?MODULE, [LuaFile], []).

%% gen_server.

init([LuaFile]) ->
	{ok, #state{luafile = LuaFile}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
