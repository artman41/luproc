-module(luproc_luafile_sup).
-behaviour(supervisor).

-export([start_child/1]).
-export([start_link/0]).
-export([init/1]).

start_child(LuaFile) when is_binary(LuaFile) ->
	supervisor:start_child(?MODULE, [LuaFile]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
		#{
			id => luproc_luafile_srv,
			start => {luproc_luafile_srv, start_link, []},
			restart => permanent,
			shutdown => 5000,
			type => worker
		}
	],
	{ok, {{simple_one_for_one, 1, 5}, Procs}}.
