-module(luproc_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	SupFlags = {one_for_one, 1, 5},
	Procs = [
		#{
			id => luproc_luafile_sup,
			start => {luproc_luafile_sup, start_link, []},
			restart => permanent,
			shutdown => 5000,
			type => supervisor
		},
		#{
			id => luproc_lua_srv,
			start => {luproc_lua_srv, start_link, []},
			restart => permanent,
			shutdown => 5000,
			type => worker
		}
	],
	{ok, {SupFlags, Procs}}.
