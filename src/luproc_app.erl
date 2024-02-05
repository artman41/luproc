-module(luproc_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	case luproc_sup:start_link() of
		Ok = {ok, _Sup} ->
			load_lua_files(),
			Ok;
		Err = {error, _} ->
			Err
	end.

stop(_State) ->
	ok.


load_lua_files() ->
	[case luproc_lua_srv:load_file(LuaFile) of
		{ok, _} ->
			luproc_luafile_sup:start_child(list_to_binary(LuaFile));
		Err ->
			Err
	end || LuaFile <- filelib:wildcard("lua/**/*.lua")],
	ok.