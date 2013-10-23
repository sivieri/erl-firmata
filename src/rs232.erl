-module(rs232).
-export([init/0, open/2, close/0, read/2, write/1]).
-on_load(init/0).

-define(APPNAME, firmata).

init() ->
    case code:priv_dir(?APPNAME) of
        {error, _} -> 
            error_logger:format("~w priv dir not found~n", [?APPNAME]),
            exit(error);
        PrivDir ->
            erlang:load_nif(filename:join([PrivDir, "rs232"]), 0)
    end.

open(_Device, _Speed) ->
    erlang:nif_error(nif_not_loaded).

close() ->
    erlang:nif_error(nif_not_loaded).

read(_Length, _Timeout) ->
    erlang:nif_error(nif_not_loaded).

write(_Buffer) ->
    erlang:nif_error(nif_not_loaded).
