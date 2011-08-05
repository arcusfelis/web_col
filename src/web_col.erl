%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc web_col.

-module(web_col).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the web_col server.
start() ->
    web_col_deps:ensure(),
    ensure_started(crypto),
    ensure_started(ux),
    application:start(web_col).


%% @spec stop() -> ok
%% @doc Stop the web_col server.
stop() ->
    application:stop(web_col).
