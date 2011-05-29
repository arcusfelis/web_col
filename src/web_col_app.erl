%% @author Mochi Media <dev@mochimedia.com>
%% @copyright web_col Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the web_col application.

-module(web_col_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for web_col.
start(_Type, _StartArgs) ->
    web_col_deps:ensure(),
    web_col_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for web_col.
stop(_State) ->
    ok.
