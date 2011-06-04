%% @author Uvarov Michael <freeakk@gmail.com>
%% @copyright 2011 Uvarov Michael <freeakk@gmail.com>

%% @doc Web server for web_col.

-module(web_col_web).
-author("Mochi Media <dev@mochimedia.com>").
-author("Uvarov Michael <freeakk@gmail.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    io:format("Define atoms: ~w", 
        [{non_ignorable, blanked, shifted, shift_trimmed, lower, upper, off}]),

    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    [$/] ++ Path = Req:get(path),
    
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    "data" ->
                        col_data(Req);
                    "key" ->
                        col_key(Req);
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Return sorted data as plain text.
col_data(Req) ->
    PostList = Req:parse_post(),
    InStrings = ux_string:explode($\n, ux_par:string("input", PostList)),
    [_|_] = InStrings,
    
    OutStrings = ux_col:sort(InStrings, col_params(PostList)),

    Res = string:join(OutStrings, "\n"),
    Req:ok({"text/plain", unicode:characters_to_binary(Res)}).

%% Return sort key.
col_key(Req) ->
    PostList = Req:parse_post(),
    InStrings = ux_string:explode($\n, ux_par:string("input", PostList)),
    [_|_] = InStrings,
    
    Params = col_params(PostList),
    OutStrings = [
        lists:flatten(
            io_lib:format("~w", 
                [ux_col:sort_key(Str, Params)])) || Str <- InStrings],
    Res = string:join(OutStrings, "\n"),
    Req:ok({"text/plain", list_to_binary(Res)}).

%% Extract params from a POST data list.
col_params(PostList) ->
    V = ux_col:get_options([
        {natural_sort, ux_par:atom("natural_sort", PostList)},
        {case_sensitive, ux_par:atom("case_sensitive", PostList)},
        {strength, ux_par:integer("strength", PostList)},
        {alternate, ux_par:atom("alternate", PostList)},
        {case_first, ux_par:atom("case_first", PostList)}
    ]), io:format("~w~n", [V]), V.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
