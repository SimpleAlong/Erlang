%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 2月 2021 16:26
%%%-------------------------------------------------------------------
-module(tcp_client_app).
-author("Administrator").
-behaviour(application).
%% API
-export([start/2, stop/1, ensure_start/0]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%  节点开启
%% @spec start(_StartType, _StartArgs)  -> {'ok', pid()} | {'ok', pid(), State :: term()} | {'error', Reason :: term()}.
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    tcp_client_sup:start_link().

%%--------------------------------------------------------------------
%% @doc
%%  节点关闭
%% @spec stop(_StopArg)  ->   term()
%% @end
%%--------------------------------------------------------------------
stop(_StopArg) ->
    init:stop(),
    ok.

ensure_start() ->
    ensure_deps_start([tcp_client]).
ensure_deps_start([]) ->
    ok;
ensure_deps_start([App | T] = Apps) ->
    case application:start(App) of
        {error, {not_started, App2}} ->
            add_deps_path(App2),
            ensure_deps_start([App2] ++ Apps);
        {'error', Reason} ->
            io:format("start application ~p Apps:~p failed, reason: ~p~n", [App, Apps, Reason]);
        _ ->
            ensure_deps_start(T)
    end.
add_deps_path(App) ->
    code:add_path(lists:concat(["../deps/", App, "/ebin"])).