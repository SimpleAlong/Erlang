%%%-------------------------------------------------------------------
%%% @author Along
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 2月 2021 10:48
%%%-------------------------------------------------------------------
-module(tcp_server_app).
-author("Along").
-behaviour(application).
%% API
-export([start/2, stop/1, app_start/0]).
%%--------------------------------------------------------------------
%% @doc
%%  节点开启 开始监督进程
%% @spec start(_StartType, _StartArgs)  -> {'ok', pid()} | {'ok', pid(), State :: term()} | {'error', Reason :: term()}.
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    tcp_server_sup:start_link().

%%--------------------------------------------------------------------
%% @doc
%%  节点关闭
%% @spec stop(_StopArg)  ->   term()
%% @end
%%--------------------------------------------------------------------
stop(_StopArg) ->
    init:stop(),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% 启动一个应用程序
%%  启动应用程序 Application。如果应用程序没有加载，应用程序控制器首先会使用 application:load/1 函数把它加载。
%%  它将会确定所有要包含的应用程序都加载进来。但是不会启动它们。确保应用程序 Application 的代码能够得到保护。
%%
%%  应用程序控制器将会检测应用描述里的键 applications 的值，来确保应用程序运行之前其他所有要包含进来的应用程序都已经启动好了。
%%  如果没，将返回 {error,{not_started,App}}，App 是缺少没有启动的应用程序。
%%
%%  应用控制器会为应用程序 Application 生成一个主应用程序。那个主应用程序来管理应用程序 Application 里的所有进程。
%%  主应用程序通过调用定义在应用描述文件键 mod 里的回调函数 Module:start/2 来启动。

%% @spec application：start(Application) -> ok | {error, Reason}
%% @param Application     -- param describe ::type ()--
%% @end
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @private
%% @doc
%%  describe message
%% @spec function(..Param) -> ok
%% @param param     -- param describe ::type ()--
%% @end
%%--------------------------------------------------------------------
app_start() ->
    app_start([tcp_server]).
app_start([]) ->
    ok;
app_start([App | T] = Apps) ->

    case application:start(App) of
        {error, {not_started, App2}} ->
            add_app_path(App2),
            app_start([App2] ++ Apps);
        {'error', Reason} ->
            io:format("start application ~p Apps:~p failed, reason: ~p~n", [App, Apps, Reason]);
        _ ->
            app_start(T)
    end.
add_app_path(App) ->
    code:add_path(lists:concat(["../deps/", App, "/ebin"])).