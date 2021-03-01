%%%-------------------------------------------------------------------
%%% @author Along
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 2月 2021 11:11
%%%-------------------------------------------------------------------
-module(tcp_server_listener).
-author("Along").
-behaviour(gen_server).
-include("logger.hrl").
%% API
-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-define(port, 4399).

-record(state, {socket, ref}).
%%tcp_server监听参数
-define(TCP_OPTIONS, [binary, {packet, 2}, {active, true}, {reuseaddr, true}, {nodelay, false}, {delay_send, true}, {send_timeout, infinity}, {keepalive, true}, {exit_on_close, true}]).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%%  init 函数
%% @spec init([])  -> {ok, term()}
%% @param []     -- 与gen_server:start_link ArgParam 全匹配--
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(?port, ?TCP_OPTIONS) of
        %% 创建监听成功返回监听socket
        {ok, Socket} ->
            gen_server:cast(self(), accept),
            {ok, #state{socket = Socket}};
        {error, Reason} ->
            {stop, Reason}
    end.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc 服务进程被同步调用时的回调函数
%% @end
%%-------------------------------------------------------------------------
handle_call(Request, _From, State) ->
    {stop, {unknown_cal, Request}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc 服务进程被异步调用时的回调函数
%% @end
%%-------------------------------------------------------------------------
handle_cast(accept, State) ->
    accept(State);
handle_cast(Request, State) ->
    {stop, {unknown_cal, Request}, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc 回调函数，处理那些直接发消息到进程邮箱的事件
%% 这里处理的是 {inet_async, ListSock, Ref, {ok, CliSocket}}事件，
%% inet_async 表示是一个异步事件，服务器端接收连接采用异步的方式，
%% 客户端连接最终会被转化成一个 inet_async 消息发送到进程邮箱等待处理
%% {{ok, CliSocket}} 里的CliSocket表示客户端建立的连接套接口
%% @end
%%-------------------------------------------------------------------------
handle_info({inet_async, ListenSock, Ref, {ok, CliSocket}}, #state{socket =ListenSock, ref = Ref} = State) ->
    case set_socket_opt(ListenSock, CliSocket) of
        ok -> ok;
        {error, Reason} -> exit({set_sockopt, Reason})
    end,
    start_client(CliSocket),
    accept(State);
handle_info({inet_async, LSock, Ref, {error, closed}}, State=#state{socket=LSock, ref=Ref}) ->
    {stop, normal, State};

handle_info(Info, State) ->
    io:format("~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket),
    ok.

set_socket_opt(ListenSocket, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListenSocket, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(CliSocket, Opts) of
                ok -> ok;
                Error -> gen_tcp:close(CliSocket), Error
            end;
        Error ->
            gen_tcp:close(CliSocket), Error
    end.

accept(State = #state{socket =LSock}) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} -> {noreply, State#state{ref=Ref}};
        Error     -> {stop, {cannot_accept, Error}, State}
    end.

%% 开启客户端服务
start_client(Sock) ->
    {ok, Child} = tcp_msg_handle:start(Sock),
    ok = gen_tcp:controlling_process(Sock, Child).