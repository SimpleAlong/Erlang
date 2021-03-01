%%%-------------------------------------------------------------------
%%% @author Along
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 2月 2021 10:33
%%%-------------------------------------------------------------------
-module(tcp_msg_handle).
-author("Along").
-behaviour(gen_server).
-include("logger.hrl").
%% API
-export([start/1, start_link/1]).
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2]).

-record(state, {
    msgdata = <<>>,
    socket
}).
-define(port, 4399).
-define(TCP_TIMEOUT, 120000).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%   启动simple_one_for_one
%% @spec start(Supervisor, ChildSpec)-> ref
%% @param param     -- param describe ::type ()--
%% @end
%%--------------------------------------------------------------------

start(Socket) ->
    SupervisorName = tcp_msg_sup,
    Arg = [Socket], %% Arg 可以是childSpec…… #{id, ……}, 可以是list, [Term()] list里面必须有Erlang项。 start_link 接收是Term项
    supervisor:start_child(SupervisorName, Arg).


start_link(Arg) ->
    gen_server:start_link(?MODULE, [Arg], []).

%%--------------------------------------------------------------------
%% @doc
%%  init 函数
%% @spec init([Arg])  -> {ok, term()}
%% @param [Arg]     -- 与gen_server:start_link ArgParam 全匹配--
%% @end
%%--------------------------------------------------------------------
init([Socket]) ->
    process_flag(trap_exit,true),
    ?INFO_LOG("socket ~p", [Socket]),
    {ok, #state{socket = Socket}}.

handle_call(Request, _From, State) ->
    {reply, Request, State}.

handle_cast(_Request, State) ->
    {reply, State}.

handle_info({tcp, Socket, Msg}, #state{
    msgdata = MsgData} = State) ->
    inet:setopts(Socket, [{active, once}]),
    handleMsg(<<MsgData/binary, Msg/binary>>, State);


handle_info(tcp_closed, State) ->
    ?INFO_LOG("=======tcp_closed========="),
    {stop, normal, State};

handle_info(timeout, State) ->
    ?INFO_LOG("=============~ts===============",["处理超时"]),
    {stop, normal, State};

handle_info(Info, State) ->
    ?INFO_LOG("=======otherMsg:~p==========", [Info]),
    {stop, normal, State}.

terminate(TerminateResponse, _State) ->
    ?INFO_LOG("~ts TerminateResponse ~p ", ["进程退出处理",TerminateResponse]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 处理消息及粘包、分包问题
%% @spec handleMsg(Msg,State) -> OtherMsg
%% @param param     -- param describe ::type ()--
%% @end
%%--------------------------------------------------------------------
handleMsg(<<Len:16, Data:Len/binary>>, State) ->
    ?INFO_LOG("hangleMsg1:~p", [Data]),
    gen_tcp:send(State#state.socket, <<Len:16, Data:Len/binary>>),
    {noreply, State#state{msgdata= <<>>}, ?TCP_TIMEOUT};
handleMsg(<<Len:16, Data:Len/binary, Other/binary>>, State) ->
    ?INFO_LOG("hangleMsg2:~p", [Data]),
    gen_tcp:send(State#state.socket, <<Len:16, Data:Len/binary>>),
    handleMsg(Other, State);
handleMsg(OtherMsgIn, State) ->
    ?ERROR_LOG("OtherMsgIn: ~p", [OtherMsgIn]),
    {noreply, State#state{msgdata = OtherMsgIn}, ?TCP_TIMEOUT}.