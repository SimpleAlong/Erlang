%%%-------------------------------------------------------------------
%%% @author Along
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 2月 2021 10:33
%%%-------------------------------------------------------------------
-module(client).
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
    socket,
    cnt = 0
}).
-define(port, 4399).
-define(host, "localhost").
-define(TCP_TIMEOUT, 120000).

%%lists:foreach(fun(X)-> client:start(X) end, lists:seq(1,10000)).
start(Socket) ->
    supervisor:start_child(tcp_client_sup, [Socket]).

start_link(Arg) ->
    gen_server:start_link(?MODULE, [Arg], []).

init(_Socket) ->
    process_flag(trap_exit, true),
    {ok, Sock} = gen_tcp:connect(?host, ?port, [binary, {packet, 2}, {active, true}]),
    gen_tcp:send(Sock, <<(byte_size(list_to_binary("hello"))):16, (list_to_binary("hello"))/binary>>),
    {ok, #state{socket = Sock}}.

handle_call(Request, _From, State) ->
    {reply, Request, State}.

handle_cast(_Request, State) ->
    {reply, State}.
handle_info({tcp, _Socket, Msg}, #state{cnt = Cnt}=State) ->
    timer:sleep(1000),
    handleMsg(Msg, State#state{cnt = Cnt + 1});
handle_info(Info, State) ->
    ?INFO_LOG("=======otherMsg:~p==========", [Info]),
    {noreply, State, ?TCP_TIMEOUT}.

terminate(TerminateResponse, _State) ->
    ?INFO_LOG("~ts TerminateResponse ~p ", ["进程退出处理", TerminateResponse]),
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
    ?INFO_LOG("tcp_client: hangleMsg1:~p", [Data]),
    gen_tcp:send(State#state.socket, binaryMsg(State#state.cnt)),
    {noreply, State#state{msgdata = <<>>}, ?TCP_TIMEOUT};
handleMsg(<<Len:16, Data:Len/binary, Other/binary>>, State) ->
    ?INFO_LOG("tcp_client: hangleMsg2:~p", [Data]),
    gen_tcp:send(State#state.socket, binaryMsg(State#state.cnt)),
    handleMsg(Other, State);
handleMsg(OtherMsgIn, State) ->
    ?ERROR_LOG("tcp_client: OtherMsgIn: ~p", [OtherMsgIn]),
    {noreply, State#state{msgdata = OtherMsgIn}, ?TCP_TIMEOUT}.


binaryMsg(Cnt)->
    Data = io_lib:format("msg index ~p", [Cnt]),
    DataBin = list_to_binary(Data),
    <<(byte_size(DataBin)):16, DataBin/binary>>.

