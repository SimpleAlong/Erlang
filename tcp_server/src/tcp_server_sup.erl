%%%-------------------------------------------------------------------
%%% @author Along
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 2月 2021 10:50
%%%-------------------------------------------------------------------
-module(tcp_server_sup).
-author("Along").
-behaviour(supervisor).
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% {Id, StartFunc, Restart, Shutdown, Type, Modules}
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================
%%--------------------------------------------------------------------
%% @doc
%%  开始监督者模式
%% @spec start_link -> refult
%% @param {local, ?MODULE}     -- 参数1.监督者本地注册名--
%% @param ?MODULE              -- 参数2.回调模块 处理init--
%% @param []                   -- 参数3. init 参数--
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc
%%  init函数
%% @spec init() -> ->  {ok, {SupFlags, [ChildSpec,...]}} | ignore.
%%  SupFlags参数如下
%%{Type, Times, Sec}
%%
%%  Type: 重启策略
%%      one_for_one: 一个子进程终止，只重启该进程，在init的时候会启动参数内的子进程
%%      simple_one_for_one: 同one_for_one，但是在init的时候不会启动子进程，需要动态调用启动
%%      one_for_all: 一个子进程终止，将重启所有子进程
%%      rest_for_one: 一个子进程终止，将按顺序重启这个子进程和之后顺序的子进程
%%  Times: 次数(监控频率)
%%  Sec: 秒数(监控频率)，如果在Sec秒内重启次数超过Times，则终止所有进程，并终止监控树，将由父进程决定它的命运
%% ChildSpec参数如下
%%  Id 子进程ID标识符
%%  StartFunc = {M, F, A}: 子程序启动入口
%%  Restart: 重启方案
%%      permanent: 如果app终止了，整个系统都会停止工作（application:stop/1除外）。
%%      transient: 如果app以normal的原因终止，没有影响。任何其它终止原因都谁导致整个系统关闭。
%%      temporary: app可以以任何原因终止。只产生报告，没有其它任何影响。
%%  Shutdown: 终止策略
%%      brutal_kill: 无条件终止
%%      超时值(毫秒): 终止时，如果超时，则强制终止
%%      infinity: 如果子进程是监控树，设置为无限大，等待其终止为止
%%  Type:
%%      worker: 普通子进程
%%      supervisor: 子进程是监控树
%%  Modules:
%%      dynamic: 当子进程是gen_event
%%      [Module]: 当子进程是监控树、gen_server或者gen_fsm，表示回调模块名称
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = {one_for_one, 5, 10},
    ChildSpec = [
        ?CHILD(tcp_server_listener, worker),
        ?CHILD(tcp_msg_sup, supervisor)
    ],
    {ok, {SupFlags, ChildSpec}}.




