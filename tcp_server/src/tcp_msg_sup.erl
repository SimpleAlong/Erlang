%%%-------------------------------------------------------------------
%%% @author Along
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 2月 2021 10:50
%%%-------------------------------------------------------------------
-module(tcp_msg_sup).
-author("Along").
-behaviour(supervisor).
%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).
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
start_link()->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([])->
    SupFlag = {simple_one_for_one,5, 10},

    StartSpec = [
        ?CHILD(tcp_msg_handle, worker)
    ],
    {ok, {SupFlag,StartSpec}}.




