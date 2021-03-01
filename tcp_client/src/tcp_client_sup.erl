%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 2月 2021 16:39
%%%-------------------------------------------------------------------
-module(tcp_client_sup).
-author("Administrator").
-behaviour(supervisor).

%% API
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 5, 60}, [{client,
        {client, start_link, []},
        temporary,
        2000,
        worker,
        [client]}]
    }}.