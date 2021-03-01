%%%-------------------------------------------------------------------
%%% @author Along
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 2月 2021 16:44
%%%-------------------------------------------------------------------
-module(client).
-author("Along").

%% API
-export([connect/0, connect/1, close/1, request/2, receive_msg/0]).

connect() -> connect(10000).
connect(0) -> skip;
connect(Num) ->
    spawn(fun()-> t() end),
    connect(Num - 1).

t()->
    {ok, S} = gen_tcp:connect("127.0.0.1", 4399, [binary, {packet, 2},{send_timeout, infinity}]),
    lists:foreach(fun(X) -> gen_tcp:send(S, <<(byte_size(integer_to_binary(X))):16, (integer_to_binary(X))/binary>>) end, lists:seq(1, 100000000))
.

close(Socket) ->
    gen_tcp:close(Socket).

request(Socket, Msg) ->
    ok = gen_tcp:send(Socket, Msg),
    receive_msg().

receive_msg() ->
    receive M ->
        error_logger:info_msg("~p~n", [M])
    end.