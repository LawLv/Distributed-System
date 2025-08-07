-module(gms4).
-export([start/1, start/2]).
-define(arghh, 1000).

start(Id) ->
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Self) end)}.

init(Id, Master) ->
    leader(Id, Master, 0, [], [Master]).

start(Id, Grp) ->
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, N, [Leader|Slaves], Group} ->
            erlang:monitor(process, Leader),
            Master ! {view, Group},
            slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves], Group}, Slaves, Group)
    end.


leader(Id, Master, N, Slaves, Group) ->
    receive
        {mcast, Msg} ->  % 处理多播消息
            % 向从节点广播消息，并等待ACK
            bcast_with_ack(Id, {msg, N, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, N+1, Slaves, Group);
        {join, Wrk, Peer} ->  % 处理节点加入
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast_with_ack(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N+1, Slaves2, Group2);
        stop ->  % 停止领导者
            ok
    end.

% 带ACK机制的广播
bcast_with_ack(Id, Msg, Slaves) ->
    lists:foreach(fun(Node) -> Node ! {send_msg, Id, Msg} end, Slaves),
    wait_for_acks(Id, Msg, Slaves).
% 等待从节点的ACK确认
wait_for_acks(Id, Msg, Slaves) ->
    receive
        {ack, From} ->
            RemainingSlaves = lists:delete(From, Slaves),
            if
                RemainingSlaves == [] -> ok;  % 所有ACK都收到了
                true -> wait_for_acks(Id, Msg, RemainingSlaves)
            end
    after 5000 ->  % 5秒超时后，重新发送消息
        io:format("Leader ~p: ACK timeout, resending ~p~n", [Id, Msg]),
        bcast_with_ack(Id, Msg, Slaves)
    end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    receive
        {send_msg, LeaderId, Msg} ->  % 接收到来自领导者的消息
            Master ! Msg,  % 处理消息
            LeaderId ! {ack, Id},  % 发送ACK给领导者
            slave(Id, Master, Leader, N+1, Msg, Slaves, Group);  % 继续运行
        {join, Wrk, Peer} ->  % 处理节点加入
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, N, Msg} ->  % 处理普通消息
            Master ! Msg,
            slave(Id, Master, Leader, N+1, Msg, Slaves, Group);
        {view, N, [Leader|Slaves2], Group2} ->  % 处理视图更新
            Master ! {view, Group2},
            slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves2], Group2}, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->  % 处理领导者崩溃
            election(Id, Master, N, Last, Slaves, Group);
        stop ->  % 停止从节点
            ok
    end.

election(Id, Master, N, Last, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            bcast(Id, Last, Rest), % resend the latest msg
            Master ! {view, Group},
            io:format("Current leader is: ~p~n", [Id]),
            leader(Id, Master, N, Rest, Group);
        [Leader|Rest] ->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N, Last, Rest, Group)
    end.

bcast(Id, Last, Nodes) ->
    lists:foreach(fun(Node) -> Node ! Last end, Nodes),
    crash(Id).

crash(Id) ->
    case rand:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w: crash ~n", [Id]),
            exit(no_luck);
        _ ->
            ok
    end.
