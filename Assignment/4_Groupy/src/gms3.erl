-module(gms3).
-export([start/1, start/2]).
-define(arghh, 100).

start(Id) -> % start the first node and become the leader
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Self) end)}.

init(Id, Master) ->
    leader(Id, Master, 0, [], [Master]).

start(Id, Grp) -> % append a node (Node ID, One of the member node's ID)
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) -> % new node id, member node's ID, new node application id
    Self = self(),
    Grp ! {join, Master, Self}, % send msg to some Grp node
    receive
        {view, N, [Leader|Slaves], Group} ->
            erlang:monitor(process, Leader),
            Master ! {view, Group},
            slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves], Group}, Slaves, Group)
    end.


leader(Id, Master, N, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            % multicast to all peers
            % send to the application layer
            bcast(Id, {msg, N, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, N+1, Slaves, Group);
        {join, Wrk, Peer} ->
%%            io:format("reveive add request~n"),
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N+1, Slaves2, Group2);
        stop ->
            ok
    end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {join, Wrk, Peer} ->
            io:format("reveive add request~n"),
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, N, Msg} ->
            Last2 = {msg, N, Msg},
            N2 = N+1,
            Master ! Msg,
            slave(Id, Master, Leader, N2, Last2, Slaves, Group);
        {msg, I, _} when I < N -> % ignore the old msg
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {view, N, [Leader|Slaves2], Group2} -> % from leader
            Master ! {view, Group2},
            Last2 = {view, N, [Leader|Slaves2], Group2},
            N2 = N+1,
            slave(Id, Master, Leader, N2, Last2, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, N, Last, Slaves, Group);
        stop ->
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
