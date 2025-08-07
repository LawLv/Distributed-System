-module(gms1).
-export([start/1, start/2]).

start(Id) -> % start the first node and become the leader
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Self) end)}.

init(Id, Master) -> % Node ID, Application layer process
    leader(Id, Master, [], [Master]). %  (_, _, Slaves list, Group list)

start(Id, Grp) -> % append a node (Node ID, One of the member node's ID)
    Self = self(),
    {ok, spawn_link(fun() -> init(Id, Grp, Self) end)}.

init(Id, Grp, Master) ->  % new node id, member node's ID, new node application id
    Self = self(),
    Grp ! {join, Master, Self}, % send msg to some Grp node
    receive
        {view, [Leader|Slaves], Group} ->
            Master ! {view, Group},
            slave(Id, Master, Leader, Slaves, Group) % turn new node to slave node
    end.


leader(Id, Master, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            % multicast to all peers
            % send to the application layer
            bcast(Id, {msg, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, Slaves, Group);
        {join, Wrk, Peer} -> % {_, app id, node id}
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, [self()|Slaves], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, Slaves2, Group2);
        stop ->
            ok

    end.

bcast(_, Msg, Slaves) -> % broadcast Msg to all Slaves
    lists:map(fun(Slave) -> Slave ! Msg end, Slaves).

slave(Id, Master, Leader, Slaves, Group) ->
    receive
        {mcast, Msg} -> % send Msg to leader
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join, Wrk, Peer} -> % redirect join request
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg, Msg} -> % redirect Msg to Master
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader|Slaves2], Group2} -> % renew view of slaves
            Master ! {view, Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        stop ->
            ok
    end.