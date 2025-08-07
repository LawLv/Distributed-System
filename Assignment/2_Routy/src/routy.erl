-module(routy).
-export([start/2, stop/1, pretty_print/6]).

start(Reg, Name) -> % process called Reg, Node name is Name
    register(Reg, spawn(fun() -> init(Name) end)). % start a new process as Reg and init it.

stop(Node) ->
    Node ! stop,
    unregister(Node).

init(Name) ->
    Intf = intf:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
    receive
        {add, Node, Pid} -> % add new node, and moni its pid
            Ref = erlang:monitor(process,Pid),
            Intf1 = intf:add(Node, Ref, Pid, Intf), % add pid to Intf list
            router(Name, N, Hist, Intf1, Table, Map);
        {remove, Node} -> % remove Node
            {ok, Ref} = intf:ref(Node, Intf), % find Ref of the Node process
            erlang:demonitor(Ref),
            Intf1 = intf:remove(Node, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {'DOWN', Ref, process, _, _}  -> % handle node quit situation
            {ok, Down} = intf:name(Ref, Intf),
            io:format("~w: exit recived from ~w~n", [Name, Down]),
            Intf1 = intf:remove(Down, Intf),
%%            Map1 = map:update(Down, Map),
%%            io:format("Map1: ~w~n", [Map1]),
            router(Name, N, Hist, Intf1, Table, Map);
        {links, Node, R, Links} -> % use new node info to update Map
            case hist:update(Node, R, Hist) of
                {new, Hist1} ->
                    intf:broadcast({links, Node, R, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    router(Name, N, Hist1, Intf, Table, Map1);
                old ->
                    router(Name, N, Hist, Intf, Table, Map)
            end;
        {route, Name, From, Message} ->
            io:format("~w: received message ~w ~n", [Name, Message]),
            router(Name, N, Hist, Intf, Table, Map);
        {route, To, From, Message} -> % route Message to target node To, find the next node's pid and send the message
            io:format("~w: routing message (~w)~n", [Name, Message]),
            case dijkstra:route(To, Table) of
                {ok, Gw} ->
                    case intf:lookup(Gw, Intf) of
                    {ok, Pid} ->
                        Pid ! {route, To, From, Message};
                    notfound ->
                        ok
                    end;
                notfound ->
                    ok 
            end,
            router(Name, N, Hist, Intf, Table, Map);
        {send, To, Message} -> % send Message to target node To
            self() ! {route, To, Name, Message},
            router(Name, N, Hist, Intf, Table, Map);
        {status, From} ->
            From ! status,
            router(Name, N, Hist, Intf, Table, Map);
        status ->
            pretty_print(Name, N, Hist, Intf, Table, Map),
            router(Name, N, Hist, Intf, Table, Map);
        update ->
            Table1 = dijkstra:table(intf:list(Intf), Map),
            io:format("Table1: ~w~n", [Table1]),
            router(Name, N, Hist, Intf, Table1, Map);
%%            io:format("Map: ~w~n", [Map]);
        broadcast ->
            Message = {links, Name, N, intf:list(Intf)},
            intf:broadcast(Message, Intf),
            router(Name, N+1, Hist, Intf, Table, Map);
        {updatemap, Node} ->
            Map1 = map:update(Node, Map),
            router(Name, N, Hist, Intf, Table, Map1);
        stop -> ok
    end.

pretty_print(Name, N, Hist, Intf, Table, Map) ->
    io:format("Name: ~s~n", [Name]),
    io:format("Hist: ~w~n", [Hist]),
    io:format("Intf: ~w~n", [Intf]),
    io:format("Table: ~w~n", [Table]),
    io:format("Map: ~w~n", [Map]).
    


