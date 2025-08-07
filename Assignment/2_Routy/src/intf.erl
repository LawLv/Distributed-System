-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() ->
    [].

add(Name, Ref, Pid, Intf) -> % add a new interface
    [{Name, Ref, Pid}] ++ Intf.

remove(Name, Intf) -> % remove an interface called "Name"
    lists:keydelete(Name, 1, Intf).


lookup(Name, Intf) -> % use name to find Pid and return {ok, Pid}
    case lists:keyfind(Name, 1, Intf) of
        {_, _, Pid} ->
            {ok, Pid};
        false -> 
            notfound
    end.

ref(Name, Intf) -> % find the reference of "Name"
    case lists:keyfind(Name, 1, Intf) of
        {_, Ref, _} ->
            {ok, Ref};
        false -> 
            notfound
    end.

name(Ref, Intf) -> % find the name of "Ref" interface
    case lists:keyfind(Ref, 2, Intf) of
        {Name, _, _} ->
            {ok, Name};
        false -> 
            notfound
    end.

list(Intf) -> % list all the interfaces' name
    Set = sets:from_list(extract_all_names(Intf)),
    sets:to_list(Set).

extract_all_names([]) ->
    [];
extract_all_names([Item|RO]) ->
    {Name, _, _} = Item,
    [Name] ++ extract_all_names(RO).

broadcast(Message, Intf) ->
    All_processes = list(Intf), % get all the names
    Fun2 = fun({_, Pid}) ->
        Pid ! Message % send Message to Pid Process
           end,
    lists:map(fun(X) -> Fun2(lookup(X, Intf)) end, All_processes).
