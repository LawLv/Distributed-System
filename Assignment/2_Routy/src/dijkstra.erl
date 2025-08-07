-module(dijkstra).
-export([table/2, route/2]).
% sorted: {Node, N, Gateway}, ranked by path length

entry(Node, Sorted) -> % find Node's shortest path, return length
    case lists:keyfind(Node, 1, Sorted) of
        {_, N, _} ->
            N;
        false ->
            0
    end.

replace(Node, N, Gateway, Sorted) -> % replace the specific node's entry, if no then append, then sort
    case lists:keyfind(Node, 1, Sorted) of
        {_, _, _} -> % been found
            S1 = lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway}); % 在sorted中查找第一个元素是Node的元组，替换
        false ->
            S1 = lists:append([{Node, N, Gateway}], Sorted)
    end,
    lists:keysort(2, S1).

update(Node, N, Gateway, Sorted) -> % update N if the new one is smaller
    N1 = entry(Node, Sorted),
    case N < N1 of
        true ->
            replace(Node, N, Gateway, Sorted);
        false ->
            Sorted
    end.

iterate(Sorted, Map, Table) -> % Table:{Node, Gateway} 目标节点，下一跳; Map: [{berlin,[london,paris]}] 联通信息
    case Sorted of
        [] -> % All nodes are finished
            Table;
        [{Node, N, Gateway}|R1] -> % pick the first element
            case N == inf of
                true -> % Node is unreachable
                    Table;
                false ->
                    Reach_nodes = map:reachable(Node, Map), % pick the neighbor nodes
                    case Reach_nodes of
                        [] -> % no reachable nodes
                            R2 = R1;
                        [_|_] -> % update the information of these neighbors
                            R2 = update_for_each_reachable_node(Reach_nodes, Node, R1) % R2是所有Node通到的节点都将Node作为网关
                    end,
                    iterate(R2, Map, Table ++ [{Node, Gateway}])
            end
    end.
update_for_each_reachable_node([], _, _) ->
    [];
update_for_each_reachable_node([R_node], Node, Sorted) ->
    update(R_node, 1, Node, Sorted);
update_for_each_reachable_node([R_node|Rest], Node, Sorted) ->
    S2 = update(R_node, 1, Node, Sorted),
    update_for_each_reachable_node(Rest, Node, S2).



table(Gateways, Map) -> % {Node, N, Gateway}: {Node, inf, unknown}; Gateway node's gateway is itself, N = 0
    All_nodes = map:all_nodes(Map),
    Fun1 = fun(Node) -> {Node, inf, unknown} end,
    S1 = lists:map(Fun1, All_nodes),
    Fun2 = fun(Node) -> {Node, 0, Node} end,
    S2 = lists:map(Fun2, Gateways),
    Sorted = S2++S1,

    iterate(Sorted, Map, []).


route(Node, Table) -> % 返回Node节点的上一跳
    case lists:keyfind(Node, 1, Table) of
        {Node, Gateway} ->
            {ok, Gateway};
        false -> 
            notfound
    end.
