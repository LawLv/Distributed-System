-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1, update/2]). % 新建，更新，查询，查询

new() -> % generate an empty map
    [].

update(Node, Links, Map) -> % update Node and Links in Map
    case lists:keyfind(Node, 1, Map) of
        {_, _} ->
            lists:keyreplace(Node, 1, Map, {Node, Links});
        false -> 
            lists:append([{Node, Links}], Map)
    end.



update(Node, Map) ->
    % 1. 删除 Map 中 Node 自身
    Map1 = remove_node(Node, Map),

    % 2. 删除其他节点的连接中对 Node 的引用
    update_links(Node, Map1).

% 删除 Map 中 Node 本身
remove_node(_Node, []) ->
    [];  % 如果 Map 为空，返回空列表
remove_node(Node, [{Node, _Links} | Rest]) ->
    remove_node(Node, Rest);  % 找到 Node，删除它
remove_node(Node, [Entry | Rest]) ->
    [Entry | remove_node(Node, Rest)].  % 未找到，保留条目并递归继续查找

% 删除所有节点的连接列表中指向 Node 的连接
update_links(_Node, []) ->
    [];  % 如果 Map 为空，返回空列表
update_links(Node, [{N, Links} | Rest]) ->
    % 删除当前节点 N 的 Links 列表中指向 Node 的连接
    UpdatedLinks = delete_link(Node, Links),
    [{N, UpdatedLinks} | update_links(Node, Rest)].

% 从 Links 列表中删除指向 Node 的连接
delete_link(_Node, []) ->
    [];  % 如果 Links 为空，返回空列表
delete_link(Node, [Node | Rest]) ->
    delete_link(Node, Rest);  % 找到 Node，删除它
delete_link(Node, [Link | Rest]) ->
    [Link | delete_link(Node, Rest)].  % 未找到，保留连接并递归继续查找




reachable(Node, Map) -> % give list of nodes reachable from Node
    case lists:keyfind(Node, 1 , Map) of
        {_, Links} ->
            Links;
        false ->
            []
    end.

all_nodes(Map) -> % give all nodes in Map
    Set = sets:from_list(extract_all_nodes(Map)),
    sets:to_list(Set).


extract_all_nodes([]) ->
    [];
extract_all_nodes([Item|RO]) ->
    {Node, Links} = Item,
    [Node] ++ Links ++ extract_all_nodes(RO).