-module(hist).
-export([new/1, update/3]).

new(Name) -> % new a hist for a node, -1 means no history
    [{Name, -1}].

update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
        {_, Latest_N} -> % find the Node's latest hist value
            case N > Latest_N of
                true -> 
                    {new, lists:keyreplace(Node, 1, History, {Node, N})};
                false ->
                    old
            end;
        false ->
            {new, [{Node, N}|History]}
    end.