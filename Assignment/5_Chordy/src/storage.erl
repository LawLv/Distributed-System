% local storage
% represented as a list of tuples {Key, Value}

-module(storage).
-compile(export_all).

create() ->  % create a new store
  [].

add(Key, Value, Store) ->  % add a Key-Value pair to store
  [{Key, Value} | Store].

lookup(Key, Store) ->  % check if a Key is in the Store
  lists:keyfind(Key, 1, Store).

split(From, To, Store) ->  % return a tuple {Updated, Rest}, Updated means pairs within From~to, Rest are the rest
  lists:partition(fun({Key,Value})-> key:between(Key, From, To) end, Store).

merge(Entries, Store) -> % add a list of key-value pairs to a store
   Store ++ Entries.