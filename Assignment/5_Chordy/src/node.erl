-module(node).
-compile(export_all).

-define(Stabilize, 100).
-define(Timeout, 1000).

start(Id) ->  % the first node
  start(Id, nil).

start(Id, Peer) ->  % connecting to an existing ring
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor, storage:create(), nil).

connect(Id, nil) ->   % if we are the first node
  {ok, {Id, nil, self()}};   % we are our own successor

connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},  % ask for Peer's key: Skey
    receive
      {Qref, Skey} ->
        Sref = monitor(Peer),   % monitor the successor, if fail return {'DOWN', Ref, process, Pid, Reason}
        {ok, {Skey, Sref, Peer}}
    after ?Timeout ->
      io:format("Time out: no response~n",[])
  end.

% send a stabilize message after som time, so new nodes are quickly linked into the ring
% called on when a node is created
schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).   % sends a stabilize message to self() repeatedly after ?Stabilize milliseconds

% a node has a key, a predecessor and a successor
node(Id, Predecessor, Successor, Store, Next) ->
    receive
      {key, Qref, Peer} ->  % peer wants to know our key (Id)
          Peer ! {Qref, Id},
          node(Id, Predecessor, Successor, Store, Next);
      {notify, New} ->   % notification from a possible new predecessor
          {Pred, UpdatedStore} = notify(New, Id, Predecessor, Store),  % {(new)Pred node, (new)Store}
          node(Id, Pred, Successor, UpdatedStore, Next);
      {request, Peer} ->  % predecessor needs to know our predecessor
          request(Peer, Predecessor, Successor),
          node(Id, Predecessor, Successor, Store, Next);
      {status, Pred, Nx} ->  % our successor informs us about its predecessor
          {Succ, Nxt} = stabilize(Pred, Id, Successor, Nx),  % return the suc after check
          node(Id, Predecessor, Succ, Store, Nxt);
      % stabilize message sent from schedule_stabilize
      stabilize ->
          stabilize(Successor),
          node(Id, Predecessor, Successor, Store, Next);
      probe -> % create a probe message
        create_probe(Id, Successor),
        node(Id, Predecessor, Successor, Store, Next);
      {probe, Id, Nodes, T} ->
        remove_probe(T, Nodes),
        node(Id, Predecessor, Successor, Store, Next);
      {probe, Ref, Nodes, T} ->  % the probe is not ours, forward it, add our own pid
        forward_probe(Ref, T, Nodes, Id, Successor),
        node(Id, Predecessor, Successor, Store, Next);
      {add, Key, Value, Qref, Client} ->  % a request to store a new key-value
        Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
        node(Id, Predecessor, Successor, Added, Next);
      % request to lookup a key in the storage
      {lookup, Key, Qref, Client} ->
        lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
        node(Id, Predecessor, Successor, Store, Next);
      {handover, Elements} ->  % 接管Elements
        Merged = storage:merge(Store, Elements),
        node(Id, Predecessor, Successor, Merged, Next);

      % if a node fails
      {'DOWN', Ref, process, _, _} ->
        {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
        node(Id, Pred, Succ, Store, Nxt);

      % state of node
      state ->
        io:format("Id: ~w~n", [Id]),
        io:format("Predecessor: ~p, Successor: ~p, Next: ~p~n", [Predecessor, Successor, Next]),
        node(Id, Predecessor, Successor, Store, Next);

      stop ->
        ok
    end.

% handover some storage
handover(Id, Store, Nkey, Npid) ->
  {Rest, Keep} = storage:split(Id, Nkey, Store),
  Npid ! {handover, Rest},  % Rest means pairs for the new
  Keep.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->   % add a new key-value to the Store
    case key:between(Key, Pkey, Id) of
      true ->
        io:format("Storing key ~p at node with Id: ~p~n", [Key, Id]),
        Client ! {Qref, ok},
        storage:add(Key, Value, Store);   % store the key-value
      false ->
        Spid ! {add, Key, Value, Qref, Client}, % send add message to successor instead
        Store
    end.

% lookup a key in the store and send the reply to the requester
lookup(Key, Qref, Client, Id, {Pkey, _, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
      true ->
        Result = storage:lookup(Key, Store), % get the key if it exist
        io:format("Found key ~p at node with Id: ~p~n", [Key, Id]),
        Client ! {Qref, Result};
      false ->
        {_,_, Spid} = Successor,
        Spid ! {lookup, Key, Qref, Client}
    end.

% create a probe message with a time stamp
create_probe(Id, {_, _, Spid}) ->
  Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

% calculate the time it took to pass a probe message around the ring
remove_probe(Time, Nodes) ->
  T = erlang:system_time(micro_seconds) - Time,
  io:format("Probe time: ~w micro seconds ~n Nodes: ~w", [T, Nodes]).

% forward the probe message to the successor
forward_probe(Ref, Time, Nodes, Id, {_, _, Spid}) ->
  Spid ! {probe, Ref, [Id | Nodes], Time}.

% check if us and successor has legal connection
stabilize(Pred, Id, Successor, Next) ->
  {Skey, Sref, Spid} = Successor,
    case Pred of
      nil ->
        Spid ! {notify, {Id, self()}},  % we suggest ourself as a predecessor
        {Successor, Next};
      {Id, _} ->  % good link
        {Successor, Next};
      {Skey, _} ->  % if the predecessor is pointing to itself
        Spid ! {notify, {Id, self()}},
        {Successor, Next};
      {Xkey, Xpid} ->  % predecessor is pointing to another node
        case key:between(Xkey, Id, Skey) of  % how to position ourself?
            true ->  % current Pred is between us and Successor
              Xpid ! {request, self()},
      		    Xref = monitor(Xpid),  % monitor our new suc
      		    drop(Sref),
      		    {{Xkey, Xref, Xpid}, {Skey, Spid}};
            false ->% we are between the nodes
              Spid ! {notify, {Id, self()}},  % node suggest itself as predecessor
              {Successor, Next}
        end
    end.

% sends request message to its successor
stabilize({_, _, Spid}) ->
  Spid ! {request, self()}.

% inform Peer about the Predecessor
request(Peer, Predecessor, {Skey,Sref,Spid}) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil, {Skey, Spid}};
    {Pkey, _, Ppid} ->
      Peer ! {status, {Pkey, Ppid}, {Skey,Spid}}
  end.

% being notified of a node is a way for a new node {Nkey, Npid} to make a proposal that it might be our predecessor
% decide on and return the predecessor
notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
      nil ->  % we don't have a predecessor
          Keep = handover(Id, Store, Nkey, Npid),  % keep: remaining pairs of old node
          Nref = monitor(Npid),   % monitor the new node
          {{Nkey, Nref, Npid}, Keep};   % new node is predecessor with
      {Pkey, Pref, _} ->  % we already have a predecessor
        % check if new node should be predecessor
        case key:between(Nkey, Pkey, Id) of
          true ->  % accept the new predecessor
            Keep = handover(Id, Store, Nkey, Npid),
            Nref = monitor(Npid),
            drop(Pref),
            {{Nkey, Nref, Npid}, Keep};  % new predecessor from new node, and new storage
          false ->
            {Predecessor, Store}  % no change predecessor
          end
  end.

% monitor a node
monitor(Pid) ->
  erlang:monitor(process, Pid).

% demonitor a node
drop(nil) ->
  ok;
drop(Pid) ->
  erlang:demonitor(Pid, [flush]).

% predecessor has died
down(Ref, {_, Ref, _}, Successor, Next) ->
  {nil, Successor, Next};

% successor has died
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
  % monitor the new successor
io:format("Successor of ~w died~n", [Ref]),
  Nref = monitor(Npid),
  % to repair the ring
  Npid ! stabilize,
  {Predecessor, {Nkey, Nref, Npid}, nil}. % use Next as new successor