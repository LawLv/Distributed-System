-module(mlogger).
-export([start/1, stop/1]).

start(Nodes) -> % Create a child process
  spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) -> % a recursive message receiving loop
%%  Clock = time:clock(Nodes),
  Clock = vect:clock(Nodes),
  loop(Clock, [], 0).

loop(Clock, Queue, MaxQueueLength) ->
  receive
    {log, From, Time, Msg} ->
%%      Inc_clock = time:update(From, Time, Clock),
      Inc_clock = vect:update(From, Time, Clock),
      Q1 = lists:keysort(2, lists:append([{From, Time, Msg}], Queue)), % append {From, Time, Msg} to queue and rank based on Time
      Q2 = holdbackqueue(Inc_clock, Q1), % delete msg handleable
      NewMaxQueueLength = max(MaxQueueLength, length(Q2)),
      loop(Inc_clock, lists:keysort(2, Q2), NewMaxQueueLength);
    stop ->
      io:format("queue remains: ~w~n", [length(Queue)]),
      io:format("Max Holdback Queue Length: ~w~n", [MaxQueueLength]),
      ok
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

holdbackqueue(_, []) ->
  [];
holdbackqueue(Clock, [{From, Time, Msg}|Rest]) ->
%%  io:format("Holding message from ~p with time ~p~n", [From, Time]),
%%  case time:safe(Time, Clock) of
  case vect:safe(Time, Clock) of
    [true, true, true, true] ->
      log(From, Time, Msg),
      holdbackqueue(Clock, Rest);
    _ ->
      holdbackqueue(Clock, Rest) ++ [{From, Time, Msg}]
  end.