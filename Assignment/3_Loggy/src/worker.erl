-module(worker).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) -> % start a worker process
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
  random:seed(Seed, Seed, Seed),
  receive
    {peers, Peers} ->
%%      loop(Name, Log, Peers, Sleep, Jitter, time:zero());
      loop(Name, Log, Peers, Sleep, Jitter, vect:zero());
    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

  loop(Name, Log, Peers, Sleep, Jitter, LogTime)->
  Wait = random:uniform(Sleep), % random sleep time
  receive
    {msg, MTime, Msg} ->
%%      Inc_time = time:inc(Name, time:merge(LogTime, MTime)),
      Inc_time = vect:inc(Name, vect:merge(LogTime, MTime)),
      Log ! {log, Name, Inc_time, {received, Msg}},
      loop(Name, Log, Peers, Sleep,Jitter, Inc_time);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, time, {error, Error}}
  after Wait -> % send message
%%    Inc_time = time:inc(Name, LogTime),
    Inc_time = vect:inc(Name, LogTime),
    Selected = select(Peers),
    Message = {hello, random:uniform(100)},
    Selected ! {msg, Inc_time, Message},
    jitter(Jitter), % Simulate a random jitter before sending logs
    Log ! {log, Name, Inc_time, {sending, Message}},
    loop(Name, Log, Peers, Sleep, Jitter, Inc_time)
  end.

select(Peers) -> % choose an element in Peers
  lists:nth(random:uniform(length(Peers)), Peers).
jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
