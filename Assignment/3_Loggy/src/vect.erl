-module(vect).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() -> % [{john, 3}, {ringo, 2}, {paul, 4}, {george, 1}]
    [].

inc(Name, Time) -> % Increment the time list of Name Node
    case lists:keyfind(Name, 1, Time) of
        {Name, Ti} ->
            lists:keyreplace(Name, 1, Time, {Name, Ti + 1});
        false ->
            [{Name, 1} | Time]
    end.

merge([], Time) ->
    Time;
merge([{Name, Ti} | Rest], Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            [{Name, max(Ti, Tj)} | merge(Rest, lists:keydelete(Name, 1, Time))];
        false ->
            [{Name, Ti} | merge(Rest, Time)]
    end.

%% determine if a vector clock is smaller than the other one
leq([], _) ->
    true;
leq([{Name, Ti} | Rest], Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            if
                Ti =< Tj ->
                    leq(Rest, Time)
            end;
        false ->
            false
    end.

% setup the clock
clock(_) ->
    [].

% use Time(from receiving msg) to update Clock
update(From, Time, Clock) ->
    case lists:keyfind(From, 1, Clock) of
        {From, _} ->
            lists:keyreplace(From, 1, Clock, {From, lists:keyfind(From, 1, Time)});
        false ->
            [{From, lists:keyfind(From, 1, Time)} | Clock]
    end.

safe(Time, Clock) ->
    leq(Time, Clock).

