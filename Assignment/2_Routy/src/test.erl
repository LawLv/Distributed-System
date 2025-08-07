-module(test).
-export([run/0]).

run() ->
    routy:start(r1, stockholm),
    routy:start(r2, lund),
    routy:start(r3, uppsala),
    routy:start(r4, gothenburg),
    routy:start(r5, malmo),

    Pid1 = whereis(r1),
    Pid2 = whereis(r2),
    Pid3 = whereis(r3),
    Pid4 = whereis(r4),
    Pid5 = whereis(r5),

    r1 ! {add, lund, Pid2},
    r2 ! {add, stockholm, Pid1},
    r2 ! {add, uppsala, Pid3},
    r3 ! {add, gothenburg, Pid4},
    r3 ! {add, malmo, Pid5},
    r4 ! {add, malmo, Pid5},
    r4 ! {add, lund, Pid2},
    r5 ! {add, stockholm, Pid1},

    timer:sleep(timer:seconds(1)),
    r1 ! broadcast,
    r2 ! broadcast,
    r3 ! broadcast,
    r4 ! broadcast,
    r5 ! broadcast,

    timer:sleep(timer:seconds(1)),
    r1 ! update, % router
    r2 ! update,
    r3 ! update,
    r4 ! update,
    r5 ! update,


    io:format("~n"),
    io:format("Ask lund for status from stockholm~n"),
    r1 ! {status, {r2, 'sweden@192.168.5.4'}},

    timer:sleep(timer:seconds(1)),
    io:format("~n"),
    io:format("Ask stockholm for status from uppsala~n"),
    r3 ! {status, {r1, 'sweden@192.168.5.4'}},

    timer:sleep(timer:seconds(1)),
    io:format("~n"),
    io:format("Send message from stockholm to lund~n"),
    r1 ! {route, lund, stockholm, "Hello"},

    timer:sleep(timer:seconds(1)),
    io:format("~n"),
    io:format("Send message from uppsala to stockholm~n"),
    r3 ! {route, stockholm, uppsala, "Hello"},

    timer:sleep(timer:seconds(1)),
    io:format("~n"),
    io:format("Send message from gothenburg to lund~n"),
    r4 ! {route, lund, gothenburg, "Hello"},
%%
%%    timer:sleep(timer:seconds(1)),

    % r5 下线
    io:format("~n"),
    io:format("Node r5 (malmo) is going offline~n"),



    r5 ! stop,  % 停止 r5 节点
    io:format("Removing malmo......~n"),
    timer:sleep(timer:seconds(1)),
%%%%    r1 ! {remove, malmo},
%%%%    r2 ! {remove, malmo},
%%
%%    io:format("Broadcasting and updating routes......~n"),
%%    r1 ! {updatemap, malmo},
%%    r2 ! {updatemap, malmo},
%%    r3 ! {updatemap, malmo},
%%    r4 ! {updatemap, malmo},
%%
    r1 ! broadcast,
    r2 ! broadcast,
    r3 ! broadcast,
    r4 ! broadcast,
    timer:sleep(timer:seconds(1)),
    r1 ! update,
    r2 ! update,
    r3 ! update,
    r4 ! update,
    timer:sleep(timer:seconds(1)).
%%
%%
%%    io:format("~n"),
%%    io:format("Send message from uppsala to stockholm~n"),
%%    r3 ! {route, stockholm, uppsala, "Hello"}.




%%----------------------------------------------------------------------------------------------------------------------------------------------------------

