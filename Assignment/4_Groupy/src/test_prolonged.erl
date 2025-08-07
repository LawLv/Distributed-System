-module(test_prolonged).
-compile(export_all).
-define(TIMEOUT, 1000). % 定义基本的时间间隔 (1秒)

% 开始测试
start_test(Module, Sleep) ->
    io:format("Starting test with leader and two workers~n"),
    % 创建领导者和从节点 2 和 3
    W1 = first(1, Module, Sleep),
    W2 = add(2, Module, W1, Sleep),
    W3 = add(3, Module, W1, Sleep),
    timer:sleep(5000), % 等待 5 秒
    % 删除节点 3,  [1,2]
    io:format("Removing node 3~n"),
    stop(W3),
    timer:sleep(5000), % 等待 5 秒
    % 加入节点 4,  [1,2,4]
    io:format("Adding node 4~n"),
    W4 = add(4, Module, W2, Sleep),
    timer:sleep(5000), % 等待 5 秒
    % 加入节点 5,  [1,2,4,5]
    io:format("Adding node 5~n"),
    W5 = add(5, Module, W4, Sleep),
    timer:sleep(5000), % 等待 5 秒
    % 删除节点 5,  [1,2,4]
    io:format("Removing node 5~n"),
    stop(W5),
    timer:sleep(5000), % 等待 5 秒
    % 加入节点 6, [1,2,4,6]
    io:format("Adding node 6~n"),
    W6 = add(6, Module, W4, Sleep),
    timer:sleep(5000), % 等待 5 秒
    % 加入节点 7, [1,2,4,6,7]
    io:format("Adding node 7~n"),
    W7 = add(7, Module, W6, Sleep),
    timer:sleep(10000), % 等待 10 秒
    % 结束测试
    io:format("Test completed~n"),
    ok.

% First worker creation
first(N, Module, Sleep) ->
    worker:start(N, Module, random:uniform(256), Sleep).

% Add a new worker to the system
add(N, Module, Wrk, Sleep) ->
    worker:start(N, Module, random:uniform(256), Wrk, Sleep).

% Stop worker
stop(Wrk) ->
    Wrk ! stop.
