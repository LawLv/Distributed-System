-module(testCountry).
-export([run/0]).

run() ->
  % 启动瑞典节点并注册它们
  net_adm:ping('france@192.168.5.4'),  % 确保与法国节点连接

  % 添加网络连接
  {ok, R1_SE} = rpc:call('sweden@192.168.5.4', routy, start, [r1, stockholm]),
  {ok, R2_SE} = rpc:call('sweden@192.168.5.4', routy, start, [r2, lund]),
  {ok, R3_SE} = rpc:call('sweden@192.168.5.4', routy, start, [r3, uppsala]),

  % 启动法国节点并注册它们
  {ok, R1_FR} = rpc:call('france@192.168.5.4', routy, start, [r1, paris]),
  {ok, R2_FR} = rpc:call('france@192.168.5.4', routy, start, [r2, lyon]),

  % 建立节点之间的连接
  R1_SE ! {add, lund, {r2, 'sweden@192.168.5.4'}},
  R2_SE ! {add, uppsala, {r3, 'sweden@192.168.5.4'}},
  R3_SE ! {add, paris, {r1, 'france@192.168.5.4'}},
  R1_FR ! {add, lyon, {r2, 'france@192.168.5.4'}},
  R2_FR ! {add, stockholm, {r1, 'sweden@192.168.5.4'}},

  timer:sleep(timer:seconds(1)),

  % 广播连通信息并更新路由表
  R1_SE ! broadcast,
  R2_SE ! broadcast,
  R3_SE ! broadcast,
  R1_FR ! broadcast,
  R2_FR ! broadcast,

  timer:sleep(timer:seconds(1)),

  % 更新每个节点的路由表
  R1_SE ! update,
  R2_SE ! update,
  R3_SE ! update,
  R1_FR ! update,
  R2_FR ! update,

  timer:sleep(timer:seconds(1)),

  % 发送消息，验证网络的连通性
  io:format("Sending message from Stockholm to Lyon~n"),
  R1_SE ! {route, lyon, stockholm, "Hello from Stockholm to Lyon"},

  timer:sleep(timer:seconds(1)),

  io:format("Sending message from Paris to Uppsala~n"),
  R1_FR ! {route, uppsala, paris, "Hello from Paris to Uppsala"}.
