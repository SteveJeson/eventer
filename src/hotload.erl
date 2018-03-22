%%%-------------------------------------------------------------------
%%% @author zdzc
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 三月 2018 17:05
%%%-------------------------------------------------------------------
-module(hotload).
-author("zdzc").

%% API
-export([server/1]).

server(State) ->
  receive
    update -> NewState = ?MODULE:upgrade(State),
      ?MODULE:server(NewState); %% 进入模块的新版本循环中
    SomeMessage ->
      %% 完成一些处理
      server(State) %% 保持在进程当前运行的模块版本中
  end.


