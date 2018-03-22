%%%-------------------------------------------------------------------
%%% @author zdzc
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 三月 2018 14:56
%%%-------------------------------------------------------------------
-module(event).
-author("zdzc").
-compile(export_all).
-record(state, {server, name="",to_go=0}).

%% API
%%-export([]).

loop(S = #state{server=Server, to_go=[T|Next]}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
  after T*1000 ->
    if Next =:= [] ->
      Server ! {done, S#state.name};
      Next =/= [] ->
        loop(S#state{to_go=Next})
    end
  end.

start(EventName, DateTime) ->
  spawn(?MODULE, init, [self(), EventName, DateTime]).

start_link(EventName, DateTime) ->
  spawn(?MODULE, init, [self(), EventName, DateTime]).

%%% 事件模块的内部实现
init(Server, EventName, DateTime) ->
  loop(#state{server=Server,
    name=EventName,
    to_go=time_to_go(DateTime)}).

normalize(N) ->
  Limit = 49*24*60*60,
  [N rem Limit | lists:duplicate(N div Limit, Limit)].

cancel(Pid) ->
%%  设置监控器， 以免进程已经死亡了
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, proces, Pid, _Reason} ->
      ok
  end.

time_to_go(TimeOut={{_,_,_},{_,_,_}}) ->
  Now = calendar:local_time(),
  ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
  calendar:datetime_to_gregorian_seconds(Now),
  Secs = if ToGo > 0 -> ToGo;
           ToGo =< 0 -> 0
         end,
  normalize(Secs).
