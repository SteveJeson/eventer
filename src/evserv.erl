%%%-------------------------------------------------------------------
%%% @author zdzc
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 三月 2018 15:53
%%%-------------------------------------------------------------------
-module(evserv).
-author("zdzc").
-compile(export_all).

%% API
%%-export([]).

-record(state, {events, %%#event{}记录列表
  clients}). %%pids列表

-record(event, {name="",
  description="",
  pid,
  timeout={{1970,1,1},{0,0,0}}}).

start() ->
  register(?MODULE, Pid=spawn(?MODULE, init, [])),
  Pid.

start_link() ->
  register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
  Pid.

terminate() ->
  ?MODULE ! shutdown.

subscribe(Pid) ->
  Ref = erlang:monitor(process, whereis(?MODULE)),
  ?MODULE ! {self(), Ref, {subscribe, Pid}},
  receive
    {Ref, ok} ->
      {ok, Ref};
    {'DOWN', Ref, process, _Pid, Reason} ->
      {error, Reason}
  after 5000 ->
    {error, timeout}
  end.

add_event(Name, Description, TimeOut) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
  receive
    {Ref, Msg} -> Msg
  after 5000 ->
    {error, timeout}
  end.

add_event2(Name, Description, TimeOut) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {add, Name, Description,TimeOut}},
  receive
    {Ref, {error, Reason}} -> erlang:error(Reason);
    {Ref, Msg} -> Msg
  after 5000 ->
    {error, timeout}
  end.

cancel(Name) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {cancel, Name}},
  receive
    {Ref, ok} -> ok
  after 5000 ->
    {error, timeout}
  end.

listen(Delay) ->
  receive
    M = {done, _Name, _Description} ->
      [M | listen(0)]
  after Delay*1000 ->
    []
  end.

loop(S = #state{}) ->
  receive
    {Pid, MsgRef, {subscribe, Client}} ->
      Ref = erlang:monitor(process, Client),
      NewClients = orddict:store(Ref, Client, S#state.clients),
      Pid ! {MsgRef, ok},
      loop(S#state{clients=NewClients});
    {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
      case valid_datetime(TimeOut) of
        true ->
          EventPid = event:start_link(Name, TimeOut),
          NewEvents = orddict:store(Name,
            #event{name=Name,
              description = Description,
              pid = EventPid,
              timeout = TimeOut},
            S#state.events),
          Pid ! {MsgRef, ok},
          loop(S#state{events = NewEvents});
        false ->
          Pid ! {MsgRef, {error, bad_timeout}},
          loop(S)
      end;
    {Pid, MsgRef, {cancel, Name}} ->
      Events = case orddict:find(Name, S#state.events) of
                 {ok, E} ->
                   event:cancel(E#event.pid),
                   orddict:erase(Name, S#state.events);
                 error ->
                   S#state.events
               end,
      Pid ! {MsgRef, ok},
      loop(S#state{events = Events});
    {done, Name} ->
      case orddict:find(Name, S#state.events) of
        {ok, E} ->
          send_to_clients({done, E#event.name, E#event.description},
            S#state.clients),
          NewEvents = orddict:erase(Name, S#state.events),
          loop(S#state{events=NewEvents});
        error ->
          %% 事件取消的同时，超时也被触发了，会进入这个分支
          loop(S)
      end;
    shutdown ->
      exit(shutdown);
    {'DOWN', Ref, process, _Pid, _Reason} ->
      loop(S#state{clients = orddict:erase(Ref, S#state.clients)});
    code_change ->
      ?MODULE:loop(S);
    Unknown ->
      io:format("Unknown message: ~p~n", [Unknown]),
      loop(S)
  end.

init() ->
  %% 从静态文件中加载事件的逻辑可以放这里
  %% 需要给init函数传递一个参数，用来指定从哪个文件中寻找事件。然后即可进行加载
  %% 还可以通过这个函数直接把事件传递给服务器
  loop(#state{events = orddict:new(),
    clients = orddict:new()}).

valid_datetime({Date,Time}) ->
  try
      calendar:valid_date(Date) andalso valid_time(Time)
  catch
      error:function_clause ->
        false
  end;
valid_datetime(_) ->
  false.

valid_time({H,M,S}) -> valid_time(H,M,S).
valid_time(H,M,S) when
  H >= 0, H < 24,
  M >= 0, M < 60,
  S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.

send_to_clients(Msg, ClientDict) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).