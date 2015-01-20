-module(agent_test).
-include_lib("eunit/include/eunit.hrl").
-export([maps_put/3,
         maps_get/2,
         maps_get_and_update/2]).

agent_test_() ->
    {setup,
     fun() ->
             ok
     end,
     fun(_) ->
             [
              {"start_link/2",
               fun() ->
                       {ok, Pid} = agent:start_link(fun() -> maps:new() end),
                       ?assertEqual(true, lists:member(Pid, erlang:element(2, erlang:process_info(self(), links)))),
                       ?assertEqual(ok, agent:update(Pid, fun(Map) -> maps:put(<<"key">>, <<"value">>, Map) end)),
                       ?assertEqual(<<"value">>, agent:get(Pid, fun(Map) -> maps:get(<<"key">>, Map, []) end)),
                       ?assertEqual(<<"value">>, 
                                    agent:get_and_update(Pid,
                                                         fun(Map) ->
                                                                 {maps:get(<<"key">>, Map, []),
                                                                  maps:remove(<<"key">>, Map)}
                                                         end)
                                   ),
                       ?assertEqual(#{}, agent:get(Pid, fun(Map) -> Map end)),
                       ?assertEqual(ok, catch agent:stop(Pid)),
                       wait_until_dead(Pid)
               end},
              {"start/2",
               fun() ->
                       {ok, Pid} = agent:start(maps, new, []),
                       ?assertEqual(ok, agent:update(Pid, ?MODULE, maps_put, [<<"key">>, <<"value">>])),
                       ?assertEqual(<<"value">>, agent:get(Pid, ?MODULE, maps_get, [<<"key">>])),
                       ?assertEqual(<<"value">>, agent:get_and_update(Pid, ?MODULE, maps_get_and_update, [<<"key">>])),
                       ?assertEqual(ok, catch agent:stop(Pid)),
                       wait_until_dead(Pid)
               end}               
             ]
     end
    }.

wait_until_dead(Pid) ->
    case erlang:is_process_alive(Pid) of
        true ->
            wait_until_dead(Pid);
        _ ->
            ok
    end.

maps_put(Map, Key, Value) ->
    maps:put(Key, Value, Map).

maps_get(Map, Key) ->
    maps:get(Key, Map, []).

maps_get_and_update(Map, Key) ->
    {maps:get(Key, Map), maps:remove(Key, Map)}.
