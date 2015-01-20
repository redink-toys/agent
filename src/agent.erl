-module(agent).

-compile(export_all).

start(Fun) when erlang:is_function(Fun) ->
    start(Fun, []).

start(Fun, Option) when erlang:is_function(Fun) ->
    agent_server:start(Fun, Option).

start_link(Fun) when erlang:is_function(Fun) ->
    start_link(Fun, []).

start_link(Fun, Option) when erlang:is_function(Fun) ->
    agent_server:start_link(Fun, Option).

start(Module, Fun, Args) ->
    start(Module, Fun, Args, []).

start(Module, Fun, Args, Option) ->
    agent_server:start({Module, Fun, Args}, Option).

start_link(Module, Fun, Args) ->
    start_link(Module, Fun, Args, []).

start_link(Module, Fun, Args, Option) ->
    agent_server:start_link({Module, Fun, Args}, Option).


get(Agent, Fun) when erlang:is_function(Fun) ->
    get(Agent, Fun, 5000).
get(Agent, Fun, TimeOut) when erlang:is_function(Fun) ->
    gen_server:call(Agent, {get, Fun}, TimeOut).

get(Agent, Module, Fun, Args) ->
    get(Agent, Module, Fun, Args, 5000).
get(Agent, Module, Fun, Args, TimeOut) ->
    gen_server:call(Agent, {get, {Module, Fun, Args}}, TimeOut).


get_and_update(Agent, Fun) when erlang:is_function(Fun) ->
    get_and_update(Agent, Fun, 5000).
get_and_update(Agent, Fun, TimeOut) when erlang:is_function(Fun) ->
    gen_server:call(Agent, {get_and_update, Fun}, TimeOut).

get_and_update(Agent, Module, Fun, Args) ->
    get_and_update(Agent, Module, Fun, Args, 5000).
get_and_update(Agent, Module, Fun, Args, TimeOut) ->
    gen_server:call(Agent, {get_and_update, {Module, Fun, Args}}, TimeOut).


update(Agent, Fun) when erlang:is_function(Fun) ->
    update(Agent, Fun, 5000).
update(Agent, Fun, TimeOut) when erlang:is_function(Fun) ->
    gen_server:call(Agent, {update, Fun}, TimeOut).

update(Agent, Module, Fun, Args) ->
    update(Agent, Module, Fun, Args, 5000).
update(Agent, Module, Fun, Args, TimeOut) ->
    gen_server:call(Agent, {update, {Module, Fun, Args}}, TimeOut).


cast(Agent, Fun) when erlang:is_function(Fun) ->
    gen_server:cast(Agent, {cast, Fun}).

cast(Agent, Module, Fun, Args) ->
    gen_server:cast(Agent, {cast, {Module, Fun, Args}}).


stop(Agent) ->
    stop(Agent, 5000).
stop(Agent, TimeOut) ->
    gen_server:call(Agent, stop, TimeOut).
