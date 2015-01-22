%%%-------------------------------------------------------------------
%%% @author redink
%%% @copyright (C) 2015 , redink
%%%
%%%
%%%
%%% Created :  by redink
%%%-------------------------------------------------------------------

-module(agent).

-export([start/1,
         start/2,
         start_link/1,
         start_link/2,
         start/3,
         start/4,
         start_link/3,
         start_link/4,

         get/2,
         get/3,
         get/4,
         get/5,

         get_and_update/2,
         get_and_update/3,
         get_and_update/4,
         get_and_update/5,

         update/2,
         update/3,
         update/4,
         update/5,

         cast/2,
         cast/4,

         stop/1,
         stop/2]).

-type start_options() :: [start_option()].

-type start_option()  :: {debug, debug_option()}      |
                         {timeout, timeout()} |
                         {spawn_opt, spawn_option()}.

-type debug_option()  :: [trace                       |
                          log                         |
                          statistics                  |
                          {log_to_file, string()}     |
                          {install, {fun(), term()}}].

-type spawn_option()  :: [term()].

-spec start(fun()) -> {ok, pid()}.

start(Fun) when erlang:is_function(Fun) ->
    start(Fun, []).

-spec start(fun(), start_options()) -> {ok, pid()}.

start(Fun, Option) when erlang:is_function(Fun) ->
    agent_server:start(Fun, Option).

-spec start_link(fun()) -> {ok, pid()}.

start_link(Fun) when erlang:is_function(Fun) ->
    start_link(Fun, []).

-spec start_link(fun(), start_options()) -> {ok, pid()}.

start_link(Fun, Option) when erlang:is_function(Fun) ->
    agent_server:start_link(Fun, Option).

-spec start(atom(), atom(), [term()]) -> {ok, pid()}.

start(Module, Fun, Args) ->
    start(Module, Fun, Args, []).

-spec start(atom(), atom(), [term()], start_options()) -> {ok, pid()}.

start(Module, Fun, Args, Option) ->
    agent_server:start({Module, Fun, Args}, Option).

-spec start_link(atom(), atom(), [term()]) -> {ok, pid()}.

start_link(Module, Fun, Args) ->
    start_link(Module, Fun, Args, []).

-spec start_link(atom(), atom(), [term()], start_options()) -> {ok, pid()}.

start_link(Module, Fun, Args, Option) ->
    agent_server:start_link({Module, Fun, Args}, Option).

-spec get(pid(), fun()) -> term().

get(Agent, Fun) when erlang:is_function(Fun) ->
    get(Agent, Fun, 5000).

-spec get(pid(), fun(), timeout()) -> term().

get(Agent, Fun, TimeOut) when erlang:is_function(Fun) ->
    gen_server:call(Agent, {get, Fun}, TimeOut).

-spec get(pid(), atom(), atom(), [term()]) -> term().

get(Agent, Module, Fun, Args) ->
    get(Agent, Module, Fun, Args, 5000).

-spec get(pid(), atom(), atom(), [term()], timeout()) -> term().

get(Agent, Module, Fun, Args, TimeOut) ->
    gen_server:call(Agent, {get, {Module, Fun, Args}}, TimeOut).

-spec get_and_update(pid(), fun()) -> term().

get_and_update(Agent, Fun) when erlang:is_function(Fun) ->
    get_and_update(Agent, Fun, 5000).

-spec get_and_update(pid(), fun(), timeout()) -> term().

get_and_update(Agent, Fun, TimeOut) when erlang:is_function(Fun) ->
    gen_server:call(Agent, {get_and_update, Fun}, TimeOut).

-spec get_and_update(pid(), atom(), atom(), [term()]) -> term().

get_and_update(Agent, Module, Fun, Args) ->
    get_and_update(Agent, Module, Fun, Args, 5000).

-spec get_and_update(pid(), atom(), atom(), [term()], timeout()) -> term().

get_and_update(Agent, Module, Fun, Args, TimeOut) ->
    gen_server:call(Agent, {get_and_update, {Module, Fun, Args}}, TimeOut).

-spec update(pid(), fun()) -> ok.

update(Agent, Fun) when erlang:is_function(Fun) ->
    update(Agent, Fun, 5000).

-spec update(pid(), fun(), timeout()) -> ok.

update(Agent, Fun, TimeOut) when erlang:is_function(Fun) ->
    gen_server:call(Agent, {update, Fun}, TimeOut).

-spec update(pid(), atom(), atom(), [term()]) -> ok.

update(Agent, Module, Fun, Args) ->
    update(Agent, Module, Fun, Args, 5000).

-spec update(pid(), atom(), atom(), [term()], timeout()) -> ok.

update(Agent, Module, Fun, Args, TimeOut) ->
    gen_server:call(Agent, {update, {Module, Fun, Args}}, TimeOut).

-spec cast(pid(), fun()) -> ok.

cast(Agent, Fun) when erlang:is_function(Fun) ->
    gen_server:cast(Agent, {cast, Fun}).

-spec cast(pid(), atom(), atom(), [term()]) -> ok.

cast(Agent, Module, Fun, Args) ->
    gen_server:cast(Agent, {cast, {Module, Fun, Args}}).

-spec stop(pid()) -> ok.

stop(Agent) ->
    stop(Agent, 5000).

-spec stop(pid(), timeout()) -> ok.

stop(Agent, TimeOut) ->
    gen_server:call(Agent, stop, TimeOut).
