# agent
`Agent` is an [Elixir Agent](http://elixir-lang.org/getting_started/mix_otp/2.html#2.2-agents) model partial implement in Erlang.

Detailed doc, [here](https://github.com/elixir-lang/elixir/blob/v1.0.1/lib/elixir/lib/agent.ex).

The Agent module provides a basic server implementation that allows state to be **retrieved** and **updated** via a **simple** API.

## usage
### compile and eunit
	$ ./rebar com ; ./rebar eunit -v
	==> agent (compile)
	Compiled src/agent_server.erl
	Compiled src/agent.erl
	==> agent (eunit)
	INFO:  sh info:
		cwd: "/Users/redink/github/agent"
		cmd: cp -R src/agent_server.erl src/agent.erl test/agent_test.erl ".eunit"
	Compiled src/agent_server.erl
	Compiled src/agent.erl
	Compiled test/agent_test.erl
	INFO:  Cover compiling /Users/redink/github/agent
	======================== EUnit ========================
	module 'agent_test'
	  agent_test: agent_test_ (start_link/2)...[0.004 s] ok
	  agent_test: agent_test_ (start/2)...ok
	  [done in 0.010 s]
	module 'agent_server'
	module 'agent'
	=======================================================
	  All 2 tests passed.
	Cover analysis: /Users/redink/github/agent/.eunit/index.html
	
### start
	$ erl -pa ./ebin
	
### e.g.

	1> {ok, Agent} = agent:start(maps, new, []).
	{ok,<0.34.0>}
	2> agent:update(Agent, fun(Map) -> maps:put('key', 'value', Map) end).
	ok
	3> agent:get(Agent, fun(Map) -> maps:get('key', Map) end).
	value
	
## example
coming soon