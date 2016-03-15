-module(st_supervisor).
-behaviour(supervisor).
-export([init/1]).
-export([start_link/0]).

start_link() ->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).

init(_Args) ->
	{ok, {
			{one_for_one, 3, 60},
			[
				{st_event_manager, {st_event_manager, start_link, []}, permanent, 1000, worker, [st_event_manager]},
				{st_port, {st_port, start_link, []}, permanent, 1000, worker, [st_port]}
			 ]
		}
	}.