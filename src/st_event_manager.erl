-module(st_event_manager).
-export([start_link/0, subscribe/0]).

start_link() ->
	gen_event:start_link({local,?MODULE}).

subscribe() ->
	HandlerId = {st_event, make_ref()},
	gen_event:add_handler(st_event_manager, HandlerId, [self()]),
    HandlerId.