-module(st_event).
-behaviour(gen_event).

-export([code_change/3, handle_call/2, handle_event/2, handle_info/2, init/1, terminate/2]).

init([Pid]) -> {ok,Pid}.

handle_event(Event, Pid) ->
	Pid ! {st_event, Event},
	{ok, Pid}.

handle_call(_Request, State) -> {ok, ok, State}.

handle_info(_Info, State) -> {ok, State}.

terminate(_Arg, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.