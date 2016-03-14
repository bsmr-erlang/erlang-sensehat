-module(pubsub).
-author("Morten Teinum <morten.teinum@gmail.com>").
-export([init/0, subscribe/2, publish/2]).

init() ->
	[].

subscribe(Pid, PS) ->
	PS ++ [Pid].

publish(Msg, PS) ->
	lists:foreach(fun(Pid) -> Pid ! Msg end, PS).