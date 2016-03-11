%%% Raspberry PI Sense Hat Port integration
%%% morten.tenum@gmail.com

-module(sensehat).
-export([start/0, stop/0]).
-export([set_pixel/5, set_pixel/3]).


start() ->
case erl_ddll:load_driver(".", sensehat_drv) of
	ok -> ok;
	{error, already_loaded} -> ok;
	{error, Reason} -> 
    	{error, erl_ddll:format_error(Reason)} 
end,

spawn(?MODULE, init, []).

init() ->
	register(sensehat, self()),
	Port = open_port({spawn, sensehat_drv}, [use_stdio]),
	loop(Port).

set_pixel(X, Y, RGB) ->
	set_pixel(X, Y, RGB bsr 16, (RGB bsr 8) band 16#ff, RGB band 16#ff).

set_pixel(X, Y, R, G, B) ->
	call_port({set_pixel, X, Y, R, G, B}).

call_port(Msg) ->
	sensehat ! {call, self(), Msg},
	receive
		{sensehat, Result} ->
			Result
	end.

stop() ->
	sensehat ! stop.

loop(Port) ->
	receive
		{call, Caller, Msg} ->
			io:format("send to port ~p ~n", [Msg]),
			Port ! {self(), {command, encode(Msg)}},
			receive
				{Port, {data, Data}} ->
					Caller ! {sensehat, decode(Data)}
			end,
			loop(Port);
		stop ->
			Port ! {self(), close},
			receive
				{Port, closed} ->
					exit(normal)
			end;
		{'EXIT', Port, Reason} ->
			io:format("~p ~n", [Reason]),
			exit(port_terminated)
	end.

encode({set_pixel, X, Y, R, G, B}) -> [1, X, Y, R, G, B].

decode([Int]) -> Int.