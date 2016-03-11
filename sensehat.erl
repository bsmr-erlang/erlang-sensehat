%%% Raspberry PI Sense Hat Port integration
%%% morten.tenum@gmail.com

-module(sensehat).
-export([start/0, stop/0, init/0]).
-export([set_pixel/5, set_pixel/3, clear/0, fill/3, logo/0]).


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

set_pixel(X, Y, R, G, B) -> call_port({set_pixel, X, Y, R, G, B}).

fill(R, G, B) -> call_port({fill, R, G, B}).

clear() -> fill(0, 0, 0).

logo() -> logo([16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff,
16#fcf2f5, 16#ffffff, 16#fdf9fa, 16#eabec4, 16#f9e9ee, 16#ffffff, 16#fbeff2, 16#e9b1c1,
16#fdf7f9, 16#ffffff, 16#e192a7, 16#a70712, 16#da7a94, 16#ffffff, 16#fdf7f8, 16#cc4b6e,
16#ffffff, 16#ffffff, 16#fef9fa, 16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff, 16#e39eb1,
16#ffffff, 16#ffffff, 16#da7d97, 16#ca5b69, 16#d1577d, 16#c94268, 16#bc2342, 16#c93f65,
16#ffffff, 16#ffffff, 16#cc4a6e, 16#930000, 16#9e0000, 16#a60005, 16#bc2444, 16#c73b61,
16#f8e6eb, 16#ffffff, 16#fbeff2, 16#c43658, 16#b31d2a, 16#e6a6b8, 16#ffffff, 16#d56886,
16#f8e6ea, 16#ffffff, 16#ffffff, 16#ffffff, 16#fffdff, 16#ffffff, 16#fefbfc, 16#f0c9d4]).

logo(Arr) -> 
	P = fun(RGB, XY) ->
		% io:format("~p ~p ~p ~n", [Acc div 8, Acc rem 8, A]),
		set_pixel(7 - (XY div 8), 7 - XY rem 8, RGB),
		XY + 1
	end,
	lists:foldl(P, 0, Arr).

%%
%%	internal communication with the port
%%

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
			% io:format("send to port ~p ~n", [Msg]),
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

encode({set_pixel, X, Y, R, G, B}) -> [1, X, Y, R, G, B];
encode({fill, R, G, B}) -> [2, R, G, B].

decode([Int]) -> Int.