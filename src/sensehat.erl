%%% Raspberry PI Sense Hat Port integration

-module(sensehat).
-author("Morten Teinum <morten.teinum@gmail.com>").

-export([start/0, stop/0, init/0]).
-export([set_pixel/3,
	     set_pixels/1,
	     clear/0,
	     clear/1,
	     get_gamma/0,
	     set_gamma/1,
	     set_gamma_low_light/0,
	     reset_gamma/1,
	     set_rotation/1]).

start() ->
	case erl_ddll:load_driver(".", sensehat_drv) of
		ok -> ok;
		{error, already_loaded} -> ok;
		{error, Message} -> exit(erl_ddll:format_error(Message))
	end,
	% Returns the process identifier of a new process started.
	spawn(?MODULE, init, []).

stop() ->
	sensehat ! stop.

init() ->
	% register this process with the atom sensehat
	% self() will be the port owner
	% http://erlang.org/doc/reference_manual/ports.html#id88842
	register(sensehat, self()),

	% http://erlang.org/doc/man/erlang.html#open_port-2
	% open and start the driver (ErlDrvEntry.start)
	Port = open_port({spawn_driver, sensehat_drv}, [binary, use_stdio]),

	% start waiting for messages from the port
	loop(Port, shfb:create(0)).

loop(Port, FB) ->
	Port ! { self(), {command, [1, shfb:to_binary(FB)]}},

	% wait for work to do
	receive
		{set_pixel, X, Y, RGB} ->
			loop(Port, shfb:set_pixel(X, Y, RGB, FB));

		{set_pixels, Pixels} ->
			loop(Port, shfb:set_pixels(Pixels, FB));
		
		{clear, RGB} ->
			loop(Port, shfb:create(RGB));

		{set_rotation, N} ->
			loop(Port, shfb:set_rotation(N, FB));

		{cast, Msg} ->
			% Sends Data to the port.
			% alternative: port_command(Port, Msg)
			Port ! {self(), {command, encode(Msg)}},
			% our call is fire and forget, wait for next message
			loop(Port, FB);

		{call, Caller, Msg} ->
			Port ! {self(), {command, encode(Msg)}},
			receive
  				{Port, {data, Data}} ->
 					Caller ! {sensehat, Data}
  			end,
  			loop(Port, FB);

		% tell the port to close
		stop ->
			Port ! {self(), close},
			% wait for the port to be closed
			receive
				{Port, closed} ->
					exit(normal)
			end;
		% If the port has terminated for some reason.
		{'EXIT', Port, Reason} ->
			io:format("EXIT: ~p ~p ~n", [Port, Reason]),
			exit(port_terminated)
	end.

encode({get_gamma})                  -> [2];
encode({set_gamma, Value})           -> [3, Value];
encode({reset_gamma, gamma_default}) -> [4, 0];
encode({reset_gamma, gamma_low})     -> [4, 1].

cast(Msg) ->
	sensehat ! {cast, Msg},
	ok.

call(Msg) ->
  	sensehat ! {call, self(), Msg},
 	receive
 		{sensehat, Result} ->
 			Result
 	end.	

%%%
%%% API
%%%

reset_gamma(Type) ->
	cast({reset_gamma, Type}).

get_gamma() ->
	call({get_gamma}).

set_gamma(Value) ->
	cast({set_gamma, Value}).

set_gamma_low_light() ->
	set_gamma(<<0,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,3,3,3,4,4,5,5,6,6,7,7,8,8,9,10,10>>).

set_pixel(X, Y, RGB) when X >= 0, X =< 7, Y >= 0, Y =< 7 ->
	sensehat ! {set_pixel, X, Y, RGB},
	ok.

set_pixels(Pixels) when length(Pixels) =:= 8 ->
	sensehat ! {set_pixels, Pixels},
	ok.

set_rotation(N) when N =:= 0; N =:= 90; N =:= 180; N =:= 270 ->
	sensehat ! {set_rotation, N},
	ok.

clear(RGB) ->
	sensehat ! {clear, RGB},
	ok.

clear() ->
	clear(16#000000).
