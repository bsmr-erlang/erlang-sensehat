%%% Raspberry PI Sense Hat Port integration

-module(sensehat).
-author("Morten Teinum <morten.teinum@gmail.com>").

-export([start/0, stop/0, init/0]).
-export([set_pixel/3,
	     clear/0,
	     fill/1,
	     logo/0, 
	     fill_fb/1,
	     get_gamma/0,
	     set_gamma/1,
	     set_gamma_low_light/0,
	     reset_gamma/1]).

start() ->
	case erl_ddll:load_driver(".", sensehat_drv) of
		ok -> ok;
		{error, already_loaded} -> ok;
		{error, Message} -> exit(erl_ddll:format_error(Message))
	end,
	% Returns the process identifier of a new process started.
	spawn(?MODULE, init, []).

create_framebuffer(Default) ->
	array:new([{size,64}, {default,Default}, {fixed,true}]).

init() ->
	% register this process with the atom sensehat
	% self() will be the port owner
	% http://erlang.org/doc/reference_manual/ports.html#id88842
	register(sensehat, self()),

	% http://erlang.org/doc/man/erlang.html#open_port-2
	% open and start the driver (ErlDrvEntry.start)
	Port = open_port({spawn_driver, sensehat_drv}, [binary, use_stdio]),

	% start waiting for messages from the port
	loop(Port, create_framebuffer(0)).

call_port(Msg) ->
	sensehat ! {call, Msg},
	ok.

get_port(Msg) ->
  	sensehat ! {get, self(), Msg},
 	receive
 		{sensehat, Result} ->
 			Result
 	end.	

stop() ->
	sensehat ! stop.

coordinate_to_index(X,  Y) -> X * 8 + Y.

loop(Port, Framebuffer) ->
	% create a binary representation of the framebuffer that we will send to the port
	BinaryFramebuffer = list_to_binary([<<X:24>> || X <- array:to_list(Framebuffer)]),

	Port ! { self(), {command, [1, BinaryFramebuffer]}},

	% wait for work to do
	receive
		{set_pixel, X, Y, RGB} ->
			loop(Port, array:set(coordinate_to_index(X, Y), RGB, Framebuffer));

		{fill, RGB} ->
			loop(Port, create_framebuffer(RGB));

		{fill_fb, Data} ->
			loop(Port, array:fix(array:from_list(Data)));

		{call, Msg} ->
			% Sends Data to the port.
			% alternative: port_command(Port, Msg)
			Port ! {self(), {command, encode(Msg)}},
			% our call is fire and forget, wait for next message
			loop(Port, Framebuffer);

		{get, Caller, Msg} ->
			Port ! {self(), {command, encode(Msg)}},
			receive
  				{Port, {data, Data}} ->
 					Caller ! {sensehat, Data}
  			end,
  			loop(Port, Framebuffer);

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

%%%
%%% API
%%%

reset_gamma(Type) ->
	call_port({reset_gamma, Type}).

get_gamma() ->
	get_port({get_gamma}).

set_gamma(Value) ->
	call_port({set_gamma, Value}).

set_gamma_low_light() ->
	set_gamma(<<0,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,3,3,3,4,4,5,5,6,6,7,7,8,8,9,10,10>>).

set_pixel(X, Y, RGB) when X >= 0, X =< 7, Y >= 0, Y =< 7 ->
	sensehat ! {set_pixel, X, Y, RGB},
	ok.

fill(RGB) ->
	sensehat ! {fill, RGB},
	ok.

fill_fb(Data) when length(Data) =:= 64 ->
	sensehat ! {fill_fb, Data},
	ok.

clear() ->
	fill(16#000000).

logo() -> fill_fb(
	[16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff,
	 16#fcf2f5, 16#ffffff, 16#fdf9fa, 16#eabec4, 16#f9e9ee, 16#ffffff, 16#fbeff2, 16#e9b1c1,
	 16#fdf7f9, 16#ffffff, 16#e192a7, 16#a70712, 16#da7a94, 16#ffffff, 16#fdf7f8, 16#cc4b6e,
	 16#ffffff, 16#ffffff, 16#fef9fa, 16#ffffff, 16#ffffff, 16#ffffff, 16#ffffff, 16#e39eb1,
	 16#ffffff, 16#ffffff, 16#da7d97, 16#ca5b69, 16#d1577d, 16#c94268, 16#bc2342, 16#c93f65,
	 16#ffffff, 16#ffffff, 16#cc4a6e, 16#930000, 16#9e0000, 16#a60005, 16#bc2444, 16#c73b61,
	 16#f8e6eb, 16#ffffff, 16#fbeff2, 16#c43658, 16#b31d2a, 16#e6a6b8, 16#ffffff, 16#d56886,
     16#f8e6ea, 16#ffffff, 16#ffffff, 16#ffffff, 16#fffdff, 16#ffffff, 16#fefbfc, 16#f0c9d4]).

