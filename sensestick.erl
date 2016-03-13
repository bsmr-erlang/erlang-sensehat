-module(sensestick).
-export([start/0, stop/0, init/0]).


start() ->
	io:format("loading driver ~n", []),

	case erl_ddll:load_driver(".", sensestick_drv) of
		ok -> ok;
		{error, already_loaded} -> ok;
		{error, Message} -> exit(erl_ddll:format_error(Message))
	end,
	% Returns the process identifier of a new process started.
	spawn(?MODULE, init, []).

init() ->
	register(sensestick, self()),

	io:format("open port ~n", []),

	Port = open_port({spawn_driver, sensestick_drv}, [use_stdio]),

	io:format("port opened ~p ~n", [Port]),

	% send control message to open it
	port_control(Port, 1, []),

	loop(Port)
.

loop(Port) ->
	receive
		{Port, {data, Data}} ->
			io:format("event! ~p ~n", [Data]),
			loop(Port);

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

stop() ->
	sensestick ! stop.
