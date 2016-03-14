-module(sensestick).
-author("Morten Teinum <morten.teinum@gmail.com>").
-export([start/0, stop/0, init/0]).
-export([subscribe/0]).

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

	Port = open_port({spawn_driver, sensestick_drv}, [use_stdio]),

	% send control message to open it
	port_control(Port, 1, []),

	loop(Port, pubsub:init())
.

loop(Port, PS) ->
	% io:format("loop: ~p ~p ~n", [Port, PS]),
	receive
		{Port, {data, Data}} ->
			pubsub:publish({sensestick, decode(Data)}, PS),
			loop(Port, PS);

		{subscribe, Pid} ->
			loop(Port, pubsub:subscribe(Pid, PS));

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

decode([1]) -> up;
decode([2]) -> down;
decode([3]) -> left;
decode([4]) -> right;
decode([5]) -> enter.

% client api
subscribe() ->
	sensestick ! {subscribe, self()}.



