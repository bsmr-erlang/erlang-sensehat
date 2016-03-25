-module(st_port).
-author("Morten Teinum <morten.teinum@gmail.com>").
-behaviour(gen_server).

% interface api
-export([start_link/0]).

% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

start_link() ->
	case erl_ddll:load_driver(".", sensestick_drv) of
		ok -> ok;
		{error, already_loaded} -> ok;
		{error, Message} -> exit(erl_ddll:format_error(Message))
	end,
	gen_server:start_link(?MODULE, [], []).

init(_Args) ->
	% connect to the port driver
	Port = open_port({spawn_driver, sensestick_drv}, [use_stdio]),
	% send control message to connect to the joystick
	port_control(Port, 1, []),
	% initial state for this server
	{ok, Port}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(shutdown, State) ->
    {stop, normal, State}.

handle_info({_Port, {data, [Code]}}, State) ->
	gen_event:notify(st_event_manager, code_to_event(Code)),
	{noreply, State}.

terminate(Reason, State) ->
	io:format("terminate ~p ~p ~n", [Reason, State]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

code_to_event(Code) ->
  	lists:nth(Code, [up,down,left,right,enter]).
  	