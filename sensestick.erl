-module(sensestick).
-author("Morten Teinum <morten.teinum@gmail.com>").
-behaviour(gen_server).

% interface api
-export([start_link/0, stop/0, subscribe/0]).

% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-record(state, {port, pid}).

start_link() ->
	case erl_ddll:load_driver(".", sensestick_drv) of
		ok -> ok;
		{error, already_loaded} -> ok;
		{error, Message} -> exit(erl_ddll:format_error(Message))
	end,
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, shutdown).

init(_Args) ->
	% event manager that we notify changes from the port
	{ok, Pid} = gen_event:start_link(),
	% connect to the port driver
	Port = open_port({spawn_driver, sensestick_drv}, [use_stdio]),
	% send control message to connect to the joystick
	port_control(Port, 1, []),
	% initial state for this server
	{ok, #state{port=Port, pid=Pid}}.

handle_call({subscribe, From}, _From, State) ->
	HandlerId = {st_event, make_ref()},
	gen_event:add_handler(State#state.pid, HandlerId, [From]),
    {reply, HandlerId, State}.

handle_cast(shutdown, State) ->
    {stop, normal, State}.

handle_info({_Port, {data, [Code]}}, State) ->
	gen_event:notify(State#state.pid, code_to_event(Code)),
	{noreply, State}.

terminate(Reason, State) ->
	io:format("terminate ~p ~p ~n", [Reason, State]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

code_to_event(Code) ->
  	lists:nth(Code, [up,down,left,right,enter]).

% client api
subscribe() ->
	gen_server:call(?MODULE, {subscribe, self()}).
