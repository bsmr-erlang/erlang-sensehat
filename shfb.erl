%% framebuffer module for Raspberry Pi Sense Hat
%% morten.teinum@gmail.com

-module(shfb).
-export([create/1, set_pixel/4, to_binary/1, set_data/2, set_rotation/2]).

-record(framebuffer, {rotation, data}).

%% setnth from http://stackoverflow.com/a/4781219/1167976
setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

create_row(Color) ->
	[Color || _ <- lists:seq(1, 8)].

create(Color) ->
	#framebuffer {
		rotation=0,
		data=[create_row(Color) || _ <- lists:seq(1, 8)]
	}.

set_rotation(N, FB) ->
	FB#framebuffer{rotation=N}.

set_pixel(X, Y, Color, FB) ->
	%% extract the row
	RowToChange = lists:nth(X, FB#framebuffer.data),

	%% change the column in that row
	ChangedRow = setnth(Y, RowToChange, Color),

	%% create a new matrix with the changed row
	FB#framebuffer { data=setnth(X, FB#framebuffer.data, ChangedRow) }.

get_render_buffer(#framebuffer{data=Data, rotation=0}) ->
	Data;

get_render_buffer(#framebuffer{data=Data, rotation=90}) ->
	rotate90(Data);

get_render_buffer(#framebuffer{data=Data, rotation=180}) ->
	rotate90(rotate90(Data)).

to_binary(FB) ->
	list_to_binary([<<RGB:24>> || RGB <- lists:flatten(get_render_buffer(FB))]).

set_data(List, FB) ->
	FB#framebuffer { data = List }.

rotate90(Data) ->
	reverse_rows(transpose(Data)).

reverse_rows(Data) ->
	lists:map(fun(E) -> lists:reverse(E) end, Data).

% from haskell http://stackoverflow.com/a/5412598/1167976
transpose([[X | Xs] | Xss]) ->
    [[X | [H || [H | _] <- Xss]]
     | transpose([Xs | [T || [_ | T] <- Xss]])];
transpose([[] | Xss]) -> transpose(Xss);
transpose([]) -> [].