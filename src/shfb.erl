%% framebuffer module for Raspberry Pi Sense Hat

-module(shfb).
-author("Morten Teinum <morten.teinum@gmail.com>").
-export([create/1, set_pixel/4, set_pixels/2, to_binary/1, set_rotation/2]).

-record(framebuffer, {rotation, pixels}).

%% setnth from http://stackoverflow.com/a/4781219/1167976
setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

create_row(Color) ->
	[Color || _ <- lists:seq(1, 8)].

create(Color) ->
	#framebuffer {
		rotation=0,
		pixels=[create_row(Color) || _ <- lists:seq(1, 8)]
	}.

set_rotation(N, FB) ->
	FB#framebuffer{rotation=N}.

set_pixel(X, Y, Color, FB) ->
	%% extract the row
	RowToChange = lists:nth(X, FB#framebuffer.pixels),

	%% change the column in that row
	ChangedRow = setnth(Y, RowToChange, Color),

	%% create a new matrix with the changed row
	FB#framebuffer { pixels=setnth(X, FB#framebuffer.pixels, ChangedRow) }.

set_pixels(Pixels, FB) ->
	FB#framebuffer { pixels = Pixels }.

get_render_buffer(#framebuffer{pixels=Pixels, rotation=0}) ->
	Pixels;

get_render_buffer(#framebuffer{pixels=Pixels, rotation=90}) ->
	rotate90(Pixels);

get_render_buffer(#framebuffer{pixels=Pixels, rotation=180}) ->
	rotate90(rotate90(Pixels));

get_render_buffer(#framebuffer{pixels=Pixels, rotation=270}) ->
	transpose(Pixels).

rotate90(Pixels) ->
	reverse_rows(transpose(Pixels)).

reverse_rows(Pixels) ->
	lists:map(fun(E) -> lists:reverse(E) end, Pixels).

to_binary(FB) ->
	list_to_binary([<<RGB:24>> || RGB <- lists:flatten(get_render_buffer(FB))]).

% from haskell http://stackoverflow.com/a/5412598/1167976
transpose([[X | Xs] | Xss]) ->
    [[X | [H || [H | _] <- Xss]]
     | transpose([Xs | [T || [_ | T] <- Xss]])];
transpose([[] | Xss]) -> transpose(Xss);
transpose([]) -> [].