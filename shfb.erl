%% framebuffer module for Raspberry Pi Sense Hat
%% morten.teinum@gmail.com

-module(shfb).
-export([create/1, set_pixel/4, to_binary/1, set_data/2]).

%% setnth from http://stackoverflow.com/a/4781219/1167976
setnth(1, [_|Rest], New) -> [New|Rest];
setnth(I, [E|Rest], New) -> [E|setnth(I-1, Rest, New)].

create_row(Color) ->
	[Color || _ <- lists:seq(1, 8)].

create(Color) ->
	[create_row(Color) || _ <- lists:seq(1, 8)].

set_pixel(X, Y, Color, FB) ->
	%% extract the row
	RowToChange = lists:nth(X, FB),

	%% change the column in that row
	ChangedRow = setnth(Y, RowToChange, Color),

	%% create a new matrix with the changed row
	setnth(X, FB, ChangedRow).

to_binary(FB) ->
	list_to_binary([<<RGB:24>> || RGB <- lists:flatten(FB)]).

set_data(List, _) ->
	List.