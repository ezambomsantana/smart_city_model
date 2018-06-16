-module(trip_parser).

% usage:
%
% l(osm_parser).
% osm_parser:show("map.osm").

-export([
         show/1
        ]).

show( FileName ) ->
	case file:read_file(FileName) of
		{ok, Data} ->
			List = binary:split(Data, [<<"\n">>], [global]),
			read_line(1 , List);
		_ ->
			ok
	end.

read_line( _Count, [] ) -> [];
read_line( Count , [ Data | ListRest ] ) ->
	String = binary_to_list(Data),

	case String of
		[] -> [];
		_ ->
			Text = string:chomp(String),
			TextSplit = string:split( Text ,  ";" , all ),
			Name = lists:nth( 1 , TextSplit ),
			Origin = lists:nth( 2 , TextSplit ),
			Destination = lists:nth( 3 , TextSplit ),
			LinkOrigin = lists:nth( 4 , TextSplit ),
			CarCount = lists:nth( 5 , TextSplit ),
			Start = lists:nth( 6 , TextSplit ),
			Type = ok,
			Mode = lists:nth( 7 , TextSplit ),
			Park = ok,
			Uuid = lists:nth( 8 , TextSplit ),
			Element = { Origin, Destination, CarCount, Start, LinkOrigin, Type, Mode, Name, Park, Uuid },
			%Element = { Origin, Destination, CarCount, Start, LinkOrigin, Type, Mode, Name, Park },
			[ Element | read_line( Count +1 , ListRest ) ]
	end.
