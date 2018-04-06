-module(events_parser).

-export([read_csv/1]).

read_csv( FileName ) ->
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
        Type = lists:nth( 1 , TextSplit ),
        V1 = list_to_atom( lists:nth( 2 , TextSplit ) ),
        V2 = list_to_atom( lists:nth( 3 , TextSplit ) ),
        { Time, _ } = string:to_integer( lists:nth( 4 , TextSplit ) ),
        { Duration, _ } = string:to_integer( lists:nth( 5 , TextSplit ) ),
        { Capacity, _ } = string:to_integer( lists:nth( 6 , TextSplit ) ),
        Element = { Time, [ { Type, V1, V2, Duration, Capacity } ] },
        [ Element | read_line( Count +1 , ListRest ) ]
    end.
