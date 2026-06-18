-module(rainfall_parser).

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
        { Time, _ } = string:to_integer( lists:nth( 1 , TextSplit ) ),
        { Rainfall, _ } = string:to_float( lists:nth( 2 , TextSplit ) ),
        Element = { Time, Rainfall },
        [ Element | read_line( Count +1 , ListRest ) ]
    end.
