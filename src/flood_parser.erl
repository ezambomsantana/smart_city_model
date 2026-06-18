-module(flood_parser).

-export([read_csv/1, get_flood_events/2]).

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

        RoadName = list_to_atom( lists:nth( 1 , TextSplit ) ),
        { LinkId, _ } = string:to_integer( lists:nth( 2 , TextSplit ) ),
        FromNode = list_to_atom( lists:nth( 3 , TextSplit ) ),
        ToNode = list_to_atom( lists:nth( 4 , TextSplit ) ),
        { Rainfall, _ } = string:to_float( lists:nth( 5 , TextSplit ) ),
        Element = { Rainfall, { LinkId, FromNode, ToNode, RoadName } },
        [ Element | read_line( Count +1 , ListRest ) ]
    end.

get_flood_events(ListRainfall, ListFlood) ->
    Rainfall = dict:from_list( ListRainfall ),

    % Creates a dictionary where:
    % Key: Time of flood, Value: List of streets
    Flood = lists:foldl(
        fun({Time, Street}, AccDict) ->
            dict:append(Time, Street, AccDict)
        end,
        dict:new(),
        ListFlood
    ),

	close_street_events( Flood, Rainfall ).

-spec close_street_events( dict:dict(), dict:dict() ) -> list().
close_street_events( Flood, Rainfall ) ->
    EventsList = lists:flatten(
        lists:map(
            fun(Time) ->
                AccumulatedRainfall = calculate_accumulated_rainfall( Rainfall, Time ),

                ReduceTrafficEvents = generate_reduce_traffic_events( AccumulatedRainfall ),
            
                FloodStreets = get_flood_streets( Flood, AccumulatedRainfall ),
            
                CloseStreetEvents = generate_close_street_events( FloodStreets ),

                EventsList = lists:flatten([ReduceTrafficEvents, CloseStreetEvents]),
                
                % Creates a list of tuples {Time, Event}
                lists:map(
                    fun(Event) -> {Time, Event} end,
                    EventsList
                )
            end,
            dict:fetch_keys( Rainfall )
        )
    ),
    EventsList.

-spec calculate_accumulated_rainfall( dict:dict(), integer() ) -> number().
calculate_accumulated_rainfall( Rainfall, CurrentTime ) ->
	Period = 60 * 60 * 3, % 3 hours
	
	StartTime = CurrentTime - Period,
	
	AllKeys = dict:fetch_keys( Rainfall ),
	
	PeriodKeys = lists:filter( 
		fun( Time ) -> Time > StartTime andalso Time =< CurrentTime end, 
		AllKeys 
	),
	
	% Return the sum of the rainfall values in the last period
	lists:sum( 
		lists:map( 
			fun( Time ) -> 
				case dict:find( Time, Rainfall ) of
					{ok, Value} -> Value;
					error -> 0
				end
			end, 
			PeriodKeys 
		) 
	).

-spec get_flood_streets( dict:dict(), number() ) -> list().
get_flood_streets( Flood, AccumulatedRainfall ) ->
    AllKeys = dict:fetch_keys( Flood ),

    RainfallKeys = lists:filter( 
        fun( RainfallLimit ) -> RainfallLimit =< AccumulatedRainfall end,
        AllKeys 
    ),

    
	FloodStreets = lists:foldl(
		fun( Rainfall, Acc ) ->
			case dict:find( Rainfall, Flood ) of
				{ok, StreetList} -> 
					lists:foldl(
						fun( Street, StreetAcc ) ->
							V1 = element( 2, Street ),
							V2 = element( 3, Street ),
							[ { V1, V2 } | StreetAcc ]
						end,
						Acc,
						StreetList
					);
				error -> Acc
			end
		end, [], RainfallKeys ),

	FloodStreets.

-spec generate_close_street_events( list() ) -> list().
generate_close_street_events( FloodStreets ) ->

    EventsList = lists:foldl(
        fun ( Street, EventsList ) ->
            V1 = element( 1, Street ),
            V2 = element( 2, Street ),

			Duration = 60 * 60 * 2, % 2 hours
			CloseStreetEvent = { "close_street", V1, V2, Duration },

			[ CloseStreetEvent | EventsList ]

        end,
        [], FloodStreets
    ),

    EventsList.

generate_reduce_traffic_events( AccumulatedRainfall ) ->
    MinRainfall = 5.0,
    MaxRainfall = 30.0,
    DeltaRainfall = MaxRainfall - MinRainfall,

    MinSpeedDecrease = 2,
    MaxSpeedDecrease = 17,
    DeltaSpeedDecrease = MaxSpeedDecrease - MinSpeedDecrease,

    MinCapacityDecrease = 4,
    MaxCapacityDecrease = 30,
    DeltaCapacityDecrease = MaxCapacityDecrease - MinCapacityDecrease,

    case AccumulatedRainfall of
        X when X < MinRainfall ->
            ReduceTrafficEvent = { "reduce_traffic", 100, 100 },
            [ ReduceTrafficEvent ];
        X when X >= MaxRainfall ->
            ReduceTrafficEvent = { "reduce_traffic", 100 - MaxCapacityDecrease, 100 - MaxSpeedDecrease },
            [ ReduceTrafficEvent ];
        _ ->
            SpeedDecrease = (DeltaSpeedDecrease / DeltaRainfall) * (AccumulatedRainfall - MinRainfall) + MinSpeedDecrease,
            CapacityDecrease = (DeltaCapacityDecrease / DeltaRainfall) * (AccumulatedRainfall - MinRainfall) + MinCapacityDecrease,
            ReduceTrafficEvent = { "reduce_traffic", 100 - CapacityDecrease, 100 - SpeedDecrease },
            [ ReduceTrafficEvent ]
    end.
