-module(create_scenario).

-export([
            create_map_list/2,
            create_street_list/1,
            create_buses/2,
            calculate_bus_path/3,
            spaw_proccess/3,
            split_list/4,
            collectResults/1,
						readConfigPath/0,
						create_traffic_signals/1
        ]).

% for each vertex is necessary to save its out links
create_map_list([] , _Graph ) -> [];
create_map_list([Element | MoreElements] , Graph ) ->
	
	{_, V1, V2, Label} = digraph:edge( Graph , Element ),

	Id = element( 1 , Label),
	Length = element( 1 , string:to_float(element( 2 , Label))), % Link Length	
	CapacityXml = element( 1 , string:to_float(element( 3 , Label))),
	Capacity = CapacityXml*5, % xml unit is in "cars", and the internal unit is "bikes"
	Freespeed = element( 1 , string:to_float(element( 4 , Label))), 
	Lanes = element(1, string:to_float(element(5, Label))),		
	IsCycleway = element(7, Label),
  IsCyclelane = element(8, Label),
  Inclination = element(9, Label),
	
	Vertices = list_to_atom( lists:concat( [ V1 , V2 ] )),

	NewElement = { Vertices , { list_to_atom( Id ) , Length , Capacity , Freespeed , 0, Lanes, {}, IsCycleway, IsCyclelane, Inclination} },  % 0 is the number of cars in the link

	[ NewElement | create_map_list( MoreElements , Graph ) ].

create_street_list( Graph ) ->	
	Vertices = digraph:vertices( Graph ),
	create_street_list( Vertices , [] , Graph ).

create_street_list([] , List , _Graph ) -> List;
create_street_list([Element | MoreElements] , List , Graph) ->
	Edges = digraph:out_edges( Graph , Element ),
	ListEdges = create_map_list( Edges , Graph ),
	create_street_list( MoreElements , List ++ ListEdges , Graph ).

create_buses( [] , _CityGraph  ) -> ok;
create_buses( [ Bus | Buses ] , CityGraph  ) -> 

	Id = element( 1 , Bus ),
	Interval = element( 2 , Bus ),
	Stops = element( 3 , Bus ),
	StartTime = element( 4 , Bus ),

	Path = calculate_bus_path( Stops , CityGraph , [] ),

	FinalStartTime = element( 1 , string:to_integer( StartTime ) ) - 600 + class_RandomManager:get_uniform_value( 1200 ),

	class_Actor:create_initial_actor( class_Bus,
		[ Id , Path , FinalStartTime , Interval , Stops ] ),

	create_buses( Buses , CityGraph ).

calculate_bus_path( [ Stop | List ] , CityGraph  , Path ) ->
	case length( List ) >= 1 of 
		true ->
			NextStop = lists:nth( 1 , List ),
			ParcialPath = case length( List ) == 1 of 
			   true -> 
				digraph:get_short_path( CityGraph , list_to_atom( Stop ) , list_to_atom( NextStop ) );	
			   false ->					
				lists:droplast( digraph:get_short_path( CityGraph , list_to_atom( Stop ) , list_to_atom( NextStop ) ) )		
			end,
			calculate_bus_path( List , CityGraph , Path ++ ParcialPath);
		false ->
			Path
	end.	

create_traffic_signals([]) -> ok;
create_traffic_signals([{signal, SignalAttribs, SignalContent} | Signals]) ->
	[{nodes, [{node, [{id, NodeId}], _} | _]}, _] = SignalContent,
	class_Actor:create_initial_actor(class_TrafficSignals, 
		[string:concat("traffic-signals-at-node-", NodeId), {signal, SignalAttribs, SignalContent}]),

	create_traffic_signals(Signals);

create_traffic_signals([_ | Signals]) ->
	create_traffic_signals(Signals).

spaw_proccess( [] , _CityGraph, _DigitalRails ) -> ok;
spaw_proccess( [ List | MoreLists ] , CityGraph, DigitalRails ) ->
	{ Name , ListTrips } = List,

	spawn( create_agents, iterate_list , [ 1 , ListTrips , CityGraph , Name , self(), DigitalRails ]),
	spaw_proccess( MoreLists , CityGraph, DigitalRails ).

split_list( [] , _NumberLists , _ListSplit , ListReturn ) -> ListReturn;
split_list( [ Name | Names ] , NumberLists , ListSplit , ListReturn ) ->

	{List , ListCars } = lists:split(round (length (ListSplit) / NumberLists), ListSplit),

	Element = [ { Name , List } ],

	split_list( Names , length ( Names ) , ListCars , ListReturn ++ Element ).

collectResults( [] ) -> ok;
collectResults( ListNames ) ->
  receive
    { Name } ->
      collectResults( ListNames -- [Name] );
    _ ->
      collectResults( ListNames )
  end.

readConfigPath() ->
	{ok, Device} = file:open('../interscsimulator.conf', [read]),
	{ok, Data} = file:read_line(Device),
	string:chomp(Data).