% Author: Eduardo Santana (efzambom@ime.usp.br)

-module(smart_city_test).

% For all facilities common to all tests:
-include("test_constructs.hrl").

% for each vertex is necessary to save its out links
create_map_list([] , _Graph , List) ->
	List;

create_map_list([Element | MoreElements] , Graph , List) ->
	
	{_E, _V1, _V2, _Label} = digraph:edge( Graph , Element ),

	Id = element( 1 , _Label),
	Length = element( 1 , string:to_float(element( 2 , _Label))), % Link Length	
	Capacity = element( 1 , string:to_float(element( 3 , _Label))),
	Freespeed = element( 1 , string:to_float(element( 4 , _Label))), 		
	
	Vertices = list_to_atom(lists:concat( [ _V1 , _V2 ] )),

	NewElement = [{ Vertices , { list_to_atom( Id ) , Length , Capacity , Freespeed , 0 } }],  % The last 0 is the number of cars in the link

	create_map_list( MoreElements , Graph , List ++ NewElement ).


% Create the actors that represent the city vertex

create_street_list( Graph ) ->
	
	Vertices = digraph:vertices( Graph ),

	create_street_list( Vertices , [] , Graph ).

create_street_list([] , List , _Graph ) -> List;
create_street_list([Element | MoreElements] , List , Graph) ->

	Edges = digraph:out_edges( Graph , Element ),

	ListEdges = create_map_list( Edges , Graph , [] ),

	StreetPID = class_Actor:create_initial_actor( class_Street,
		  [ atom_to_list(Element) , ListEdges ] ),

	NewElement = [{ Element , StreetPID }], 

	create_street_list( MoreElements , List ++ NewElement , Graph ).


create_buses( [] , _ListVertex , _CityGraph , _LogPID  ) -> ok;
create_buses( [ Bus | Buses ] , ListVertex , CityGraph , LogPID  ) -> 

	Id = element( 1 , Bus ),
	Interval = element( 2 , Bus ),
	Stops = element( 3 , Bus ),
	StartTime = element( 4 , Bus ),

	Path = calculate_bus_path( Stops , CityGraph , [] ),

	ListVertexPath = create_agents:get_path_nodes( Path , ListVertex , [] ),

	class_Actor:create_initial_actor( class_Bus,
		[ Id , ListVertexPath , Path , element( 1 , string:to_integer( StartTime )) , Interval , LogPID , Stops ] ),

	create_buses( Buses , ListVertex , CityGraph , LogPID  ).

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
	


spaw_proccess( [] , _ListVertex , _CityGraph , _MetroActor  ) -> 
	ok;

spaw_proccess( [ List | MoreLists ] , ListVertex , CityGraph , CityActors ) ->

	Name = element( 1 , List ),
	ListTrips = element( 2 , List ),

	spawn(create_agents, iterate_list , [ 1 , dict:from_list( ListVertex ) , ListTrips , CityGraph , Name , CityActors , self() ]),
	spaw_proccess( MoreLists  , ListVertex , CityGraph , CityActors ).



split_list( [] , _NumberLists , _ListSplit , ListReturn ) ->
	ListReturn;

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

% Runs the test.
%
-spec run() -> no_return().
run() ->	


	?test_start,

	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{

							simulation_name = "Sim-Diasca Smart City Integration Test",

							tick_duration = 1

							% We leave it to the default specification (all_outputs):
							% result_specification =
							%  [ { targeted_patterns, [ {".*",[data_and_plot]} ] },
							%    { blacklisted_patterns, ["^Second" ] } ]

							%result_specification = [ { targeted_patterns, [ {".*",data_only} ] } ]

						   },


	DeploymentSettings = #deployment_settings{

							computing_hosts = { use_host_file_otherwise_local,
												"sim-diasca-host-candidates.txt" },

							additional_elements_to_deploy = [ { ".", code } ],

							enable_performance_tracker = false

						   },




	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings, DeploymentSettings, LoadBalancingSettings ),

	ConfigPath = readConfigPath(),

	Config = config_parser:show( ConfigPath ),

	ListCars = trip_parser:show( element( 4 , Config ) ), % Read the cars from the trips.xml file

	CityGraph = map_parser:show( element( 3 , Config ) , false ), % Read the map from the map.xml file

	MetroFile = element( 5 , Config ), % Read the metro graph from the city. TODO: verify if this configurition does not exist.

	ListBuses = bus_parser:show( element( 6 , Config ) ), % Read the list of buses. TODO: verify if this configurition does not exist.

	ParkSpots = park_parser:show( element( 7 , Config ) ), 

	% create the vertices actors
	ListVertex  = create_street_list( CityGraph ),

	{ _ , Pwd } = file:get_cwd(),
	OutputPath = string:concat( Pwd, "/" ),
	AmqpClientPath = string:concat( Pwd, "/../deps/amqp_client"),

	LogPID = class_Actor:create_initial_actor( class_Log,
			[ string:concat( OutputPath, element( 1 , Config ) ),
			  [ AmqpClientPath,
				string:concat( AmqpClientPath, "/ebin" ),
				string:concat( AmqpClientPath, "/include/rabbit_common/ebin" )
			  ]
			] ),

	MetroActor = class_Actor:create_initial_actor( class_Metro, [ "MetroCity" , string:concat( OutputPath, MetroFile ) ] ), 

	CityActor = case element( 8 , Config ) of % verify if it is necessary to generate the city graph actor 
		"true" ->
			 class_Actor:create_initial_actor( class_City, [ "City" , { string:concat( OutputPath, element( 3 , Config ) ), ListVertex } ] );
		_ ->
			ok
	end,

	ParkActor = case ParkSpots of
	    ok ->
		ok;
	    _ ->		
		class_Actor:create_initial_actor( class_Parking , [ "Parking" , ParkSpots , LogPID ] )
	end,

	Names = [ "car1" , "car2" , "car3" , "car4" , "car5" , "car6" ],

	List = split_list( Names , length ( Names ) , ListCars , []  ),   

	spaw_proccess( List , ListVertex , CityGraph , { LogPID , MetroActor , ParkActor , CityActor } ),

	ok = collectResults(Names),

	create_buses( ListBuses , dict:from_list( ListVertex ) , CityGraph , LogPID  ),

	% We want this test to end once a specified virtual duration elapsed, in
	% seconds:
	SimulationDuration = element( 1 , string:to_integer(element( 2 , Config ) ) ),

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	?test_info_fmt( "Starting simulation, for a stop after a duration "
					"in virtual time of ~Bms.", [ SimulationDuration ] ),

	RootTimeManagerPid ! { startFor, [ SimulationDuration, self() ] },

	?test_info( "Waiting for the simulation to end, "
				"since having been declared as a simulation listener." ),

	receive

		simulation_stopped ->

			?test_info( "Simulation stopped spontaneously, "
						"specified stop tick must have been reached." )

	end,

	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	sim_diasca:shutdown(),

	?test_stop.


