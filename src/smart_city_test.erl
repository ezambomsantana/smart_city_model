% Author: Eduardo Santana (efzambom@ime.usp.br)

-module(smart_city_test).

% For all facilities common to all tests:
-include("test_constructs.hrl").	

-spec run() -> no_return().
run() ->	

	?test_start,
	
	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{
		simulation_name = "Sim-Diasca Smart City Integration Test",
		tick_duration = 1,
		result_specification = no_output
	},

	DeploymentSettings = #deployment_settings{
		computing_hosts = localhost_only,
		additional_elements_to_deploy = [ { ".", code } ],
		enable_performance_tracker = false,
		enable_data_logger = false
	},

	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
	DeploymentManagerPid = sim_diasca:init( SimulationSettings, DeploymentSettings, LoadBalancingSettings ),

	ConfigPath = os:getenv( "CONFIG_PATH" ),
	
	io:format("Path: ~s", [ ConfigPath ] ),

	Config = config_parser:show( ConfigPath ),

	io:format("Trips: ~s", [ element( 4 , Config ) ] ),

	ListCars = trip_parser:show( element( 4 , Config ) ), % Read the cars from the trips.xml file

	CityGraph = map_parser:show( element( 3 , Config ) , false ), % Read the map from the map.xml file

	MetroFile = element( 5 , Config ), % Read the metro graph from the city. TODO: verify if this configurition does not exist.

	ListBuses = bus_parser:show( element( 6 , Config ) ), % Read the list of buses. TODO: verify if this configurition does not exist.

	ParkSpots = park_parser:read_csv( element( 7 , Config ) ), 

	TrafficSignals = traffic_signal_parser:show( element(8, Config ) ),

	DigitalRails = digital_rails_parser:show(element(9, Config )),

	ListEdges = create_scenario:create_street_list( CityGraph ),

	{ _ , Pwd } = file:get_cwd(),
	OutputPath = string:concat( Pwd, "/" ),
	AmqpClientPath = string:concat( Pwd, "/../deps/amqp_client"),

	LogName = string:concat( OutputPath, element( 1 , Config ) ),
	Paths = [ AmqpClientPath,
			string:concat( AmqpClientPath, "/ebin" ),
			string:concat( AmqpClientPath, "/include/rabbit_common/ebin" )
		],
	class_Actor:create_initial_actor( class_Street,  [ "Street" , ListEdges , LogName , Paths ] ),

	class_Actor:create_initial_actor( class_Metro, [ "MetroCity" , string:concat( OutputPath , MetroFile ) ] ), 

	class_Actor:create_initial_actor( class_DigitalRails,  [ DigitalRails ] ),

	case element( 8 , Config ) of % verify if it is necessary to generate the city graph actor 
		"true" -> class_Actor:create_initial_actor( class_City, [ "City" , { string:concat( OutputPath, element( 3 , Config ) ) } ] );
		_ -> ok
	end,

	case ParkSpots of
	    ok -> ok;
	    _ -> class_Actor:create_initial_actor( class_Parking , [ "Parking" , ParkSpots ] )
	end,

	Names = [ "car1" , "car2" , "car3" , "car4" , "car5" , "car6" , "car7" , "car8" ],

	List = create_scenario:split_list( Names , length ( Names ) , ListCars , []  ),   

	create_scenario:spaw_proccess( List , CityGraph , DigitalRails ), 
	create_scenario:collectResults( Names ),
	create_scenario:create_buses( ListBuses , CityGraph ),
	create_scenario:create_traffic_signals( TrafficSignals ),

	SimulationDuration = element( 1 , string:to_integer(element( 2 , Config ) ) ),

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	?test_info_fmt( "Starting simulation, for a stop after a duration "
					"in virtual time of ~Bms.", [ SimulationDuration ] ),

	RootTimeManagerPid ! { startFor, [ SimulationDuration, self() ] },

	receive
		simulation_stopped ->
			?test_info( "Simulation stopped spontaneously, "
						"specified stop tick must have been reached." )
	end,

	?test_stop.