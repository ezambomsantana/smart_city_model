%Class that represents a person that can moves around the city graph on foot or by car
-module(class_Car).

-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName , ListTripsFinal , StartTime , Type , Park , Mode, GraphManagerPid, Uuid, Channel ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/10, new_link/10,
		 synchronous_new/10, synchronous_new_link/10,
		 synchronous_timed_new/10, synchronous_timed_new_link/10,
		 remote_new/11, remote_new_link/11, remote_synchronous_new/11,
		 remote_synchronous_new_link/11, remote_synchronisable_new_link/11,
		 remote_synchronous_timed_new/11, remote_synchronous_timed_new_link/11,
		 construct/11, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, get_parking_spot/3 , set_new_path/3 ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter() , parameter(), parameter(), parameter(), parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, CarName ),

	InitialTrip = lists:nth( 1 , ListTripsFinal ),	
	Path = element( 2 , InitialTrip ),

	NewState = setAttributes( ActorState, [
		{ car_name, CarName },
		{ trips , ListTripsFinal },
		{ type, Type },
		{ distance , 0 },
		{ car_position, -1 },
		{ start_time , StartTime },
		{ path , Path },
		{ mode , Mode },
		{ last_vertex_pid , ok },
		{ coordFrom , ok },
		{ wait , false },
		{ graph_manager, GraphManagerPid },
		{ uuid, Uuid },
		{ channel, Channel }
						] ),

	case Park of
		ok ->
			setAttribute( NewState , park_status , not_parking );
		_ ->
			setAttributes( NewState , [ { park_status , find } , { park , Park } ] )
	end.

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->	
	Trips = getAttribute( State , trips ), 
	Wait = getAttribute( State , wait ), 
	Path = getAttribute( State , path ), 
	verify_next_action( State , Trips , Path , Wait ).

verify_next_action( State , Trips , Path , Wait ) when Wait == true ->
	CurrentTrip = lists:nth( 1 , Trips ),		
	?wooper_return_state_only( request_position( State , CurrentTrip , Path ) );

verify_next_action( State , _Trip , Path , _Wait ) when Path == false ->
	executeOneway( State , declareTermination );

verify_next_action( State , Trips , Path , _Wait ) when length( Trips ) == 0, Path == finish -> 
	executeOneway( State , declareTermination );

verify_next_action( State , Trips , Path , _Wait ) when length( Trips ) > 0 ->
	CurrentTrip = lists:nth( 1 , Trips ),		
	?wooper_return_state_only( request_position( State , CurrentTrip , Path ) );

verify_next_action( State , _Trips , _Path , _Wait  ) ->
	Type = getAttribute( State , type ),						
	TotalLength = getAttribute( State , distance ),
	StartTime = getAttribute( State , start_time ),
	CarId = getAttribute( State , car_name ),	
	LastPosition = getAttribute( State , car_position ),
	Mode = getAttribute( State , mode ), 

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	print:write_final_message( Type , TotalLength , StartTime , CarId , CurrentTickOffset , LastPosition , Mode , csv ),
	PathFinish = setAttribute( State , path , finish ),

	executeOneway( PathFinish , scheduleNextSpontaneousTick ).

request_position( State , _Trip , Path ) when Path == finish ->
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),
	Trips = getAttribute( State , trips ), 
	NewTrips = list_utils:remove_element_at( Trips , 1 ),
	
	NewState = case length( NewTrips ) > 0 of
		true -> 
			InitialTrip = lists:nth( 1 , NewTrips ),	
			NewPath = element( 2 , InitialTrip ),
			setAttributes( State , [ { trips , NewTrips } , { path, NewPath} ] );
		false -> 
			setAttributes( State , [ { trips , NewTrips } , { path, ok} ] )
	end,

	executeOneway( NewState , addSpontaneousTick , CurrentTickOffset + 1 );	


request_position( State , Trip , Path ) ->
	case length( Path ) > 1 of
		true ->	get_next_vertex( State , Path , element( 1 , Trip ) );
		false -> verify_park( State , element( 1 , Trip ) )
	end.

verify_park( State , Mode ) when Mode == walk ->
	FinalState = setAttribute( State, path , finish ),
	executeOneway( FinalState , scheduleNextSpontaneousTick );


verify_park( State , _Mode ) ->						
	DecrementVertex = getAttribute( State , last_vertex_pid ),	
	ets:update_counter( list_streets , DecrementVertex , { 6 , -1 }),
	ParkStatus = getAttribute( State , park_status ),

	case ParkStatus of

		not_parking ->
			FinalState = setAttribute( State , path , finish ),

			executeOneway( FinalState , scheduleNextSpontaneousTick );
		finish ->

			Park = getAttribute( State , park ),
					
			Parking = ets:lookup_element(options, parking_pid, 2 ),
			NewState = class_Actor:send_actor_message( Parking, { spot_in_use, { Park } } , State ),
									
			FinalState = setAttribute( NewState, path , finish ),

			executeOneway( FinalState , scheduleNextSpontaneousTick );
		find ->
			Park = getAttribute( State , park ),
			Parking = ets:lookup_element(options, parking_pid, 2 ),
			class_Actor:send_actor_message( Parking, { spot_available, { Park } } , State )
	end.


get_next_vertex( State , [ Current | Path ] , Mode ) when Mode == walk ->			
	Vertices = list_to_atom( lists:concat( [ Current , lists:nth( 1 , Path ) ] )),
	
	Data = lists:nth( 1, ets:lookup( list_streets , Vertices ) ),
	{ Id , Time , Distance } = traffic_models:get_speed_walk( Data ),
	
	TotalLength = getAttribute( State , distance ) + Distance,
	FinalState = setAttributes( State , [ { distance , TotalLength } , { car_position , Id } , { path , Path } ] ), 

%	print_movement( State ),

	executeOneway( FinalState , addSpontaneousTick , class_Actor:get_current_tick_offset( FinalState ) + Time );

get_next_vertex( State , Path , _Mode ) ->

	Origin = lists:nth( 1 , Path ),
	Vertices = list_to_atom( lists:concat( [ Origin  , lists:nth( 2 , Path ) ] )),

	CurrentTick = class_Actor:get_current_tick_offset( State ),

	PathChangedState = case ets:info(events) of
				   undefined -> ok;
				   _ ->
					   case ets:lookup( events, Vertices ) of
						   [{ _ , _ }] -> best_path( State, Path, Origin );
						   _ -> ok
					   end
			   end,

	IntermediateState = case PathChangedState of
				    ok -> case ets:info(traffic_events) of
						  undefined -> ok;
						  _ ->
							  case ets:lookup( traffic_events, Origin ) of
								  [{ _ , { FromNode, ToNode } }] ->
									  case edgeInPath( Path, FromNode, ToNode ) of
										  true -> best_path( State, Path, Origin );
										  false -> ok
									  end;
								  _ -> ok
							  end
					  end;
				    ChangedState -> ChangedState
			    end,

	case IntermediateState of
		ok ->
			Data = lists:nth( 1, ets:lookup( list_streets , Vertices ) ),
			{ _ , _ , _ , _ , _ , _ , From , _ , NumCars , Tick , MaxCar } = Data,

			case Tick /= CurrentTick of 
				true ->

					ets:update_element( list_streets , Vertices , { 9 , 0 }),
					ets:update_element( list_streets , Vertices , { 10 , CurrentTick });

				false ->

					ok

			end,

			case NumCars >= MaxCar of

				true ->

					FinalState = setAttributes( State , [ { wait, true } ] ),
					executeOneway( FinalState , addSpontaneousTick , CurrentTick + 1 );

				false ->

					DecrementVertex = getAttribute( State , last_vertex_pid ),
					case DecrementVertex of
						ok ->
							ok;
						_ ->
							ets:update_counter( list_streets, DecrementVertex , { 6 , -1 })
					end,	

					ets:update_counter( list_streets , Vertices , { 6 , 1 }),

					NewPath = lists:nthtail( 1 , Path ),

					ets:update_counter( list_streets , Vertices , { 9 , 1 }),

					{ Id , Time , Distance } = traffic_models:get_speed_car( Data ),

					TotalLength = getAttribute( State , distance ) + Distance,
					FinalState = setAttributes( State , [{ wait , false } , {distance , TotalLength} , {car_position , Id} , {last_vertex_pid , Vertices} , {path , NewPath},  { coordFrom , From } ] ), 

					%	send data to rabbitMQ, including the From lat/long

					io:format("."),
					Uuid = getAttribute( FinalState, uuid ),
					Channel = getAttribute( FinalState, channel ),
					spawn( print, formatAndPublish, [ Uuid, atom_to_list(Id), CurrentTick, Channel ] ),

					executeOneway( FinalState , addSpontaneousTick , CurrentTick + Time )

			end;
		NewState ->
			executeOneway( NewState , addSpontaneousTick , CurrentTick + 1 )
	end.


best_path( State, Path, Origin ) ->
	GraphManagerPid = getAttribute( State, graph_manager ),

	[ Destination ] = lists:nthtail(length(Path)-1, Path),

	GraphManagerPid ! { get_best_path, Origin, Destination, self() }, 
	BestPath = receive
			   { best_path, PathCalculated } -> PathCalculated
		   end,

	setAttributes( State , [ { path, BestPath } ] ).


edgeInPath( [], _From, _To ) -> false;
edgeInPath( [ Node | Path ], From, To ) ->
	case Node =:= From of
		true ->
			case lists:nth( 1 , Path ) =:= To of
				true -> true;
				false -> edgeInPath( Path, From, To)
			end;
		false -> edgeInPath( Path, From, To)
	end.


get_parking_spot( State , IdNode , _ParkingPID ) ->
	Node = element( 1 , IdNode ),
	case Node of 
	     nok ->
		io:format( "nao disponivel");
    	     _ ->
		{ Path , City } = { getAttribute( State , path ), ets:lookup_element(options, city_pid, 2 ) },
		CurrentVertice = lists:nth( 1 , Path ),
		class_Actor:send_actor_message( City , { get_path, { CurrentVertice , Node } } , State )
	end.
 
set_new_path( State , NewPath , _CityPID ) ->
	Path = element( 1 , NewPath ), 
	StateDict = setAttributes( State , [ { path , Path } , { park_status , finish } ] ),
	Trips = getAttribute( StateDict , trips ), 
	CurrentTrip = list_utils:get_element_at( Trips , 1 ),
        request_position( StateDict , CurrentTrip , Path ).

-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->
	StartTime = getAttribute( State , start_time ),
    	FirstActionTime = class_Actor:get_current_tick_offset( State ) + StartTime,   	
	NewState = setAttribute( State , start_time , FirstActionTime ),
	executeOneway( NewState , addSpontaneousTick , FirstActionTime ).

%print_movement( State ) ->

%	LastPosition = getAttribute( State , car_position ),

%	{ Trips , CurrentTickOffset , CarId , Type , NewPosition }
 %            = { getAttribute( LengthState , trips ), class_Actor:get_current_tick_offset( State ) , 
  %               getAttribute( State , car_name ) , getAttribute( State , type ) , getAttribute( LengthState , car_position ) },
%	CurrentTrip =  lists:nth( 1 , Trips ),

%	FinalState = case LastPosition == -1 of

%		false ->
			
%			print:write_movement_car_message( LengthState , CarId , LastPosition , Type , ets:lookup_element(options, log_pid, 2 ) , CurrentTickOffset , NewPosition , csv  );
 

%		true -> 

%			LinkOrigin = element( 3 , CurrentTrip ), 

%			print:write_initial_message( LengthState , ets:lookup_element(options, log_pid, 2 ) , CarId , Type , CurrentTickOffset , LinkOrigin , LastPosition , csv )
	   
%	end.
