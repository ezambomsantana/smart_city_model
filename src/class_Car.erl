%Class that represents a person that can moves around the city graph on foot or by car
-module(class_Car).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName, ListVertex , ListTripsFinal , StartTime , Type , Park , Mode , PID ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/9, new_link/9,
		 synchronous_new/9, synchronous_new_link/9,
		 synchronous_timed_new/9, synchronous_timed_new_link/9,
		 remote_new/10, remote_new_link/10, remote_synchronous_new/10,
		 remote_synchronous_new_link/10, remote_synchronisable_new_link/10,
		 remote_synchronous_timed_new/10, remote_synchronous_timed_new_link/10,
		 construct/10, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, go/3 , get_parking_spot/3 , set_new_path/3 ).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter() , parameter() , parameter() , parameter()  ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, CarName ),

        DictVertices = dict:from_list( ListVertex ),

	ParkStatus = case Park of
		ok ->
			finish;
		_ ->
			find
	end,

	setAttributes( ActorState, [
		{ car_name, CarName },
		{ dict , DictVertices },
		{ trips , ListTripsFinal },
		{ log_pid, element ( 1 , PID ) },
		{ type, Type },
		{ distance , 0 },
		{ car_position, -1 },
		{ start_time , StartTime },
		{ path , ok },
		{ parking , element ( 3 , PID ) },
		{ city , element ( 4 , PID ) },
		{ park , Park },
		{ park_status , ParkStatus },
		{ mode , Mode },
		{ last_vertex_pid , ok }
						] ).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->
	
	Trips = getAttribute( State , trips ), 
	
	case length( Trips ) > 0 of

		false ->		
			
			Path = getAttribute( State , path ), 

			case Path of 

				finish -> 
					
					executeOneway( State , declareTermination );

				_ ->

					NewState = setAttribute( State , path , finish ),

					Type = getAttribute( NewState , type ),
						
					TotalLength = getAttribute( NewState , distance ),

					StartTime = getAttribute( NewState , start_time ),

					CarId = getAttribute( NewState , car_name ),	

					CurrentTickOffset = class_Actor:get_current_tick_offset( NewState ), 

					LastPosition = getAttribute( NewState , car_position ),

					Mode = getAttribute( NewState , mode ), 

					FinalState = print:write_final_message( NewState , Type , TotalLength , StartTime , CarId , CurrentTickOffset , LastPosition , ?getAttr(log_pid) , Mode , csv ),

					executeOneway( FinalState, scheduleNextSpontaneousTick )

				end;

		true ->

			CurrentTrip = list_utils:get_element_at( Trips , 1 ),		

			NewState = request_position( State , CurrentTrip ),
			?wooper_return_state_only( NewState )

	end.

-spec request_position( wooper:state() , parameter() ) -> wooper:state().
request_position( State , Trip ) ->
	
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 	

	PathTest = getAttribute( State , path ),

	PathState = case PathTest of

		ok -> 
			
			PathTrip = element( 2 , Trip ),
			setAttribute( State, path, PathTrip );

		_ ->

			State

	end,

			
	Path = getAttribute( PathState , path ),

	case Path of 

		finish ->

			Trips = getAttribute( State , trips ), 
			
			NewTrips = list_utils:remove_element_at( Trips , 1 ),

			NewState = setAttributes( State , [ { trips , NewTrips } , { path, ok} ] ),

			executeOneway( NewState , addSpontaneousTick , CurrentTickOffset + 1 );	
	
		false ->

			State;

		_ ->

			case length( Path ) > 1 of

				true ->	

					get_next_vertex( PathState , Path , Trip );

				false ->							

					LastPosition = getAttribute( PathState , car_position ),

					case LastPosition == -1 of

						true ->
							
							executeOneway( PathState , declareTermination );	

						false ->

							verify_park( PathState , Trip , CurrentTickOffset )
					
					end

			end

	end.

verify_park( State , Trip , CurrentTickOffset ) ->

	NewState = case element( 1 , Trip ) of % mode

		"car" ->							
		
			RemovePID = getAttribute( State , last_vertex_pid ),
	
			class_Actor:send_actor_message( element( 1 , RemovePID ) ,
				 { decrement_vertex_count, { element( 2 , RemovePID) } }, State );
		_ ->		
			State

	end,	

	Park = getAttribute( NewState , park ),

	ParkStatus = getAttribute( NewState , park_status ),

	Parking = getAttribute( NewState , parking ),

	case ParkStatus of

		finish ->

			NewNewState = case Park of

				ok ->
		
					NewState;

				_ -> class_Actor:send_actor_message( Parking, { spot_in_use, { Park } } , NewState )
	
			end,
									
			FinalState = setAttribute( NewNewState, path, finish ),

			executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + 1 );
		find ->

			class_Actor:send_actor_message( Parking, { spot_available, { Park } } , NewState )

	end.




get_next_vertex( State , Path , Trip ) ->

	% get the current and the next vertex in the path	
	InitialVertice = list_utils:get_element_at( Path , 1 ),

	FinalVertice = list_utils:get_element_at( Path , 2 ),

	DictVertices = getAttribute( State , dict ),

	Mode = element( 1 , Trip ),

	VertexPID = dict:find( InitialVertice , DictVertices),
					
	Vertices = list_to_atom( lists:concat( [ InitialVertice , FinalVertice ] )),

	FinalState = setAttribute( State , path , list_utils:remove_element_at( Path , 1 ) ),

	case Mode  of

		"walk" ->							

			class_Actor:send_actor_message( element( 2 , VertexPID ) ,
				{ get_speed_walk, { Vertices } }, FinalState );

		_ ->		

			RemovePID = getAttribute( FinalState , last_vertex_pid ),
			FinalState2 = case RemovePID of
				ok ->
					FinalState;
				_ ->
					class_Actor:send_actor_message( element( 1 , RemovePID ) ,
							{ decrement_vertex_count, { element( 2 , RemovePID) } }, FinalState )
			end,

			FinalStateCar = setAttribute( FinalState2 , last_vertex_pid , { element( 2 , VertexPID ) , Vertices } ),
					
			class_Actor:send_actor_message( element( 2 , VertexPID ) ,
				{ get_speed_car, { Vertices } }, FinalStateCar )
	end.

get_parking_spot( State , IdNode , _ParkingPID ) ->

	Node = element( 1 , IdNode ),

	case Node of 

	     nok ->

		io:format( "nao disponivel");

    	     _ ->

		Path = getAttribute( State , path ),

		CurrentVertice = list_utils:get_element_at( Path , 1 ),

		City = getAttribute( State , city ), 

		class_Actor:send_actor_message( City , { get_path, { CurrentVertice , Node } } , State )

	end.
 
set_new_path( State , NewPath , _CityPID ) ->

	Path = element( 1 , NewPath ), 

	DictNewVertices = dict:from_list( element( 2 , NewPath ) ),

	StateDict = setAttributes( State , [ { dict , DictNewVertices } , { path , Path } , { park_status , finish } ] ),

	Trips = getAttribute( StateDict , trips ), 

	CurrentTrip = list_utils:get_element_at( Trips , 1 ),

        request_position( StateDict , CurrentTrip ).

-spec go( wooper:state(), car_position() , parameter() ) -> class_Actor:actor_oneway_return().
go( State, PositionTime , _GraphPID ) ->

	TotalTime = class_Actor:get_current_tick_offset( State ) + element( 2 , PositionTime ), % CurrentTime + Time to pass the link

	% Calculate the total distance that the person moved until now.
	TotalLength = getAttribute( State , distance ) + element( 3 , PositionTime),
	LengthState = setAttributes( State , [ { distance , TotalLength } , { car_position , element( 1 , PositionTime ) } ] ), 

%	CurrentTrip = list_utils:get_element_at( Trips , TripIndex ),

	%FinalState = case LastPosition == -1 of

	%	false ->
			
	%		write_movement_car_message( NewState , CurrentTickOffset , CarId , LastPosition , NewPosition , Type );


	%	true -> 

	%		LinkOrigin = element( 3 , CurrentTrip ), if it return, it is necessary to change the create_agents.erl

	%		write_initial_message( NewState , CurrentTickOffset , CarId , LinkOrigin , NewPosition , Type )
	   


	%end,

	executeOneway( LengthState , addSpontaneousTick , TotalTime ).


% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->


	StartTime = getAttribute( State , start_time ),

    	FirstActionTime = class_Actor:get_current_tick_offset( State ) + StartTime,   	

	NewState = setAttribute( State , start_time , FirstActionTime ),

	executeOneway( NewState , addSpontaneousTick , FirstActionTime ).


