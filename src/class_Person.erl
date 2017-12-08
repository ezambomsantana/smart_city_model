%Class that represents a person that can moves around the city graph on foot or by car
-module(class_Person).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName, ListVertex , ListTripsFinal , StartTime , Type , Mode , PID ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/8, new_link/8,
		 synchronous_new/8, synchronous_new_link/8,
		 synchronous_timed_new/8, synchronous_timed_new_link/8,
		 remote_new/9, remote_new_link/9, remote_synchronous_new/9,
		 remote_synchronous_new_link/9, remote_synchronisable_new_link/9,
		 remote_synchronous_timed_new/9, remote_synchronous_timed_new_link/9,
		 construct/9, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, go/3 , metro_go/3 , bus_go/3 ).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter() , parameter() , parameter()  ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, CarName ),

        DictVertices = dict:from_list( ListVertex ),

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
		{ cost , 3.8 },
		{ metro , element ( 2 , PID ) },
		{ mode , Mode },
		{ pt_status , start } %public transport -> bus or metro
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

			Mode = element( 1 , CurrentTrip ),			

			case Mode of 

				"walk" ->
	
					NewState = request_position( State , CurrentTrip  ),
					?wooper_return_state_only( NewState );

				"bus" ->

					PtStatus = getAttribute( State , pt_status ), 

					case PtStatus of 
	
	
						finish ->


							NewTrips = list_utils:remove_element_at( Trips , 1 ),

							NewState = setAttributes( State , [ {trips , NewTrips } , {  pt_status , start } ] ),					

							executeOneway( NewState , scheduleNextSpontaneousTick );

						start ->
								
							NewState = request_position_bus( State , CurrentTrip ),

							?wooper_return_state_only( NewState )

					end;


				"metro" ->
			
					PtStatus = getAttribute( State , pt_status ), 

					case PtStatus of 
	
	
						finish ->
							
							NewTrips = list_utils:remove_element_at( Trips , 1 ),

							NewState = setAttributes( State , [ {trips , NewTrips } , {  pt_status , start } ] ),					
							executeOneway( NewState , scheduleNextSpontaneousTick );

						start ->
	
							NewState = request_position_metro( State , CurrentTrip ),

							?wooper_return_state_only( NewState )

					end

					


			end


	end.

-spec request_position_bus( wooper:state() , parameter() ) -> wooper:state().
request_position_bus( State , Trip ) -> 

	Origin = element( 2 , Trip ),

	Destination = element( 3 , Trip ), 

	Line = element( 4 , Trip ), 

	DictVertices = getAttribute( State , dict ),

	VertexPID = element( 2 , dict:find( list_to_atom( Origin ) , DictVertices) ),	% get the pid of the bus stop vertex

	class_Actor:send_actor_message( VertexPID ,
		{ wait_bus , { Destination , Line } }, State ).

			
-spec request_position_metro( wooper:state() , parameter() ) -> wooper:state().
request_position_metro( State , Trip ) -> 

	Origin = element( 2 , Trip ),

	Destination = element( 4 , Trip ), 

	class_Actor:send_actor_message( ?getAttr( metro ) ,
		{ getTravelTime, { Origin , Destination } }, State ).



-spec metro_go( wooper:state(), value(), pid() ) -> class_Actor:actor_oneway_return().
metro_go( State, PositionTime , _GraphPID ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

	TotalTime = CurrentTickOffset + element( 1 , PositionTime ), % CurrentTime + Time to pass the link

	Trips = getAttribute( State , trips ), 
	
	Trip = list_utils:get_element_at( Trips , 1 ),

	Destination = element( 5 , Trip ), 

	PositionState = setAttributes( State , [ { car_position, list_to_atom( Destination ) } , { pt_status , finish } ] ),

	CarId = getAttribute( PositionState , car_name ),
  	Type = getAttribute( PositionState , type ),
	FinalState = print:write_movement_bus_metro_message( PositionState , CurrentTickOffset , 0 , CarId , Type , Destination , bus , ?getAttr(log_pid) , csv ),

	executeOneway( FinalState , addSpontaneousTick, TotalTime ).

-spec bus_go( wooper:state(), value(), pid() ) -> class_Actor:actor_oneway_return().
bus_go( State, _PositionTime , _GraphPID ) ->

	% get the current time of the simulation
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

	Trips = getAttribute( State , trips ), 
	
	Trip = list_utils:get_element_at( Trips , 1 ),

	Destination = element( 6 , Trip ), 

	PositionState = setAttributes( State , [ { car_position, list_to_atom( Destination ) } , { pt_status , finish } ] ),

	CarId = getAttribute( PositionState , car_name ),
  	Type = getAttribute( PositionState , type ),

	FinalState = print:write_movement_bus_metro_message( PositionState , CurrentTickOffset , 0 , CarId , Type , Destination , bus , ?getAttr(log_pid) , csv ),

	executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + 1 ).



-spec request_position( wooper:state() , parameter() ) -> wooper:state().
request_position( State , Trip ) ->
	
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 	

	PathTest = getAttribute( State , path ),

	PathState = case PathTest of

		ok -> 
			
			PathTrip = element( 5 , Trip ),
			setAttribute( State, path, PathTrip );

		_ ->

			State

	end,

			
	Path = getAttribute( PathState , path ),

	case Path of 

		finish ->

			Trips = getAttribute( State , trips ), 
			
			NewTrips = list_utils:remove_element_at( Trips , 1 ),

			NewState = setAttributes( State , [ { trips , NewTrips } , { path, ok } ] ),

			executeOneway( NewState , addSpontaneousTick , CurrentTickOffset + 1 );	
	
		false ->

			State;

		_ ->

			case length( Path ) > 1 of

				true ->	

					% get the current and the next vertex in the path	
					InitialVertice = list_utils:get_element_at( Path , 1 ),

					FinalVertice = list_utils:get_element_at( Path , 2 ),

					DictVertices = getAttribute( PathState , dict ),

					VertexPID = dict:find( InitialVertice , DictVertices),
					
					Vertices = list_to_atom( lists:concat( [ InitialVertice , FinalVertice ] )),

					FinalState = setAttribute( PathState , path, list_utils:remove_element_at( Path , 1 ) ), % remove the current element of the path
			
					class_Actor:send_actor_message( element( 2 , VertexPID ) ,
						{ get_speed_walk, { Vertices } }, FinalState );

				false ->							

					LastPosition = getAttribute( PathState , car_position ),

					case LastPosition == -1 of

						true ->
							
							executeOneway( PathState , declareTermination );	

						false ->	

						
							FinalState = setAttribute( PathState, path, finish ),

							executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + 1 )
						
					end

			end

	end.

-spec go( wooper:state(), car_position() , parameter() ) -> class_Actor:actor_oneway_return().
go( State, PositionTime , _GraphPID ) ->

	TotalTime = class_Actor:get_current_tick_offset( State ) + element( 2 , PositionTime ), % CurrentTime + Time to pass the link

	% Calculate the total distance that the person moved until now.
	TotalLength = getAttribute( State , distance ) + element( 3 , PositionTime),
	LengthState = setAttributes( State,  [ { distance , TotalLength } , { car_position , element( 1 , PositionTime ) } ] ),
		
	LastPosition = getAttribute( State , car_position ),
	NewPosition = getAttribute( LengthState , car_position ),

	Trips = getAttribute( LengthState , trips ), 

	CurrentTrip = list_utils:get_element_at( Trips , 1 ),

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 	
	CarId = getAttribute( State , car_name ),
  	Type = getAttribute( State , type ),

	FinalState = case LastPosition == -1 of

		false ->
			
			print:write_movement_car_message( LengthState , CarId , LastPosition , Type , ?getAttr(log_pid) , CurrentTickOffset , NewPosition , csv  );
 

		true -> 

			LinkOrigin = element( 3 , CurrentTrip ), 

			print:write_initial_message( LengthState , ?getAttr(log_pid) , CarId , Type , CurrentTickOffset , LinkOrigin , LastPosition , csv )

	end,

	executeOneway( FinalState , addSpontaneousTick , TotalTime ).


% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

    	FirstActionTime = class_Actor:get_current_tick_offset( State ) + 10,   	

	executeOneway( State , addSpontaneousTick , FirstActionTime ).
