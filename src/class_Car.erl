%Class that represents a person that can moves around the city graph on foot or by car
-module(class_Car).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName , ListTripsFinal , StartTime , Type , Park , Mode ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/7, new_link/7,
		 synchronous_new/7, synchronous_new_link/7,
		 synchronous_timed_new/7, synchronous_timed_new_link/7,
		 remote_new/8, remote_new_link/8, remote_synchronous_new/8,
		 remote_synchronous_new_link/8, remote_synchronisable_new_link/8,
		 remote_synchronous_timed_new/8, remote_synchronous_timed_new_link/8,
		 construct/8, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, get_parking_spot/3 , set_new_path/3 ).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, CarName ),

	NewState = setAttributes( ActorState, [
		{ car_name, CarName },
		{ trips , ListTripsFinal },
		{ type, Type },
		{ distance , 0 },
		{ car_position, -1 },
		{ start_time , StartTime },
		{ path , ok },
		{ park , Park },
		{ mode , Mode },
		{ last_vertex_pid , ok }
						] ),

	case Park of
		ok ->
			setAttribute( NewState , park_status , finish );
		_ ->
			setAttribute( NewState , park_status , find )
	end.

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

					FinalState = print:write_final_message( NewState , Type , TotalLength , StartTime , CarId , CurrentTickOffset , LastPosition , ets:lookup_element(options, log_pid, 2 ) , Mode , csv ),

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
			setAttribute( State, path , PathTrip );

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
		
			DecrementVertex = getAttribute( State , last_vertex_pid ),
	
			ets:update_counter( list_streets, DecrementVertex , { 6 , -1 }),
			State;
		_ ->		
			State

	end,	

	{ Park , ParkStatus } = { getAttribute( NewState , park ), getAttribute( NewState , park_status ) },

	case ParkStatus of

		finish ->

			NewNewState = case Park of

				ok ->
		
					NewState;

				_ ->
					
					Parking = ets:lookup_element(options, parking_pid, 2 ),
					class_Actor:send_actor_message( Parking, { spot_in_use, { Park } } , NewState )
	
			end,
									
			FinalState = setAttribute( NewNewState, path , finish ),

			executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + 1 );
		find ->

			Parking = ets:lookup_element(options, parking_pid, 2 ),
			class_Actor:send_actor_message( Parking, { spot_available, { Park } } , NewState )

	end.




get_next_vertex( State , Path , Trip ) ->

	% get the current and the next vertex in the path	
	{ InitialVertice , FinalVertice } = { lists:nth( 1 , Path ) , lists:nth( 2 , Path ) },

	Mode = element( 1 , Trip ),
					
	Vertices = list_to_atom( lists:concat( [ InitialVertice , FinalVertice ] )),

	FinalState = setAttribute( State , path , list_utils:remove_element_at( Path , 1 ) ),

	case Mode  of

		"walk" ->		
					
			Data = lists:nth( 1, ets:lookup( list_streets , Vertices ) ),
			StreetData = traffic_models:get_speed_walk( Data ),
                        go( FinalState , StreetData );

		_ ->		

			DecrementVertex = getAttribute( FinalState , last_vertex_pid ),
			FinalState2 = case RemovePID of
				ok ->
					FinalState;
				_ ->
					ets:update_counter( list_streets, DecrementVertex , { 6 , -1 }),
					FinalState
			end,
		
			FinalStateCar = setAttribute( FinalState2 , last_vertex_pid , Vertices ),		
		
			ets:update_counter( list_streets , Vertices , { 6 , 1 }),
			Data = lists:nth( 1, ets:lookup( list_streets , Vertices ) ),

			StreetData = traffic_models:get_speed_car( Data ),

                        go( FinalStateCar , StreetData )
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

        request_position( StateDict , CurrentTrip ).

-spec go( wooper:state(), car_position() ) -> class_Actor:actor_oneway_return().
go( State, PositionTime ) ->

	TotalTime = class_Actor:get_current_tick_offset( State ) + element( 2 , PositionTime ), % CurrentTime + Time to pass the link

%	LastPosition = getAttribute( State , car_position ),
	% Calculate the total distance that the person moved until now.
	TotalLength = getAttribute( State , distance ) + element( 3 , PositionTime),
	LengthState = setAttributes( State , [ { distance , TotalLength } , { car_position , element( 1 , PositionTime ) } ] ), 

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
	   


%	end,

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


