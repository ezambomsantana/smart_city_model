%Class that represents a person that can moves around the city graph on foot or by car
-module(class_Person).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName, ListVertex , ListTripsFinal , StartTime , LogPID , Type , Park , PID ).

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
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, go/3 , metro_go/3 , bus_go/3 , get_parking_spot/3 , set_new_path/3 ).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter() , parameter() , parameter() , parameter() ) -> wooper:state().
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
		{ trip_index , 1 },
		{ log_pid, LogPID },
		{ type, Type },
		{ distance , 0 },
		{ car_position, -1 },
		{ start_time , StartTime },
		{ path , ok },
		{ cost , 0 },
		{ metro , element ( 1 , PID ) },
		{ parking , element ( 2 , PID ) },
		{ city , element ( 3 , PID ) },
		{ park , Park },
		{ park_status , ParkStatus },
		{ pt_status , start } %public transport -> bus or metro
						] ).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->
	
	Trips = getAttribute( State , trips ), 

	TripIndex = getAttribute( State , trip_index ), 
	
	case TripIndex > length( Trips ) of

		true ->		
			
			Path = getAttribute( State , path ), 

			case Path of 

				finish -> 
					
					executeOneway( State , declareTermination );

				_ ->

					Type = getAttribute( State , type ),
							
					TotalLength = getAttribute( State , distance ),

					StartTime = getAttribute( State , start_time ),

					CarId = getAttribute( State , car_name ),	

					CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

					TotalTime =   CurrentTickOffset - StartTime, 	

					LastPosition = getAttribute( State , car_position ),

					NewState = setAttribute( State , path , finish ),

					Cost = getAttribute( State , cost ), 

					FinalState = write_final_message( NewState , CurrentTickOffset , CarId , LastPosition , TotalTime , TotalLength , Type , Cost ),

					executeOneway( FinalState, scheduleNextSpontaneousTick )

				end;


		_ ->

			CurrentTrip = list_utils:get_element_at( Trips , TripIndex ),

			Mode = element( 1 , CurrentTrip ),			

			case Mode of 

				"car" ->
	
					NewState = request_position( State , CurrentTrip ),
					?wooper_return_state_only( NewState );	

				"walk" ->
	
					NewState = request_position( State , CurrentTrip  ),
					?wooper_return_state_only( NewState );

				"bus" ->

					PtStatus = getAttribute( State , pt_status ), 

					case PtStatus of 
	
	
						finish ->
					
							NextTrip = getAttribute( State , trip_index ) + 1,

							NewState = setAttribute( State , trip_index , NextTrip ),

							FinalState = setAttribute( NewState , pt_status , start ),

							executeOneway( FinalState , scheduleNextSpontaneousTick );

						start ->
								
							NewState = request_position_bus( State , CurrentTrip ),

							?wooper_return_state_only( NewState )

					end;


				"metro" ->
			
					PtStatus = getAttribute( State , pt_status ), 

					case PtStatus of 
	
	
						finish ->
					
							NextTrip = getAttribute( State , trip_index ) + 1,

							NewState = setAttribute( State , trip_index , NextTrip ),

							FinalState = setAttribute( NewState , pt_status , start ),

							executeOneway( FinalState , scheduleNextSpontaneousTick );

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

	MetroPID = ?getAttr( metro ),

	class_Actor:send_actor_message( MetroPID ,
		{ getTravelTime, { Origin , Destination } }, State ).



-spec metro_go( wooper:state(), value(), pid() ) -> class_Actor:actor_oneway_return().
metro_go( State, PositionTime , _GraphPID ) ->

	% get the current time of the simulation
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

	% get the response from the city graph
	Time = element( 1 , PositionTime ),

  	CarId = getAttribute( State , car_name ),
  	Type = getAttribute( State , type ),

	Trips = getAttribute( State , trips ), 

	TripIndex = getAttribute( State , trip_index ), 
	
	Trip = list_utils:get_element_at( Trips , TripIndex ),

	Destination = element( 5 , Trip ), 

	LastPosition = getAttribute( State , car_position ),	

	CostState = setAttribute( State, cost, 3.8 ),

	PositionState = setAttribute( CostState , car_position, list_to_atom( Destination ) ),

	StatusState = setAttribute( PositionState , pt_status , finish ),

	FinalState = write_movement_metro_message( StatusState , CurrentTickOffset , CarId , LastPosition , Destination , CarId , Type ),

	executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + Time ).

-spec bus_go( wooper:state(), value(), pid() ) -> class_Actor:actor_oneway_return().
bus_go( State, _PositionTime , _GraphPID ) ->

	% get the current time of the simulation
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

  	CarId = getAttribute( State , car_name ),
  	Type = getAttribute( State , type ),

	Trips = getAttribute( State , trips ), 

	TripIndex = getAttribute( State , trip_index ), 
	
	Trip = list_utils:get_element_at( Trips , TripIndex ),

	Destination = element( 6 , Trip ), 

	LastPosition = getAttribute( State , car_position ),	

	CostState = setAttribute( State, cost, 3.8 ),

	PositionState = setAttribute( CostState , car_position, list_to_atom( Destination ) ),

	StatusState = setAttribute( PositionState , pt_status , finish ),

	FinalState = write_movement_bus_message( StatusState , CurrentTickOffset , CarId , LastPosition , Destination , CarId , Type ),

	executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + 1 ).




remove_first( [ _First | List ] ) ->
	
	List.	

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
			
			NextTrip = getAttribute( PathState , trip_index ) + 1,

			NewState = setAttribute( PathState , trip_index, NextTrip ),
			
			FinalState = setAttribute( NewState, path, ok ),

			executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + 1 );	
	
		false ->

			State;

		_ ->

			case length( Path ) > 1 of

				true ->	

					% get the current and the next vertex in the path	
					InitialVertice = list_utils:get_element_at( Path , 1 ),

					FinalVertice = list_utils:get_element_at( Path , 2 ),

					DictVertices = getAttribute( PathState , dict ),

					%the mode that the person will make the trip, walking or by car
					Mode = element( 1 , Trip ),

					No = dict:find( InitialVertice , DictVertices),
					
					Vertices = list_to_atom(lists:concat( [ InitialVertice , FinalVertice ] )),

					VertexPID = element( 2 , No ),	

					PathRest = remove_first( Path ),

					FinalState = setAttribute( PathState , path, PathRest ),
	
					class_Actor:send_actor_message( VertexPID ,
						{ getPosition, { Vertices , Mode } }, FinalState );

				false ->							

					LastPosition = getAttribute( PathState , car_position ),

					case LastPosition == -1 of

						true ->
							
							executeOneway( PathState , declareTermination );	

						false ->	

							Park = getAttribute( State , park ),

							ParkStatus = getAttribute( State , park_status ),

							Parking = getAttribute( State , parking ),

							case ParkStatus of

								finish ->

									NewState = case Park of

										ok ->
		
											State;

										_ -> class_Actor:send_actor_message( Parking, { spot_in_use, { Park } } , State )

									end,
									
									TotalLength = getAttribute( NewState , distance ),

									Mode = element( 1 , Trip ),

									CostState = case Mode of

										"car" ->
					
											Cost = TotalLength / 1000 * 0.70, 

											setAttribute( NewState , cost , Cost );	

										_ ->
	
											NewState

									end,				
		
									FinalState = setAttribute( CostState, path, finish ),

									executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + 1 );
								find ->

            								class_Actor:send_actor_message( Parking, { spot_available, { Park } } , State )

							end


					end

			end

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

	StateDict = setAttribute( State , dict , DictNewVertices ),

	NewState = setAttribute( StateDict , path , Path ),

	NewNewState = setAttribute( NewState , park_status , finish ),

	Trips = getAttribute( NewNewState , trips ), 

	TripIndex = getAttribute( NewNewState , trip_index ), 

	CurrentTrip = list_utils:get_element_at( Trips , TripIndex ),

        request_position( NewNewState , CurrentTrip ).

 
% Called by the route with the requested position. Write the file to show the position of the car in the map.
%
% (actor oneway)
%
-spec go( wooper:state(), value(), pid() ) -> class_Actor:actor_oneway_return().
go( State, PositionTime , _GraphPID ) ->

	move ( State , PositionTime ).

-spec move( wooper:state(), car_position() ) -> class_Actor:actor_oneway_return().
move( State, PositionTime ) ->

	% get the current time of the simulation
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

	% get the response from the city graph
	NewPosition = element( 1 , PositionTime ),
	Time = element( 2 , PositionTime),
	Length = element( 3 , PositionTime),

	% Calculate the total distance that the person moved until now.
	TotalLength = getAttribute( State , distance ) + Length,
	LengthState = setAttribute( State, distance , TotalLength ),

	LastPosition = getAttribute( LengthState , car_position ),	
	NewState = setAttribute( LengthState, car_position, NewPosition ),
		
  	CarId = getAttribute( LengthState , car_name ),
  	Type = getAttribute( LengthState , type ),

	TripIndex = getAttribute( State , trip_index ), 

	Trips = getAttribute( State , trips ), 
	
	CurrentTrip = list_utils:get_element_at( Trips , TripIndex ),

	FinalState = case LastPosition == -1 of

		false ->
			
			write_movement_car_message( NewState , CurrentTickOffset , CarId , LastPosition , NewPosition , Type );


		true -> 

			LinkOrigin = element( 3 , CurrentTrip ),

			write_initial_message( NewState , CurrentTickOffset , CarId , LinkOrigin , NewPosition , Type )
	   


	end,

	executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + Time ).


% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	Time = getAttribute( State, start_time ),

    	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),   	

	ScheduledState = executeOneway( State , addSpontaneousTick, CurrentTickOffset + Time ),

	?wooper_return_state_only( ScheduledState ).






write_final_message( State , CurrentTickOffset , CarId , LastPosition , TotalTime , TotalLength , Type , Cost ) ->

	LeavesTraffic = io_lib:format( "<event time=\"~w\" type=\"vehicle leaves traffic\" person=\"~s\" link=\"~s\" vehicle=\"~s\" relativePosition=\"1.0\" />\n", [ CurrentTickOffset , CarId , LastPosition , CarId ] ),
			
	LeavesVehicles = io_lib:format( "<event time=\"~w\" type=\"PersonLeavesVehicle\" person=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , CarId , CarId ] ),
						
	Arrival = io_lib:format( "<event time=\"~w\" type=\"arrival\" person=\"~s\" vehicle=\"~s\" link=\"~s\" legMode=\"car\" trip_time=\"~w\" distance=\"~w\" cost=\"~w\" action=\"~s\"/>\n", [ CurrentTickOffset , CarId , CarId ,  LastPosition, TotalTime , TotalLength , Cost , Type ] ),

	ActStart = io_lib:format( "<event time=\"~w\" type=\"actstart\" person=\"~s\"  link=\"~s\"  actType=\"h\"  />\n", [ CurrentTickOffset , CarId , LastPosition ] ),

	TextFile = lists:concat( [ LeavesTraffic , LeavesVehicles , Arrival , ActStart ] ),

	LogPid = ?getAttr(log_pid),

	class_Actor:send_actor_message( LogPid , { receive_action, { TextFile } }, State ).

write_initial_message( State , CurrentTickOffset , CarId , LinkOrigin , NewPosition , Type ) ->

   	Text1 = io_lib:format( "<event time=\"~w\" type=\"actend\" person=\"~s\" link=\"~s\" actType=\"h\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , Type ] ),
   	Text2 = io_lib:format( "<event time=\"~w\" type=\"departure\" person=\"~s\" link=\"~s\" legMode=\"car\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , Type ] ),
  	Text3 = io_lib:format( "<event time=\"~w\" type=\"PersonEntersVehicle\" person=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , CarId , Type ] ),
  	Text4 = io_lib:format( "<event time=\"~w\" type=\"wait2link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , CarId , Type ] ),
  	
	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId , Type ] ),

	TextFile = lists:concat( [ Text1 , Text2 , Text3 , Text4 , NextPositionText  ] ),

	LogPID = ?getAttr(log_pid),
		
	class_Actor:send_actor_message( LogPID, { receive_action, { TextFile } }, State ).


write_movement_car_message( State , CurrentTickOffset , CarId , LastPosition , NewPosition , Type ) ->

	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , atom_to_list(LastPosition) , CarId , Type ] ),
	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId , Type ] ),

	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

	LogPID = ?getAttr(log_pid),

	class_Actor:send_actor_message( LogPID,	{ receive_action, { TextFile } }, State ).

write_movement_bus_message( State , CurrentTickOffset , CarId , LastPosition , Destination , CarId , Type ) ->

	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"bus\" />\n", [ CurrentTickOffset , CarId , LastPosition , CarId , Type ] ),
	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"bus\" />\n", [  CurrentTickOffset , CarId , Destination , CarId , Type ] ),


	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

	LogPID = ?getAttr(log_pid),

	class_Actor:send_actor_message( LogPID, { receive_action, { TextFile } }, State ).

write_movement_metro_message( State , CurrentTickOffset , CarId , LastPosition , Destination , CarId , Type ) ->

	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"metro\" />\n", [ CurrentTickOffset , CarId , LastPosition , CarId , Type ] ),
	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" trip=\"metro\" />\n", [  CurrentTickOffset , CarId , Destination , CarId , Type ] ),


	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

	LogPID = ?getAttr(log_pid),

	class_Actor:send_actor_message( LogPID, { receive_action, { TextFile } }, State ).
