%Class that represents a person that can moves around the city graph on foot or by car
-module(class_Person).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName, ListVertex , Origin, Path , StartTime , LinkOrigin , LogPID , Type  , Mode).

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
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, go/3 ).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Smart-City.Car").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").

% Creates a new car
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter() , parameter() , parameter() , parameter()  , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->


	ActorState = class_Actor:construct( State, ActorSettings, CarName ),

        DictVertices = dict:from_list( ListVertex ),

	setAttributes( ActorState, [
		{ car_name, CarName },
		{ link_origin, LinkOrigin },
		{ dict , DictVertices },
		{ origin , Origin },
		{ path , Path },
		{ log_pid, LogPID },
		{ type, Type },
		{ distance , 0 },
		{ car_position, -1 },
		{ start_time , StartTime },
		{ mode , Mode }						
						] ).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->
	
	NewState = request_position( State ),
	
	?wooper_return_state_only( NewState ).

remove_first( [ _First | List ] ) ->
	
	List.
	

-spec request_position( wooper:state() ) -> wooper:state().
request_position( State ) ->

	Path = ?getAttr(path),

	case Path of 

		finish ->

			executeOneway( State , declareTermination );			
	
		false ->

			State;

		_ ->

			case length( Path ) > 1 of

				true ->	

					% get the current and the next vertex in the path	
					InitialVertice = list_utils:get_element_at( Path, 1 ),

					FinalVertice = list_utils:get_element_at( Path, 2 ),

					DictVertices = getAttribute( State , dict ),

					%the mode that the person will make the trip, walking or by car
					Mode = getAttribute( State , mode ),

					Vertices = list_to_atom(lists:concat( [ InitialVertice , FinalVertice ] )),

					VertexPID = element( 2 , dict:find( InitialVertice , DictVertices)),	

					PathRest = remove_first( Path ),

					FinalState = setAttribute( State , path, PathRest ),
	
					class_Actor:send_actor_message( VertexPID ,
						{ getPosition, { Vertices , Mode } }, FinalState );

				false ->							

					LastPosition = getAttribute( State , car_position ),

					case LastPosition == -1 of

						true ->
							
							executeOneway( State , declareTermination );	

						false ->
		
							
  							Type = getAttribute( State , type ),

							
							TotalLength = getAttribute( State , distance ),

							StartTime = getAttribute( State , start_time ),

							CarId = getAttribute( State , car_name ),	

							CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

							TotalTime =   CurrentTickOffset - StartTime, 	

							LeavesTraffic = io_lib:format( "<event time=\"~w\" type=\"vehicle leaves traffic\" person=\"~s\" link=\"~s\" vehicle=\"~s\" relativePosition=\"1.0\" />\n", [ CurrentTickOffset , CarId , atom_to_list(LastPosition) , CarId ] ),
							LeavesVehicles = io_lib:format( "<event time=\"~w\" type=\"PersonLeavesVehicle\" person=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , CarId , CarId ] ),
							
							Arrival = io_lib:format( "<event time=\"~w\" type=\"arrival\" person=\"~s\" vehicle=\"~s\" link=\"~s\" legMode=\"car\" trip_time=\"~w\" distance=\"~w\" action=\"~s\"/>\n", [ CurrentTickOffset , CarId , CarId ,  atom_to_list(LastPosition), TotalTime , TotalLength , Type ] ),
							ActStart = io_lib:format( "<event time=\"~w\" type=\"actstart\" person=\"~s\"  link=\"~s\"  actType=\"h\"  />\n", [ CurrentTickOffset , CarId , atom_to_list(LastPosition) ] ),
	
							TextFile = lists:concat( [ LeavesTraffic , LeavesVehicles , Arrival , ActStart ] ),
	
							LogPid = ?getAttr(log_pid),

							NewState = setAttribute( State, path, finish ),

							NewNewState = class_Actor:send_actor_message( LogPid ,
								{ receive_action, { TextFile } }, NewState ),

							executeOneway( NewNewState , addSpontaneousTick, CurrentTickOffset + 1 )

					end

			end

	end.

	

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

	FinalState = case LastPosition == -1 of

		false ->

			LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , atom_to_list(LastPosition) , CarId , Type ] ),
			NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId , Type ] ),

			TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

			LogPID = ?getAttr(log_pid),

			class_Actor:send_actor_message( LogPID,
				{ receive_action, { TextFile } }, NewState );

		true -> 

			LinkOrigin = getAttribute(State, link_origin),
	   
   			Text1 = io_lib:format( "<event time=\"~w\" type=\"actend\" person=\"~s\" link=\"~s\" actType=\"h\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , Type ] ),
   			Text2 = io_lib:format( "<event time=\"~w\" type=\"departure\" person=\"~s\" link=\"~s\" legMode=\"car\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , Type ] ),
  			Text3 = io_lib:format( "<event time=\"~w\" type=\"PersonEntersVehicle\" person=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , CarId , Type ] ),
  			Text4 = io_lib:format( "<event time=\"~w\" type=\"wait2link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [ CurrentTickOffset , CarId , LinkOrigin , CarId , Type ] ),
  	
			NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" action=\"~s\" />\n", [  CurrentTickOffset , CarId , atom_to_list(NewPosition) , CarId , Type ] ),

			TextFile = lists:concat( [ Text1 , Text2 , Text3 , Text4 , NextPositionText  ] ),

			LogPID = ?getAttr(log_pid),
			
			class_Actor:send_actor_message( LogPID,
				{ receive_action, { TextFile } }, NewState )

	end,

	executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + Time ).


% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	SimulationInitialTick = ?getAttr(initial_tick),

	% Checking:
	true = ( SimulationInitialTick =/= undefined ),

	Time = getAttribute( State, start_time ),

    	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),   	

	ScheduledState = executeOneway( State , addSpontaneousTick, CurrentTickOffset + Time ),

	?wooper_return_state_only( ScheduledState ).
