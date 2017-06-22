%Class that represents a bus that can moves around the city graph
-module(class_Bus).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, BusName, ListVertex , Path , StartTime , Interval , LogPID ).

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
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, go/3 ).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->


	ActorState = class_Actor:construct( State, ActorSettings, BusName ),

        DictVertices = dict:from_list( ListVertex ),

	setAttributes( ActorState, [
		{ bus_name, BusName },
		{ dict , DictVertices },
		{ log_pid, LogPID },
		{ distance , 0 },
		{ car_position, -1 },
		{ start_time , StartTime },	
		{ path , Path },
		{ interval , Interval },
		{ next_bus , { StartTime , 1 } },
		{ buses , dict:new( ) },
		{ buses_time , dict:new() }
						] ).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->
	
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

	NextBus = getAttribute( State , next_bus ), 
	
	BusState = case CurrentTickOffset == element( 1 , NextBus ) of

		true ->

			BusName = getAttribute( State , bus_name ), 				

			Id = io_lib:format( "~s~B", [ BusName , element( 2 , NextBus ) ] ),
			
			Interval = getAttribute( State , interval ), 

			Buses = getAttribute( State , buses ), 
		
			NewState = setAttribute( State, next_bus , { CurrentTickOffset + Interval , element( 2 , NextBus ) + 1 } ),

			Bus = [ 1 , Id , CurrentTickOffset , -1 ], % Current Position, Id, Start_time, Last Position

			NewDict = dict:store( Id , Bus , Buses ),

			NewDictState = setAttribute( NewState , buses , NewDict ),

			FinalState = request_position( NewDictState , Bus ),
			
			executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + Interval );	

		_ ->

			State



	end,

	ScheduledBuses = getAttribute( BusState , buses_time ), 

	case dict:is_key( CurrentTickOffset , ScheduledBuses ) of

		true ->

			CurrentBuses = element( 2 , dict:find( CurrentTickOffset , ScheduledBuses ) ), % element 1 is just an ok

			NewBuses = dict:erase( CurrentTickOffset , ScheduledBuses ), % remove the current tick from the dick to save memory;
			
			DictState = setAttribute( BusState, buses_time , NewBuses ),

			request_position_buses( DictState , CurrentBuses );


		false ->

			BusState % Nothing to do

	end.

	


request_position_buses( State , [] ) ->
	State;

request_position_buses( State ,  [ Bus | Buses ] ) ->
	
	NewState = request_position( State ,  Bus ),

	request_position_buses( NewState , Buses ).
	
	
-spec request_position( wooper:state() , parameter() ) -> wooper:state().
request_position( State , Bus ) ->
	
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 	
		
	Path = getAttribute( State , path ),

	Position = list_utils:get_element_at( Bus , 1 ),


	IdBus = list_utils:get_element_at( Bus , 2 ),

	case length( Path ) > Position of

		true ->	

			% get the current and the next vertex in the path	
			InitialVertice = list_utils:get_element_at( Path , Position ),

			FinalVertice = list_utils:get_element_at( Path , Position + 1 ),

			DictVertices = getAttribute( State , dict ),

			Vertices = list_to_atom( lists:concat( [ InitialVertice , FinalVertice ] ) ),

			VertexPID = element( 2 , dict:find( InitialVertice , DictVertices)),	
				
			class_Actor:send_actor_message( VertexPID ,
				{ getPosition, { Vertices , "bus" , IdBus } }, State );

		false ->							
					
			LastPosition = list_utils:get_element_at( Bus , 4 ),

			write_final_message( State , CurrentTickOffset , IdBus , LastPosition ) 

	end.

	

% Called by the route with the requested position. Write the file to show the position of the car in the map.
%
% (actor oneway)
%
-spec go( wooper:state(), value(), pid() ) -> class_Actor:actor_oneway_return().
go( State, PositionTime , _GraphPID ) ->
	
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), % get the current time of the simulation

	% get the response from the city graph
	NewPosition = element( 1 , PositionTime ),
	Time = element( 2 , PositionTime),
  	BusId = element( 3 , PositionTime ),

	Buses = getAttribute( State , buses ), 

	Bus = element( 2 , dict:find( BusId , Buses ) ), % dict:find returns { ok , Object }

	LastPosition = list_utils:get_element_at( Bus , 4 ),
		
	LogState = case LastPosition == -1 of

		false ->

			write_movement_message( State , CurrentTickOffset , BusId , LastPosition , NewPosition , BusId );
			

		true -> 

			LinkOrigin = "1", % getAttribute( State , link_origin ), 

			write_initial_message( State , CurrentTickOffset , BusId , LinkOrigin , NewPosition )	   
   			

	end,

	NewBus = [ list_utils:get_element_at( Bus , 1 ) + 1 , list_utils:get_element_at( Bus , 2 ) , list_utils:get_element_at( Bus , 3 ), NewPosition ],
	
	NewDictBuses = dict:store( BusId , NewBus , Buses ),

	BusesState = setAttribute( LogState , buses , NewDictBuses ),

	ScheduledBuses = getAttribute( BusesState , buses_time ), 

	FinalState = case dict:is_key( CurrentTickOffset + Time , ScheduledBuses ) of

		true ->

			ListBuses = element( 2 , dict:find( CurrentTickOffset + Time , ScheduledBuses ) ),
			
			NewScheduledBuses = dict:store( CurrentTickOffset + Time , ListBuses ++ [ NewBus ] , ScheduledBuses ),
			
			setAttribute( BusesState , buses_time , NewScheduledBuses );


		false ->

			NewScheduledBuses = dict:store( CurrentTickOffset + Time , [ NewBus ] , ScheduledBuses ),
			
			setAttribute( BusesState , buses_time , NewScheduledBuses )

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

	Path = getAttribute( State , path ),

	case Path of 
	
		false ->

			executeOneway( State , declareTermination );

		_ ->

			ScheduledState = executeOneway( State , addSpontaneousTick, CurrentTickOffset + Time ),

			?wooper_return_state_only( ScheduledState )
	
	end.






% Functions to write the data to the log files.

write_final_message( State , CurrentTickOffset , BusId , LastPosition ) ->

	LeavesTraffic = io_lib:format( "<event time=\"~w\" type=\"vehicle leaves traffic\" person=\"~s\" link=\"~s\" vehicle=\"~s\" relativePosition=\"1.0\" />\n", [ CurrentTickOffset , BusId , LastPosition , BusId ] ),
	
	LeavesVehicles = io_lib:format( "<event time=\"~w\" type=\"PersonLeavesVehicle\" person=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , BusId , BusId ] ),
				
	Arrival = io_lib:format( "<event time=\"~w\" type=\"arrival\" person=\"~s\" vehicle=\"~s\" link=\"~s\" legMode=\"car\" />\n", [ CurrentTickOffset , BusId , BusId ,  LastPosition ] ),

	ActStart = io_lib:format( "<event time=\"~w\" type=\"actstart\" person=\"~s\"  link=\"~s\"  actType=\"h\"  />\n", [ CurrentTickOffset , BusId , LastPosition ] ),

	TextFile = lists:concat( [ LeavesTraffic , LeavesVehicles , Arrival , ActStart ] ),

	LogPid = ?getAttr(log_pid),
				
	class_Actor:send_actor_message( LogPid , { receive_action, { TextFile } }, State ).

write_initial_message( State , CurrentTickOffset , BusId , LinkOrigin , NewPosition ) ->

	Text1 = io_lib:format( "<event time=\"~w\" type=\"actend\" person=\"~s\" link=\"~s\" actType=\"h\" />\n", [ CurrentTickOffset , BusId , LinkOrigin ] ),
   	Text2 = io_lib:format( "<event time=\"~w\" type=\"departure\" person=\"~s\" link=\"~s\" legMode=\"car\" />\n", [ CurrentTickOffset , BusId , LinkOrigin ] ),
  	Text3 = io_lib:format( "<event time=\"~w\" type=\"PersonEntersVehicle\" person=\"~s\" vehicle=\"~s\" />\n", [ CurrentTickOffset , BusId , BusId ] ),
  	Text4 = io_lib:format( "<event time=\"~w\" type=\"wait2link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" />\n", [ CurrentTickOffset , BusId , LinkOrigin , BusId ] ),
  					
	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" />\n", [  CurrentTickOffset , BusId , atom_to_list( NewPosition ) , BusId ] ),

	TextFile = lists:concat( [ Text1 , Text2 , Text3 , Text4 , NextPositionText  ] ),

	LogPID = ?getAttr(log_pid),
			
	class_Actor:send_actor_message( LogPID,	{ receive_action, { TextFile } }, State ).

write_movement_message( State , CurrentTickOffset , BusId , LastPosition , NewPosition , BusId ) ->

	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" />\n", [ CurrentTickOffset , BusId , atom_to_list(LastPosition) , BusId ] ),
	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" />\n", [  CurrentTickOffset , BusId , atom_to_list(NewPosition) , BusId ] ),

	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

	LogPID = ?getAttr(log_pid),

	class_Actor:send_actor_message( LogPID , { receive_action, { TextFile } }, State ).


