%Class that represents a bus that can moves around the city graph
-module(class_Bus).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, BusName, ListVertex , Path , StartTime , Interval , LogPID , Stops ).

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
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, go/3 , continue/3).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter() , parameter() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->


	ActorState = class_Actor:construct( State, ActorSettings, BusName ),

        DictVertices = dict:from_list( ListVertex ),

	DictStops = create_dict_stops( Stops , dict: new() ),

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
		{ buses_time , dict:new() },
		{ people_bus_stop , dict:new() },
		{ already_passed_bus_stop , dict:new() },
		{ stops , DictStops }
						] ).
create_dict_stops( [] , Dict ) ->

	Dict;

create_dict_stops( [ Stop | Stops ] , Dict ) ->

	NewDict = dict:store( list_to_atom( Stop ) , ok , Dict ),

	create_dict_stops( Stops , NewDict ).


-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->
	
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

	NextBus = getAttribute( State , next_bus ), 
	
	BusState = case CurrentTickOffset == element( 1 , NextBus ) of

		true ->

			Id = io_lib:format( "~s~B", [ getAttribute( State , bus_name ) , element( 2 , NextBus ) ] ),
			
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

			NewBuses = dict:erase( CurrentTickOffset , ScheduledBuses ), % remove the current tick from the tick to save memory;
			
			DictState = setAttribute( BusState , buses_time , NewBuses ),

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

	InitialVertice = list_utils:get_element_at( Path , Position ),

	IdBus = list_utils:get_element_at( Bus , 2 ),

	DictStops = getAttribute( State , stops ), 

	case dict:is_key( InitialVertice , DictStops ) of

		true ->

			BusStop = string:concat( IdBus , atom_to_list( InitialVertice ) ),

			AlreadyPassedBusStops = getAttribute( State , already_passed_bus_stop ),
			
			case dict:is_key( BusStop , AlreadyPassedBusStops ) of

				false ->

					DictFinal = dict:store( BusStop , ok , AlreadyPassedBusStops ),

					StatePassed = setAttribute( State , already_passed_bus_stop , DictFinal ), 				

					NewState = unload_people( StatePassed , IdBus , InitialVertice ),

					BusLine = getAttribute( NewState , bus_name ), 
					
					DictVertices = getAttribute( NewState , dict ),

					VertexPID = element( 2 , dict:find( InitialVertice , DictVertices )),	
				
					class_Actor:send_actor_message( VertexPID ,
						{ load_people , { BusLine , IdBus } }, NewState );

				true ->

					NewState = unload_people( State , IdBus , InitialVertice ),

					move( NewState , Path , Position , IdBus , InitialVertice  , Bus , CurrentTickOffset )

			end;
					

		false ->
			
			move( State , Path , Position , IdBus , InitialVertice  , Bus , CurrentTickOffset )


	end.

move( State , Path , Position , IdBus , InitialVertice , _Bus , _CurrentTickOffset ) ->

	case length( Path ) > Position of

		true ->	

			FinalVertice = list_utils:get_element_at( Path , Position + 1 ),

			DictVertices = getAttribute( State , dict ),

			Vertices = list_to_atom( lists:concat( [ InitialVertice , FinalVertice ] ) ),

			VertexPID = element( 2 , dict:find( InitialVertice , DictVertices)),	
				
			class_Actor:send_actor_message( VertexPID ,
				{ get_speed_bus, { Vertices , IdBus } }, State );

		false ->							
					
			State
			%LastPosition = list_utils:get_element_at( Bus , 4 ),

			%write_final_message( State , CurrentTickOffset , IdBus , LastPosition )

	end.

unload_people( State , IdBus , Position  ) ->

	DictPeople = getAttribute( State , people_bus_stop ), 

	Key = io_lib:format( "~s~w", [ IdBus , Position ] ), % The key is the id of the bus and the postion id of the bus stop

	case dict:is_key( Key , DictPeople ) of

		true ->

			People = element( 2 , dict:find( Key , DictPeople ) ), % element 1 is just an ok

			NewState = unload_person( State , People ),

			DictClean = dict:erase( Key , DictPeople ),

			setAttribute( NewState , people_bus_stop , DictClean );

		_ ->

			State

	end.


unload_person( State , [ ] ) ->

	State;

unload_person( State , [ Person | List ] ) ->

	NewState = class_Actor:send_actor_message( element( 1 , Person ) , 
		{ bus_go, { ok } }, State ),

	unload_person( NewState , List ).
			

-spec continue( wooper:state(), parameter(), pid() ) ->
					   class_Actor:actor_oneway_return().
continue( State , ListPeople , _StreetPID ) ->	

	People = element( 1 , ListPeople ),
	IdBus = element( 2 , ListPeople ),

	Buses = getAttribute( State , buses ), 

	Bus = element( 2 , dict:find( IdBus , Buses ) ), % dict:find returns { ok , Object }

	NewState = case People of

		nobody ->

			State;

		_ ->
				
			DictPeople = getAttribute( State , people_bus_stop ), 
			
			NewDictPeople = load_people( State , IdBus , People , DictPeople ),

			setAttribute( State , people_bus_stop , NewDictPeople )
	
	end,

	CurrentTickOffset = class_Actor:get_current_tick_offset( NewState ), 	
		
	Path = getAttribute( NewState , path ),

	Position = list_utils:get_element_at( Bus , 1 ),

	InitialVertice = list_utils:get_element_at( Path , Position ),

	move( NewState , Path , Position , IdBus , InitialVertice  , Bus , CurrentTickOffset ).


load_people( _State , _IdBus , [ ] , Dict ) ->

	Dict;

load_people( State , IdBus , [ Person | List ] , Dict ) ->

	Position = element( 1 , Person ),
	PersonPID = element( 2 , Person ),

	Key = io_lib:format( "~s~w", [ IdBus , list_to_atom( Position ) ] ), % The key is the id of the bus and the postion id of the bus stop

	NewDict = case dict:is_key( Key , Dict ) of

		true ->
			
			CurrentPeople = element( 2 , dict:find( Key , Dict ) ), % element 1 is just an ok

			dict:store( Key , CurrentPeople ++ [ { PersonPID } ] , Dict );
			
		false ->

			dict:store( Key , [ { PersonPID } ]  , Dict )		

	end,

	load_people( State , IdBus , List , NewDict ).

	

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

			%write_movement_message( State , CurrentTickOffset , BusId , LastPosition , NewPosition , BusId );

			State;
			

		true -> 

			%LinkOrigin = "1", % getAttribute( State , link_origin ), 

			%write_initial_message( State , CurrentTickOffset , BusId , LinkOrigin , NewPosition )	   

			State
   			

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

	Path = getAttribute( State , path ),

	case Path of 
	
		false ->

			executeOneway( State , declareTermination );

		_ ->

			
    			FirstActionTime = class_Actor:get_current_tick_offset( State ) + getAttribute( State, start_time ),   	

			ScheduledState = executeOneway( State , addSpontaneousTick, FirstActionTime ),

			?wooper_return_state_only( ScheduledState )
	
	end.






% Functions to write the data to the log files.

%write_final_message( State , CurrentTickOffset , BusId , LastPosition ) ->
%
%	LeavesTraffic = io_lib:format( "<event time=\"~w\" type=\"vehicle leaves traffic\" person=\"~s\" link=\"~s\" vehicle=\"~s\" relativePosition=\"1.0\" />\n", [ CurrentTickOffset , BusId , LastPosition , BusId ] ),
	
%	LeavesVehicles = io_lib:format( "<event time=\"~w\" type=\"PersonLeavesVehicle\" person=\"~s\" vehicle=\"~s\"/>\n", [ CurrentTickOffset , BusId , BusId ] ),
				
%	Arrival = io_lib:format( "<event time=\"~w\" type=\"arrival\" person=\"~s\" vehicle=\"~s\" link=\"~s\" legMode=\"car\" />\n", [ CurrentTickOffset , BusId , BusId ,  LastPosition ] ),

%	ActStart = io_lib:format( "<event time=\"~w\" type=\"actstart\" person=\"~s\"  link=\"~s\"  actType=\"h\"  />\n", [ CurrentTickOffset , BusId , LastPosition ] ),

%	TextFile = lists:concat( [ LeavesTraffic , LeavesVehicles , Arrival , ActStart ] ),

%	LogPid = ?getAttr(log_pid),
				
%	class_Actor:send_actor_message( LogPid , { receive_action, { TextFile } }, State ).

%write_initial_message( State , CurrentTickOffset , BusId , LinkOrigin , NewPosition ) ->

%	Text1 = io_lib:format( "<event time=\"~w\" type=\"actend\" person=\"~s\" link=\"~s\" actType=\"h\" />\n", [ CurrentTickOffset , BusId , LinkOrigin ] ),
%   	Text2 = io_lib:format( "<event time=\"~w\" type=\"departure\" person=\"~s\" link=\"~s\" legMode=\"car\" />\n", [ CurrentTickOffset , BusId , LinkOrigin ] ),
%  	Text3 = io_lib:format( "<event time=\"~w\" type=\"PersonEntersVehicle\" person=\"~s\" vehicle=\"~s\" />\n", [ CurrentTickOffset , BusId , BusId ] ),
%  	Text4 = io_lib:format( "<event time=\"~w\" type=\"wait2link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" />\n", [ CurrentTickOffset , BusId , LinkOrigin , BusId ] ),
  					
%	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" />\n", [  CurrentTickOffset , BusId , atom_to_list( NewPosition ) , BusId ] ),

%	TextFile = lists:concat( [ Text1 , Text2 , Text3 , Text4 , NextPositionText  ] ),

%	LogPID = ?getAttr(log_pid),
			
%	class_Actor:send_actor_message( LogPID,	{ receive_action, { TextFile } }, State ).

%write_movement_message( State , CurrentTickOffset , BusId , LastPosition , NewPosition , BusId ) ->

%	LastPositionText = io_lib:format( "<event time=\"~w\" type=\"left link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" />\n", [ CurrentTickOffset , BusId , atom_to_list(LastPosition) , BusId ] ),
%	NextPositionText = io_lib:format( "<event time=\"~w\" type=\"entered link\" person=\"~s\" link=\"~s\" vehicle=\"~s\" />\n", [  CurrentTickOffset , BusId , atom_to_list(NewPosition) , BusId ] ),

%	TextFile = lists:concat( [ LastPositionText , NextPositionText  ] ),

%	LogPID = ?getAttr(log_pid),

%	class_Actor:send_actor_message( LogPID , { receive_action, { TextFile } }, State ).


