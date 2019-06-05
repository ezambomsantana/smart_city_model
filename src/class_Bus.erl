%Class that represents a bus that can moves around the city graph
-module(class_Bus).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, BusName, Path , StartTime , Interval , Stops ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/6, new_link/6,
		 synchronous_new/6, synchronous_new_link/6,
		 synchronous_timed_new/6, synchronous_timed_new_link/6,
		 remote_new/7, remote_new_link/7, remote_synchronous_new/7,
		 remote_synchronous_new_link/7, remote_synchronisable_new_link/7,
		 remote_synchronous_timed_new/7, remote_synchronous_timed_new_link/7,
		 construct/7, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, go/3 , continue/4).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->


	ActorState = class_Actor:construct( State, ActorSettings, BusName ),

	DictStops = create_dict_stops( Stops , dict: new() ),

	setAttributes( ActorState, [
		{ bus_name, BusName },
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
		{ stops , DictStops },
		{ last_vertex_pid , ok }
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
			
			Interval = getAttribute( State , interval ), 

			Time1 = CurrentTickOffset > 21600,
			Time2 = CurrentTickOffset < 32400,
			NewInterval = case Time1 and Time2 of
				true -> trunc( Interval / 2 );
				false -> 
					Time3 = CurrentTickOffset > 59400,
					Time4 = CurrentTickOffset < 70800,	
					case Time3 and Time4 of
						true -> trunc( Interval / 2 );
						false -> Interval
					end
			end,

			Id = io_lib:format( "~s~B", [ getAttribute( State , bus_name ) , element( 2 , NextBus ) ] ),
			
			Buses = getAttribute( State , buses ), 
		
			NewState = setAttribute( State, next_bus , { CurrentTickOffset + NewInterval , element( 2 , NextBus ) + 1 } ),

			Bus = [ 1 , Id , CurrentTickOffset , -1 , ok ], % Current Position, Id, Start_time, Last Position

			NewDict = dict:store( Id , Bus , Buses ),

			NewDictState = setAttribute( NewState , buses , NewDict ),

			FinalState = request_position( NewDictState , Bus ),

			executeOneway( FinalState , addSpontaneousTick, CurrentTickOffset + NewInterval );	

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

	


request_position_buses( State , [] ) ->	State;
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

					People = ets:match_object(waiting_bus, { InitialVertice , BusLine , '_' , '_' }  ),
					ets:match_delete(waiting_bus, { InitialVertice , BusLine , '_' , '_' } ),

					continue( NewState , People , Bus , IdBus );

				true ->

					NewState = unload_people( State , IdBus , InitialVertice ),

					move( NewState , Path , Position , IdBus , InitialVertice  , Bus , CurrentTickOffset )

			end;
					

		false ->
			
			move( State , Path , Position , IdBus , InitialVertice  , Bus , CurrentTickOffset )


	end.

move( State , Path , Position , IdBus , InitialVertice , Bus , CurrentTickOffset ) ->

	case length( Path ) > Position of

		true ->	

			FinalVertice = list_utils:get_element_at( Path , Position + 1 ),

			Vertices = list_to_atom( lists:concat( [ InitialVertice , FinalVertice ] ) ),

			DecrementVertex = list_utils:get_element_at( Bus , 5 ),
			FinalState = case DecrementVertex of
				ok ->
					State;
				_ ->
					ets:update_counter( list_streets, DecrementVertex , { 6 , -3 }),
					State
			end,

			Buses = getAttribute( FinalState , buses ),

			NewBus = [ list_utils:get_element_at( Bus , 1 ) , list_utils:get_element_at( Bus , 2 ) , list_utils:get_element_at( Bus , 3 ), list_utils:get_element_at( Bus , 4 ) , DecrementVertex ],
	
			NewDictBuses = dict:store( IdBus , NewBus , Buses ),

			FinalBusState = setAttribute( FinalState , buses , NewDictBuses ),

			ets:update_counter( list_streets , Vertices , { 6 , 3 }),
			Data = lists:nth( 1, ets:lookup( list_streets , Vertices ) ),

			StreetData = traffic_models:get_speed_car(Data, car_following),

            go( FinalBusState , StreetData , IdBus );

		false ->							
				
			LastPosition = list_utils:get_element_at( Bus , 4 ),

			StartTime = list_utils:get_element_at( Bus , 3 ),

			DecrementVertex = list_utils:get_element_at( Bus , 5 ),
			print:write_final_message_bus( CurrentTickOffset , IdBus , LastPosition , StartTime , csv ),
			case DecrementVertex of
				ok ->
					State;
				_ ->
					ets:update_counter( list_streets, DecrementVertex , { 6 , -3 }),
					State
			end


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

unload_person( State , [ ] ) ->	State;
unload_person( State , [ Person | List ] ) ->
	NewState = class_Actor:send_actor_message( element( 1 , Person ) , { bus_go, { ok } }, State ),
	unload_person( NewState , List ).
			
continue( State , People , Bus , IdBus ) ->	

	NewState = case People of

		[] ->

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


load_people( _State , _IdBus , [ ] , Dict ) -> Dict;
load_people( State , IdBus , [ Person | List ] , Dict ) ->

	{ _ , _ , Destination , PersonPID } = Person,

	Key = io_lib:format( "~s~w", [ IdBus , list_to_atom( Destination ) ] ), %The key is the id of the bus and the postion id of the bus stop

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
go( State, PositionTime , BusId ) ->
	
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), % get the current time of the simulation

	% get the response from the city graph
	NewPosition = element( 1 , PositionTime ),
	Time = element( 2 , PositionTime),

	Buses = getAttribute( State , buses ), 

	Bus = element( 2 , dict:find( BusId , Buses ) ), % dict:find returns { ok , Object }

	LastPosition = list_utils:get_element_at( Bus , 4 ),

	case LastPosition == -1 of
		false ->
			print:write_movement_car_message( BusId , LastPosition , "bus" , CurrentTickOffset , NewPosition , csv  );
 		true -> 
			LinkOrigin = "1", % getAttribute( State , link_origin ), 
			print:write_initial_message( BusId , "bus" , CurrentTickOffset , LinkOrigin , LastPosition , csv )
	end,

	NewBus = [ lists:nth( 1 , Bus ) + 1 , lists:nth( 2 , Bus ) , lists:nth( 3 , Bus ), NewPosition , lists:nth( 5 , Bus ) ],
	
	NewDictBuses = dict:store( BusId , NewBus , Buses ),

	BusesState = setAttribute( State , buses , NewDictBuses ),

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
