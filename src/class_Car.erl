%Class that represents a person that can moves around the city graph on foot or by car
-module(class_Car).

-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName , ListTripsFinal , StartTime , Type , Park , Mode, DigitalRailsCapable ).

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
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, get_parking_spot/3 , set_new_path/3, receive_signal_state/3 ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter() , parameter() , parameter(), parameter() ) -> wooper:state().
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
		{ last_vertex , ok },
		{ last_vertex_pid , ok },
		{ previous_dr_name, nil },
		{ digital_rails_capable, DigitalRailsCapable}] 
	),

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
	Path = getAttribute( State , path ), 
	verify_next_action( State , Trips , Path ).

verify_next_action( State , _Trip , Path ) when Path == false ->
	executeOneway( State , declareTermination );

verify_next_action( State , Trips , Path ) when length( Trips ) == 0, Path == finish -> 
	executeOneway( State , declareTermination );

verify_next_action( State , Trips , Path ) when length( Trips ) > 0 ->
	CurrentTrip = lists:nth( 1 , Trips ),		
	?wooper_return_state_only( request_position( State , CurrentTrip , Path ) );

verify_next_action( State , _Trips , _Path ) ->
	Type = getAttribute( State , type ),						
	TotalLength = getAttribute( State , distance ),
	StartTime = getAttribute( State , start_time ),
	CarId = getAttribute( State , car_name ),	
	LastPosition = getAttribute( State , car_position ),
	Mode = getAttribute( State , mode ), 

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	print:write_final_message( Type , TotalLength , StartTime , CarId , CurrentTickOffset , LastPosition , Mode , xml ),
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

is_changing_dr(State, {Name, _DRLanes, _Cycle, _Bandwidth, _, _}) ->
	case getAttribute(State, digital_rails_capable) of 
		true -> 
			case getAttribute(State, previous_dr_name) of
				Name -> false;
				_ -> true
			end;
		_ -> false
	end;

is_changing_dr(_, _) -> false.

get_next_vertex( State , [ Current | Path ] , Mode ) when Mode == walk ->			
	Vertices = list_to_atom( lists:concat( [ Current , lists:nth( 1 , Path ) ] )),
	
	Data = lists:nth( 1, ets:lookup( list_streets , Vertices ) ),
	{ Id , Time , Distance } = traffic_models:get_speed_walk(Data, getAttribute(State, traffic_model)),

	TotalLength = getAttribute( State , distance ) + Distance,
	FinalState = setAttributes( State , [ { distance , TotalLength } , { car_position , Id } , { path , Path } ] ), 

	% print_movement( State ),

	executeOneway( FinalState , addSpontaneousTick , class_Actor:get_current_tick_offset( FinalState ) + Time );

get_next_vertex( State, [ _CurrentVertex | _ ], _Mode) -> 
	% LastVertex = getAttribute(State, last_vertex),
	[CurrentVertex | [ NextVertex | _ ]] = getAttribute(State, path),
	% [ CurrentVertex | _ ] = getAttribute( State , path ),
	Edge = list_to_atom(lists:concat([CurrentVertex, NextVertex])),

	LinkData = lists:nth(1, ets:lookup(list_streets, Edge)),
	{_, _, _, _, _, _, _Lanes, DigitalRailsInfo} = LinkData,

	ChangingDR = is_changing_dr(State, DigitalRailsInfo),
	case ChangingDR of 
		true ->
			{Name, _DRLanes, Cycle, Bandwidth, _, _} = DigitalRailsInfo,
			T = round(rand:uniform() * Cycle),
			StateAfter = setAttributes(State, [{previous_dr_name, Name}]),
		 	case T > Bandwidth of
				true -> executeOneway(StateAfter, addSpontaneousTick, class_Actor:get_current_tick_offset(StateAfter) + T);
				_ -> move_to_next_vertex(StateAfter)
			end;
		false -> move_to_next_vertex(State)
	end.
	
	% Current vertex is an atom here, but at the ets it is a string. Must convert:
	% CurrentVertexStr = lists:flatten(io_lib:format("~s", [CurrentVertex])),
	% Matches = ets:lookup(traffic_signals, CurrentVertexStr),

	% case length(Matches) of
	% 	0 -> move_to_next_vertex(State);
	% 	_ -> 	
	% 		{_, TrafficSignalsPid} = lists:nth(1, Matches),
	% 		class_Actor:send_actor_message(TrafficSignalsPid, {querySignalState, LastVertex}, State)
	% end.
	% move_to_next_vertex(State).

move_to_next_vertex( State ) ->
	[ CurrentVertex | [ NextVertex | Path ] ] = getAttribute( State , path ),
	Edge = list_to_atom(lists:concat([ CurrentVertex , NextVertex ])),
	
	case getAttribute(State, digital_rails_capable) of
		true -> ok;
		_ -> 
			DecrementVertex = getAttribute( State , last_vertex_pid ),
			case DecrementVertex of
				ok -> ok;
				_ -> ets:update_counter( list_streets, DecrementVertex , { 6 , -1 })
			end,	
			ets:update_counter( list_streets , Edge , { 6 , 1 })
	end,
	
	Data = lists:nth(1, ets:lookup(list_streets , Edge)),

	{ Id , Time , Distance } = traffic_models:get_speed_car(Data, getAttribute(State, digital_rails_capable)),

	TotalLength = getAttribute( State , distance ) + Distance,
	StateAfterMovement = setAttributes( State , [
		{distance , TotalLength} , {car_position , Id} , {last_vertex, CurrentVertex}, {last_vertex_pid , Edge} , {path , [NextVertex | Path]}] ), 

	% io:format("t=~p: ~p; ~p->~p ~n", [class_Actor:get_current_tick_offset(State), getAttribute(State, car_name), CurrentVertex, NextVertex]),
	% io:format("~p Tick: ~p; ~p => ~p, Dist: ~p, Time: ~p, Avg. Speed: ~p, NextTick: ~p\n", 
	% 	[getAttribute( State , car_name ), class_Actor:get_current_tick_offset( State ), CurrentVertex, NextVertex, Distance, Time, Distance / Time, class_Actor:get_current_tick_offset( StateAfterMovement ) + Time]),

	% print_movement(State, StateAfterMovement),
	executeOneway( StateAfterMovement , addSpontaneousTick , class_Actor:get_current_tick_offset( StateAfterMovement ) + Time ).

-spec receive_signal_state(wooper:state(), tuple(), pid()) -> oneway_return().
receive_signal_state( State , {Color, TicksUntilNextColor}, _TrafficLightPid ) -> 
	case Color of
		red -> 
			% io:format("[~p] red (green in ~p)\n", [TrafficLightPid, TicksUntilNextColor]),
			% Act spontaneously when the traffic light is green again...
			executeOneway( State , addSpontaneousTick , class_Actor:get_current_tick_offset( State ) + TicksUntilNextColor );
		green -> 
			% io:format("Traffic signal is green, continuing movement...\n"),
			move_to_next_vertex(State)
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
	% TODO: Why this is needed?
	StartTime = getAttribute( State , start_time ),
    	FirstActionTime = class_Actor:get_current_tick_offset( State ) + StartTime,   	
	NewState = setAttribute( State , start_time , FirstActionTime ),
	executeOneway( NewState , addSpontaneousTick , FirstActionTime ).

% print_movement( PreviousState, NextState ) ->
% 	CarId = getAttribute( PreviousState, car_name),
% 	LastPosition = getAttribute( PreviousState , car_position ),
% 	Type = getAttribute( PreviousState , type ),
% 	CurrentTickOffset = class_Actor:get_current_tick_offset( NextState ),
% 	NewPosition = getAttribute( NextState, car_position),

% 	case LastPosition == -1 of
% 		false ->
% 			print:write_movement_car_message( CarId, LastPosition, Type, CurrentTickOffset, NewPosition, xml );
% 		true -> 
% 			CurrentTrip = lists:nth( 1 , getAttribute( PreviousState , trips ) ),
% 			TripVertices = element(2, CurrentTrip),
% 			EdgeId = list_to_atom(lists:concat([lists:nth(1, TripVertices), lists:nth(2, TripVertices)])),
% 			Edge = lists:nth(1, ets:lookup(list_streets , EdgeId)),
% 			LinkOrigin = element(2, Edge),
% 			print:write_initial_message( CarId, Type, CurrentTickOffset, LinkOrigin, NewPosition, xml )
% 	end.