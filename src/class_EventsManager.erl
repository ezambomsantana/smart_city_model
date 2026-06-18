%Class that manage the parking spots in the city
-module(class_EventsManager).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings , EventsName , ListOfEvents ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2 ).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Creates a list with the road closure events in the city
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->
    
	% Create a dictionary with the events, does not overwrite events with the same time
	% Key: Time, Value: List of events
    Events = lists:foldl(
        fun({Time, Event}, AccDict) ->
            dict:append(Time, Event, AccDict)
        end,
        dict:new(),
        ListOfEvents
    ),

	ActorState = class_Actor:construct( State, ActorSettings , EventsName ),

	case ets:info(events) of
		undefined -> ets:new(events, [public, set, named_table]);
		_ -> ok
	end,

	case ets:info(traffic_factors) of
		undefined -> 
			ets:new(traffic_factors, [public, set, named_table]),
			ets:insert(traffic_factors, { capacity_reduction, 1.0 }),
			ets:insert(traffic_factors, { speed_reduction, 1.0 });
		_ -> ok
	end,

	setAttributes( ActorState, [ { events , Events } ] ).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

    State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	Events = getAttribute( State, events ),
	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

	NewState = case dict:find(  CurrentTickOffset, Events ) of
			   {ok, EventsList} -> iterate_events( State, EventsList );
			   error -> State
		   end,

	executeOneway( NewState , addSpontaneousTick, CurrentTickOffset + 1 ).


iterate_events( State, [] ) ->
	State;
iterate_events( State, [ Event | Events ] ) ->
	NewState = case element( 1, Event ) of
			   "open_street" ->
				%    io:format("OPEN STREET~n"),

				   V1 = element( 2, Event ),
				   V2 = element( 3, Event ),

				   [ { _, GraphManagerPid } ] = ets:lookup( graph, mypid ),

				   GraphManagerPid ! { add_edge, V1, V2 },

				   EdgeID = list_to_atom( string:concat(atom_to_list(V1), atom_to_list(V2)) ),
				   ets:delete( events, EdgeID ),

				   State;

			   "close_street" ->
				%    io:format("CLOSE STREET~n"),

				   V1 = element( 2, Event ),
				   V2 = element( 3, Event ),
				   Duration = element( 4, Event ),

				   OpenStreetEvent = { "open_street", V1, V2 },
				   CurrentTickOffset = class_Actor:get_current_tick_offset( State ),
				   OpeningTick = CurrentTickOffset + Duration,
				   
				   EdgeID = list_to_atom( string:concat(atom_to_list(V1), atom_to_list(V2)) ),
				   case ets:lookup( events, EdgeID ) of
					   [{ _, remove, OldOpeningTick }] ->
						   % if the street is already closed
						   EventsDict = getAttribute( State, events ),

						   % update the open streetevent with the new opening tick
						   NewEventsDict = dict:update(
							   OldOpeningTick,
							   fun(EventsList) ->
								   lists:filter(fun(E) -> E =/= OpenStreetEvent end, EventsList)
							   end,
							   EventsDict
						   ),

						   NewEvents = dict:append( OpeningTick, OpenStreetEvent, NewEventsDict ),
						   setAttribute( State, events, NewEvents );

					   _ ->
						   % if the street is not closed
						   [ { _, GraphManagerPid } ] = ets:lookup( graph, mypid ),
						   GraphManagerPid ! { delete_edge, V1, V2 },
						   ets:insert( events, { EdgeID , remove, OpeningTick }),
						   EventsDict = getAttribute( State, events ),
						   NewEvents = dict:append( OpeningTick, OpenStreetEvent, EventsDict ),
						   setAttribute( State, events, NewEvents )
				   end;

			   "restore_capacity" ->
				   EdgeID = element( 2, Event ),
				   CapacityFactor = element( 3 , Event ),
				   CAPACITY_INDEX = 4,
				   Street = lists:nth( 1, ets:lookup( list_streets, EdgeID )),
				   Capacity = element( CAPACITY_INDEX, Street ),
				   RestoredCapacity = Capacity * ( 100.0 / CapacityFactor ),
				   ets:update_element( list_streets, EdgeID, [ { CAPACITY_INDEX, RestoredCapacity } ] ),
				   State;

			   "reduce_capacity" ->
				   V1 = atom_to_list( element( 2, Event ) ),
				   V2 = atom_to_list( element( 3, Event ) ),
				   EdgeID = list_to_atom( string:concat(V1, V2) ),
				   Duration = element( 4, Event ),
				   CapacityFactor = element( 5, Event ),
				   CAPACITY_INDEX = 4,

				   Street = lists:nth( 1, ets:lookup( list_streets, EdgeID )),
				   Capacity = element( CAPACITY_INDEX, Street ),
				   ReducedCapacity = Capacity * ( CapacityFactor / 100.0 ),
				   ets:update_element( list_streets, EdgeID, [ { CAPACITY_INDEX, ReducedCapacity } ] ),


				   RestoreCapacityEvent = { "restore_capacity", EdgeID, CapacityFactor },
				   CurrentTickOffset = class_Actor:get_current_tick_offset( State ),
				   EventsDict = getAttribute( State, events ),
				   NewEvents = dict:append( CurrentTickOffset + Duration, RestoreCapacityEvent, EventsDict ),
				   setAttribute( State, events, NewEvents );
				
				"reduce_traffic" ->
					io:format("REDUCE TRAFFIC~n"),
					% Reduce capacity all streets by a certain percentage and reduce all vehicles velocity by a certain percentage
					CapacityFactor = element( 2, Event ),
					VelocityFactor = element( 3, Event ),
					
					ets:insert(traffic_factors, { capacity_reduction, CapacityFactor / 100.0 }),
					ets:insert(traffic_factors, { speed_reduction, VelocityFactor / 100.0 }),
					State

				% "restore_traffic" ->
				% 	io:format("RESTORE TRAFFIC~n"),
				% 	% Restore capacity all streets to the original value and restore all vehicles velocity to the original value
					
				% 	ets:insert(traffic_factors, { capacity_reduction, 1.0 }),
				% 	ets:insert(traffic_factors, { speed_reduction, 1.0 }),

				% 	State

		   end,
	iterate_events( NewState, Events ).
    
% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	executeOneway( State , addSpontaneousTick, CurrentTickOffset + 1 ).

	%?wooper_return_state_only( ScheduledState ).


