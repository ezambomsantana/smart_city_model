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


% Creates a list with the parking spots in the city
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->
    
    Events = dict:from_list( ListOfEvents ),

    ActorState = class_Actor:construct( State, ActorSettings , EventsName ),

    ets:new( events, [public, set, named_table] ),

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

find_edge( Graph, V1, V2 ) ->
    EdgesList = digraph:out_edges( Graph, V1 ),
    iterate_edges( Graph, EdgesList, V2 ).

iterate_edges( _Graph, [], _V ) ->
    ok;
iterate_edges( Graph, [Edge|EdgesList], V ) ->
    { Vertex, _ } = digraph:vertex( Graph, V ),
    EdgeToCheck = digraph:edge( Graph, Edge ),

    case EdgeToCheck of
        { Edge, _, Vertex, _ } -> EdgeToCheck;
        _ -> iterate_edges( Graph, EdgesList, V )
    end.

iterate_events( State, [] ) ->
    State;
iterate_events( State, [ Event | Events ] ) ->
	NewState = case element( 1, Event ) of
				   "open_street" ->
					   EdgeID = element( 2, Event ),

					   [ { _, { Edge, V1, V2, Label } } ] = ets:lookup( events, EdgeID ),
					   [ { _ , CityGraph } ] = ets:lookup( options , city_graph ),
					   Vertex1 = digraph:vertex( CityGraph, V1 ),
					   Vertex2 = digraph:vertex( CityGraph, V2 ),
					   digraph:add_edge( CityGraph, Edge, Vertex1, Vertex2, Label ),
					   ets:insert( options, { city_graph, CityGraph } ),
					   ets:delete( events, EdgeID ),
					   State;

				   "close_street" ->
					   V1 = element( 2, Event ),
					   V2 = element( 3, Event ),
					   Duration = element( 4, Event ),

					   [ { _ , CityGraph } ] = ets:lookup( options , city_graph ),
					   EdgeToRemove = find_edge( CityGraph, V1, V2),
					   digraph:del_edge( CityGraph, EdgeToRemove ),
					   ets:insert( options, { city_graph, CityGraph } ),

					   EdgeID = list_to_atom( string:concat(atom_to_list(V1), atom_to_list(V2)) ),
					   ets:insert( events, { EdgeID , EdgeToRemove }),

					   OpenStreetEvent = { "open_street", EdgeID },
					   CurrentTickOffset = class_Actor:get_current_tick_offset( State ),
					   EventsDict = getAttribute( State, events ),
					   NewEvents = dict:append( CurrentTickOffset + Duration, OpenStreetEvent, EventsDict ),
					   setAttribute( State, events, NewEvents );

				   "restore_capacity" ->
					   EdgeID = element( 2, Event ),
					   CapacityFactor = element( 3, Event ),
					   CAPACITY_INDEX = 4,
					   Street = lists:nth( 1, ets:lookup( list_streets, EdgeID )),
					   Capacity = element( CAPACITY_INDEX, Street ),
					   RestoredCapacity = Capacity * ( 100.0 / CapacityFactor ),
					   ets:update_element( list_streets, EdgeID, { CAPACITY_INDEX, RestoredCapacity } ),
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
					   ets:update_element( list_streets, EdgeID, { CAPACITY_INDEX, ReducedCapacity } ),

					   RestoreCapacityEvent = { "restore_capacity", EdgeID, CapacityFactor },
					   CurrentTickOffset = class_Actor:get_current_tick_offset( State ),
					   EventsDict = getAttribute( State, events ),
					   NewEvents = dict:append( CurrentTickOffset + Duration, RestoreCapacityEvent, EventsDict ),
					   setAttribute( State, events, NewEvents )
			   end,
	iterate_events( NewState, Events ).
    
% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	ScheduledState = executeOneway( State , addSpontaneousTick, CurrentTickOffset + 1 ),

	?wooper_return_state_only( ScheduledState ).


