%Class that represents a Metro Graph
-module(class_Metro).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CityName , MetroFile ).

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
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, getTravelTime/3).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(TraceEmitterCategorization,"Smart-City.City").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").

% Creates a new city graph
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , sensor_type() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, CityName ),

	MetroGraph = metro_parser:show( MetroFile , false ),

	print_graph( MetroGraph ),

	setAttributes( ActorState, [
		{ city_name, CityName },
		{ graph, MetroGraph },
		{ probe_pid, non_wanted_probe },	
		{ trace_categorization,
		 text_utils:string_to_binary( ?TraceEmitterCategorization ) }
							] ).
% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Destructor don't do nothing in this class.
	State.

% The City is a passive actor. Never start spontanely an action
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	?wooper_return_state_only( State ).


% Called by a car wanting to know his next position.
%
% (actor oneway)
%
-spec getTravelTime( wooper:state(), car_index(), pid() ) ->
					   class_Actor:actor_oneway_return().
getTravelTime( State, Path , PersonPID ) ->

	{ InitialVertice , FinalVertice } = { element(1 , Path) , element(2 , Path) },

	Graph = getAttribute( State , graph ),

	PathMetro = digraph:get_short_path( Graph , list_to_atom( InitialVertice ) , list_to_atom( FinalVertice ) ),


	io:format("vInicio: ~w~n", [ list_to_atom( InitialVertice ) ]),
	io:format("vInicio: ~w~n", [ list_to_atom( FinalVertice ) ]),
	io:format("vInicio: ~w~n", [ PathMetro ]),

	NumberStations = length( PathMetro ),

	class_Actor:send_actor_message( PersonPID,
		{ metro_go, { NumberStations * 3 * 60 } }, State ).


% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	SimulationInitialTick = ?getAttr(initial_tick),

	% Checking:
	true = ( SimulationInitialTick =/= undefined ),

	case ?getAttr(probe_pid) of

		non_wanted_probe ->
			ok;

		ProbePid ->
			ProbePid ! { setTickOffset, SimulationInitialTick }

	end,

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	?wooper_return_state_only( ScheduledState ).



print_graph( Graph ) ->
	
	Vertices = digraph:vertices( Graph ),
	Edges = digraph:edges( Graph ),
	print_vertices( Vertices  ),
	print_edges( Graph , Edges ).



print_vertices([]) ->
	ok;

print_vertices([Element | MoreElements]) ->
	io:format("vertice: ~s~n", [ Element ]),
	print_vertices(MoreElements).



print_edges(_Graph , []) ->
	ok;

print_edges(Graph, [Element | MoreElements]) ->
	Edge = digraph:edge(Graph, Element),
	io:format("vInicio: ~s~n", [ element( 2 , Edge ) ]),
	io:format("vFim: ~s~n", [ element( 3 , Edge) ]),
	io:format("id: ~s~n", [ element( 4 , Edge) ]),
	print_edges( Graph , MoreElements ).
