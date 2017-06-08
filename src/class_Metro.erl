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



% Creates a new metro graph actor
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , sensor_type() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, CityName ),

	MetroGraph = metro_parser:show( MetroFile , false ),

	setAttributes( ActorState, [
		{ city_name, CityName },
		{ graph, MetroGraph } ] ).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State.

% The City is a passive actor. Never start spontanely an action
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	?wooper_return_state_only( State ).


% Called by an agent wanting to know how much time it will spend at the metro
-spec getTravelTime( wooper:state(), car_index(), pid() ) ->
					   class_Actor:actor_oneway_return().
getTravelTime( State, Path , PersonPID ) ->

	{ InitialVertice , FinalVertice } = { element(1 , Path) , element(2 , Path) },

	Graph = getAttribute( State , graph ),

	PathMetro = digraph:get_short_path( Graph , list_to_atom( InitialVertice ) , list_to_atom( FinalVertice ) ),

	NumberStations = length( PathMetro ),

	class_Actor:send_actor_message( PersonPID,
		{ metro_go, { NumberStations * 3 * 60 } }, State ).


% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?wooper_return_state_only( State ).

