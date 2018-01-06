%Class that represents a simple City Map
-module(class_Street).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings , StreetName , ListEdges ).

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


% Creates a new city graph
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , sensor_type() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	case ets:info(list_streets) of
		undefined -> ets:new(list_streets, [public, set, named_table]);
                _ -> ok
        end,

	case ets:info(waiting_bus) of
		undefined -> ets:new(waiting_bus, [public, bag, named_table]);
                _ -> ok
        end,

	iterate_list( ListEdges ),

	class_Actor:construct( State, ActorSettings, StreetName ).


iterate_list([]) -> ok;
iterate_list([ Element | List ]) ->
	
	Vertices = element( 1, Element),
	{ Id , Length , Capacity , Freespeed , Count } = element(2, Element),

	ets:insert(list_streets, {Vertices,  Id , Length , Capacity , Freespeed , Count }),

	iterate_list( List ).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	State.

-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?wooper_return_state_only( State ).
