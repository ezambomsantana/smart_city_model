%Class that represents a Metro Graph
-module(class_City).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CityName , Graph ).

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
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, get_path/3).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

-include_lib("../deps/amqp_client/include/amqp_client.hrl").

% Creates a new metro graph actor
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , sensor_type() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

        case ets:info(options) of
	    undefined -> ets:new(options, [public, set, named_table]);
            _ -> ok
        end,
        
	ActorState = class_Actor:construct( State, ActorSettings, CityName ),
	CityGraph = map_parser:show( element( 1 , Graph ) , false ),

	ets:insert(options, { city_pid , self() }),
	ets:insert(options, { city_graph , CityGraph }),
	
	Hostname = os:getenv("RABBITMQ_HOST", "localhost"),
	{ok, Connection} = amqp_connection:start(#amqp_params_network{host=Hostname}),
	{ ok, Channel } = amqp_connection:open_channel( Connection ),

	ets:insert(options, { rabbitmq_channel, Channel }),

	setAttributes( ActorState, [ { graph , CityGraph } ] ).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State.

% The City is a passive actor. Never start spontanely an action
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	?wooper_return_state_only( State ).


% Called by an agent wanting to know how much time it will spend at the metro
-spec get_path( wooper:state(), car_index(), pid() ) ->
					   class_Actor:actor_oneway_return().
get_path( State, Data , PersonPID ) ->

	{ InitialVertice , FinalVertice } = { element(1 , Data) , element(2 , Data) },

	Graph = getAttribute( State , graph ),

	Path = digraph:get_best_path( Graph , InitialVertice , list_to_atom( FinalVertice ) ),

	class_Actor:send_actor_message( PersonPID,
		{ set_new_path , { Path } }, State ).

% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?wooper_return_state_only( State ).
