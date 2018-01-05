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
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, wait_bus/3, load_people/3 ).


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

	iterate_list( ListEdges ),

	ActorState = class_Actor:construct( State, ActorSettings, StreetName ),

	setAttributes( ActorState, [
		{ people_waiting , dict:new() }	] ).


iterate_list([]) -> ok;
iterate_list([ Element | List ]) ->
	
	Vertices = element( 1, Element),
	{ Id , Length , Capacity , Freespeed , Count } = element(2, Element),

	ets:insert(list_streets, {Vertices,  Id , Length , Capacity , Freespeed , Count }),

	iterate_list( List ).

% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State.

% The City is a passive actor. Never start spontanely an action
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	State.

-spec wait_bus( wooper:state(), parameter(), pid() ) ->
					   class_Actor:actor_oneway_return().
wait_bus( State , DestinationLine , PersonPID ) ->
	
	Destination = element( 1 , DestinationLine ),
	Line = element( 2 , DestinationLine ),

	PeopleWaiting = getAttribute( State, people_waiting ),

	case dict:is_key( Line , PeopleWaiting ) of

		true ->

			CurrentPeople = element( 2 , dict:find( Line , PeopleWaiting ) ), % element 1 is just an ok
			
			NewPeopleWaiting = dict:store( Line , CurrentPeople ++ [ { Destination , PersonPID } ] , PeopleWaiting ),
			
			setAttribute( State , people_waiting , NewPeopleWaiting );


		false ->

			NewPeopleWaiting = dict:store( Line , [ { Destination , PersonPID } ]  , PeopleWaiting ),
			
			setAttribute( State , people_waiting , NewPeopleWaiting )
	
	end.

-spec load_people( wooper:state(), parameter(), pid() ) ->
					   class_Actor:actor_oneway_return().
load_people( State , LineBus , BusPID ) ->

	Line = element( 1 , LineBus ),
	IdBus = element( 2 , LineBus ),

	PeopleWaiting = getAttribute( State, people_waiting ),
	
	case dict:is_key( Line , PeopleWaiting ) of

		true ->
			
			CurrentPeople = element( 2 , dict:find( Line , PeopleWaiting ) ), % element 1 is just an ok			
			
			NewPeopleWaiting = dict:erase( Line , PeopleWaiting ), % remove the current tick from the dick to save memory;

			NewState = setAttribute( State , people_waiting , NewPeopleWaiting ),			

			class_Actor:send_actor_message( BusPID,
	 			{ continue , { CurrentPeople , IdBus } }, NewState );


		false ->

			class_Actor:send_actor_message( BusPID,
	 			{ continue , { nobody , IdBus } }, State )
	
	end.

% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	?wooper_return_state_only( State ).
