%Class that represents a simple City Map
-module(class_DigitalRails).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings , DigitalRails ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2 ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

-spec construct( wooper:state(), class_Actor:actor_settings(), parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->
	create_digital_rails(DigitalRails),
	class_Actor:construct(State, ActorSettings, "DigitalRails").

create_digital_rails([]) -> ok;

create_digital_rails([{rail, [{name, Name}, {cycle, CycleStr}, {bandwidth, BandwidthStr}], [{links, Links}]} | Rails]) ->
	create_digital_rails_links(Name, list_to_integer(CycleStr), list_to_integer(BandwidthStr), Links),
	create_digital_rails(Rails);

create_digital_rails([_ | Rails]) ->
	create_digital_rails(Rails).

create_digital_rails_links(_, _, _, []) -> ok;
create_digital_rails_links(Name, Cycle, Bandwidth, [{link, [{origin, Origin}, {destination, Destination}], _} | Links]) ->
	Edge = list_to_atom(lists:concat([Origin, Destination])),
	ets:update_element(list_streets, Edge, {8 , {Name, 1, Cycle, Bandwidth, false, 0}}),
	create_digital_rails_links(Name, Cycle, Bandwidth, Links);

create_digital_rails_links(Name, Cycle, Bandwidth, [{link, [{origin, Origin}, {destination, Destination}, {signalized, Signalized}, {offset, Offset}], _} | Links]) ->
	Edge = list_to_atom(lists:concat([Origin, Destination])),
	ets:update_element(list_streets, Edge, {8 , {Name, 1, Cycle, Bandwidth, Signalized, Offset}}),
	create_digital_rails_links(Name, Cycle, Bandwidth, Links);

create_digital_rails_links(Name, Cycle, Bandwidth, [_ | Links]) ->
	create_digital_rails_links(Name, Cycle, Bandwidth, Links).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) -> State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) -> State.

-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) -> ?wooper_return_state_only( State ).