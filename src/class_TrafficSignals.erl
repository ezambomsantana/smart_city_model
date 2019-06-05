-module(class_TrafficSignals).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, ActorName, Signal ).

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
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, querySignalState/3 ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

-spec construct( wooper:state(), class_Actor:actor_settings(), class_Actor:name() , parameter() ) 
	-> wooper:state().
construct( State, ?wooper_construct_parameters ) ->
	{signal, [{cycle_duration, CycleDurationStr}, {offset, OffsetStr}], [{nodes, Nodes}, {phases, Phases}]} = Signal,
	{CycleDuration, _} = string:to_integer(CycleDurationStr),
	{Offset, _} = string:to_integer(OffsetStr),

	case ets:info(traffic_signals) of
		undefined -> ets:new(traffic_signals, [public, set, named_table]);
		_ -> ok
	end,

	lists:foreach(fun(Node) -> 
		{node, [{id, NodeId}], _} = Node,
		ets:insert( traffic_signals , {NodeId, self()} )
	end, Nodes),

	PhaseMap = buildPhaseMap(Phases),
	
	ActorState = class_Actor:construct( State , ActorSettings , ActorName ),
	setAttributes( ActorState , [{name, ActorName}, {cycle_duration, CycleDuration}, {offset, Offset}, {phase_map, PhaseMap}] ).

% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) -> State.


% PhaseMap: Tuples with {GreenStart, GreenDuration} indexed by origin vertex id
buildPhaseMap(Phases) ->
	AccumulatePhase = fun(Phase, AccPhaseMap) ->
		{phase, [{origin, OriginId}, {green_duration, GreenDurationStr}, {green_start, GreenStartStr}], _} = Phase,
		{GreenStart, _} = string:to_integer(GreenStartStr),
		{GreenDuration, _} = string:to_integer(GreenDurationStr),

		maps:merge(AccPhaseMap, maps:put(OriginId, {GreenStart, GreenDuration}, AccPhaseMap))
	end,

	lists:foldl(AccumulatePhase, maps:new(), Phases).

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->
  CurrentTick = class_Actor:get_current_tick_offset( State ),
  executeOneway( State , addSpontaneousTick, CurrentTick + 600 ).


-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 
	ScheduledState = executeOneway( State , addSpontaneousTick, CurrentTickOffset + 100 ),

	?wooper_return_state_only( ScheduledState ).
	
ticksUntilNextGreen(CycleDuration, TickInCycle, GreenStart, GreenDuration) 
	when TickInCycle >= GreenStart + GreenDuration -> (CycleDuration - TickInCycle) + GreenStart;

ticksUntilNextGreen(_, TickInCycle, GreenStart, _) 
	when TickInCycle < GreenStart -> GreenStart - TickInCycle;

ticksUntilNextGreen(_, _, _, _) -> 0.

-spec querySignalState( wooper:state(), parameter(), pid() ) -> class_Actor:actor_oneway_return().
querySignalState( State , OriginId , PersonPID ) ->
	Offset = getAttribute(State, offset),
	CycleDuration = getAttribute(State, cycle_duration),	

	CurrentTick = class_Actor:get_current_tick_offset( State ) - Offset,
	OriginStr = lists:flatten(io_lib:format("~s", [OriginId])),

	PhaseMap = getAttribute( State, phase_map ),
	{GreenStart, GreenDuration} = maps:get(OriginStr, PhaseMap),

	TickInCycle = CurrentTick rem (CycleDuration),

	CurrentLightState = if 
		TickInCycle >= GreenStart andalso TickInCycle < GreenStart + GreenDuration -> {green, 0};
		true -> {red, ticksUntilNextGreen(CycleDuration, TickInCycle, GreenStart, GreenDuration)}
	end,

  class_Actor:send_actor_message( PersonPID, { receive_signal_state, CurrentLightState }, State ).