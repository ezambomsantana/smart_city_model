%Class that represents a simple City Map
-module(class_Street).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings , StreetName , ListVertex ).

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
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, getPosition/3).


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

        DictVertices = dict:from_list( ListVertex ),

	ActorState = class_Actor:construct( State, ActorSettings, StreetName ),

	setAttributes( ActorState, [
		{ street_name , StreetName },
		{ number_cars , 0 },
		{ dict , DictVertices }
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

	State.



% Called by a car wanting to know his next position.
%
% (actor oneway)
%
-spec getPosition( wooper:state(), car_index(), pid() ) ->
					   class_Actor:actor_oneway_return().
getPosition( State, Path , CarPID ) ->

	LinkId = element( 1 , Path ),
	Mode = element( 2 , Path ),

	case Mode  of

		"walk" ->

			getSpeedWalk( State , LinkId , CarPID );


		_ ->

			getSpeedCar( State , LinkId , CarPID )

	end.

getSpeedCar( State , LinkId , CarPID ) ->

	Dict = getAttribute( State, dict ),

	Element = element ( 2 , dict:find( LinkId , Dict )),
	Id = element( 1 , Element ), % Link Id
	Length = element( 2 , Element ), % Link Length	
	Capacity = element( 3 , Element ),
	Freespeed = element( 4 , Element ), 	
	NumberCars = element( 5 , Element ), 

	NewDict = dict:store(LinkId , { Id , Length , Capacity , Freespeed , NumberCars  + 1 } , Dict ),

	% Calculate car speed
	Density = (NumberCars + 1) / Length ,

	MaximumDensity = Capacity / Length ,

	MinimumDensity = (Capacity / 2) / Length ,

	Speed = case Density > MinimumDensity of

		true ->

			Freespeed * (math:pow ( 1 - math:pow((Density) / MaximumDensity , 0.05), 1) + 1);

		false ->
		
			Freespeed 

	end,

	Time = ( Length / Speed ) + 1,

	NewState = setAttribute( State , dict , NewDict ),

	class_Actor:send_actor_message( CarPID,
	 	{ go, { Id , round( Time ) , round ( Length ) } }, NewState ).


getSpeedWalk( State , LinkId , CarPID ) ->


	Dict = getAttribute( State, dict ),

	Element = element ( 2 , dict:find( LinkId , Dict )),
	Id = element( 1 , Element ), % Link Id
	Length = element( 2 , Element ), % Link Length	

	Time = ( Length / 2 ) + 1,

	class_Actor:send_actor_message( CarPID,
	 	{ go, { Id , round( Time ) , round ( Length ) } }, State ).


% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	SimulationInitialTick = ?getAttr(initial_tick),

	% Checking:
	true = ( SimulationInitialTick =/= undefined ),

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	?wooper_return_state_only( ScheduledState ).
