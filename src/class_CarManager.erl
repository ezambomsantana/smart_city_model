%Class that represents a person that can moves around the city graph on foot or by car
-module(class_CarManager).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName , CarList ).

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

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, CarName ),

        DictCars = create_dict( dict:new() , CarList ),

	setAttributes( ActorState, [
		{ car_list, DictCars }
						] ).

create_dict( Dict , [] ) ->
	Dict;

create_dict( Dict , [ Car | CarList ] ) ->

	Key = element( 1 , Car ),
	Value = element( 2 , Car ),

	Element = dict:find( Key , Dict ),
	
	NewDict= case Element of 
		error -> dict:store( Key , Value , Dict );
		_ -> dict:store( Key , element( 2 , Element ) ++ Value , Dict )
	end,
	create_dict( NewDict , CarList ).


-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	CurrentTick = class_Actor:get_current_tick_offset( State ),

	DictCars = getAttribute( State , car_list ),
	Cars = dict:find( CurrentTick , DictCars ),

	NewState = case Cars of
		error -> State;
		_ -> init_cars( element( 2 , Cars ) , State )
	end,

    	FirstActionTime = CurrentTick + 1,   	

	executeOneway( NewState , addSpontaneousTick , FirstActionTime ).
	
init_cars( [] , State ) ->
	State;

init_cars( [ Car | Cars ] , State ) ->

	CarName = element( 1 , Car ),
	ListTripsFinal = element( 2 , Car ),
	StartTime = element( 3 , Car ),
	Type = element( 4 , Car ),
	Park = element( 5 , Car ),
	Mode = element( 6 , Car ),
	Count = element( 7 , Car ),	

	NewState = case Mode of

		"car" ->

			create_person_car( Count , State , { CarName , ListTripsFinal , StartTime , Type , Park , Mode } );

		"walk" ->	
			create_person_car( Count , State , { CarName , ListTripsFinal , StartTime , Type , Park , Mode } );
		_ ->

			create_person_public( Count , State , { CarName , ListTripsFinal , StartTime , Type , Mode } )
	
	end,

	init_cars( Cars , NewState ).


create_person_car( _Count = 0 , State , _Data ) -> State;
create_person_car( Count , State , Data ) ->

	CarName = io_lib:format( "~s_~B", [ element( 1 , Data ) , Count ] ),
	ListTripsFinal = element( 2 , Data ),
	StartTime = class_RandomManager:get_uniform_value( 1200 ),
	Type = element( 4 , Data ),
	Park = element( 5 , Data ),
	Mode = element( 6 , Data ),

	NewState = class_Actor:create_actor( class_Car,
		[ CarName , ListTripsFinal , StartTime , Type , Park , Mode ] , State ),

	create_person_car( Count - 1 , NewState , Data ).


create_person_public( _Count = 0 , State , _Data ) -> State;
create_person_public( Count , State , Data ) ->

	CarName = io_lib:format( "~s_~B", [ element( 1 , Data ) , Count ] ),
	ListTripsFinal = element( 2 , Data ),
	StartTime = class_RandomManager:get_uniform_value( 1200 ),
	Type = element( 4 , Data ),
	Mode = element( 5 , Data ),

	NewState = class_Actor:create_actor( class_Person,
		[ CarName , ListTripsFinal , StartTime , Type , Mode ]  , State ),

	create_person_public( Count - 1 , NewState , Data ).

-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

    	FirstActionTime = class_Actor:get_current_tick_offset( State ) + 1,   	

	executeOneway( State , addSpontaneousTick , FirstActionTime ).

