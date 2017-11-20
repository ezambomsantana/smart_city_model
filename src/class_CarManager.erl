%Class that represents a person that can moves around the city graph on foot or by car
-module(class_CarManager).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName , CarList , CityActors ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4,
		 synchronous_new/4, synchronous_new_link/4,
		 synchronous_timed_new/4, synchronous_timed_new_link/4,
		 remote_new/5, remote_new_link/5, remote_synchronous_new/5,
		 remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
		 remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
		 construct/5, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2 ).


% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , parameter() , parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings, CarName ),

        DictCars = create_dict( dict:new() , CarList ),

	setAttributes( ActorState, [
		{ car_list, DictCars },
		{ city_actors, CityActors }
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
	ListVertexPath = element( 2 , Car ),
	ListTripsFinal = element( 3 , Car ),
	StartTime = element( 4 , Car ),
	Type = element( 5 , Car ),
	Park = element( 6 , Car ),
	Mode = element( 7 , Car ),
	CityActors = getAttribute( State , city_actors ),
	Count = element( 8 , Car ),	

	NewState = case Mode of

		"car" ->

			create_person_car( Count , State , { CarName , ListVertexPath , ListTripsFinal , StartTime , Type , Park , Mode , CityActors } );

		"walk" ->	
			create_person_car( Count , State , { CarName , ListVertexPath , ListTripsFinal , StartTime , Type , Park , Mode , CityActors } );
		_ ->

			create_person_public( Count , State , { CarName , ListVertexPath , ListTripsFinal , StartTime , Type , Mode , CityActors } )
	
	end,

	init_cars( Cars , NewState ).


create_person_car( _Count = 0 , State , _Data ) -> State;
create_person_car( Count , State , Data ) ->

	CarName = io_lib:format( "~s_~B", [ element( 1 , Data ) , Count ] ),
	ListVertexPath = element( 2 , Data ),
	ListTripsFinal = element( 3 , Data ),
	StartTime = class_RandomManager:get_uniform_value( 1200 ),
	Type = element( 5 , Data ),
	Park = element( 6 , Data ),
	Mode = element( 7 , Data ),
	CityActors = element( 8 , Data ),

	NewState = class_Actor:create_actor( class_Car,
		[ CarName , ListVertexPath , ListTripsFinal , StartTime , Type , Park , Mode , CityActors ] , State ),

	create_person_car( Count - 1 , NewState , Data ).


create_person_public( _Count = 0 , State , _Data ) -> State;
create_person_public( Count , State , Data ) ->

	CarName = io_lib:format( "~s_~B", [ element( 1 , Data ) , Count ] ),
	ListVertexPath = element( 2 , Data ),
	ListTripsFinal = element( 3 , Data ),
	StartTime = class_RandomManager:get_uniform_value( 1200 ),
	Type = element( 5 , Data ),
	Mode = element( 6 , Data ),
	CityActors = element( 7 , Data ),

	NewState = class_Actor:create_actor( class_Person,
		[ CarName , ListVertexPath , ListTripsFinal , StartTime , Type , Mode , CityActors ]  , State ),

	create_person_public( Count - 1 , NewState , Data ).

-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

    	FirstActionTime = class_Actor:get_current_tick_offset( State ) + 1,   	

	executeOneway( State , addSpontaneousTick , FirstActionTime ).

