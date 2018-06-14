-module(create_agents).

-export([
         iterate_list/5
        ]).

iterate_list( ListCount , Lista , Graph , Name , MainPID ) -> 
	ListaFinal = verify_list( ListCount , Lista , Graph , Name , MainPID ),
	class_Actor:create_initial_actor( class_CarManager, [ Name , ListaFinal ] ),
	MainPID ! { Name }.

verify_list( _ListCount , [ ] , _Graph , _Name , _MainPID ) -> [];
verify_list( ListCount , [ Car | MoreCars] , Graph , Name , MainPID ) ->

	Element = case size( Car ) == 10 of
		true ->
			create_person( Car , Graph );
		false ->			
			create_person_multi_trip( Car , Graph )
	end,

	[ Element | verify_list( ListCount + 1 , MoreCars , Graph , Name , MainPID ) ].

create_person( Car , Graph ) ->
	{ Origin , Destination , CarCount , ST , LinkOrigin , Type , Mode , NameFile , Park, Uuid } = Car,
        { STInteger , _ } = string:to_integer( ST ),
	StartTime = case STInteger > 800 of
		true -> STInteger - 800 + class_RandomManager:get_uniform_value( 200 );
		false -> STInteger + class_RandomManager:get_uniform_value( 200 )
	end,

	ModeFinal = case Mode of
		ok ->
			car; % if the mode is not set in the input file, "car" is the default value.
		_ ->
			list_to_atom( Mode ) % Otherwise, car or walk.
	end,

	NewPath = digraph:get_short_path( Graph , list_to_atom(Origin) , list_to_atom(Destination) ),

	ListTripsFinal = [ { ModeFinal , NewPath , LinkOrigin } ],

	{ StartTime , [ { NameFile , ListTripsFinal , Type , Park , ModeFinal , element (1 , string:to_integer(CarCount)), Uuid } ] }.

create_person_multi_trip( Car , Graph  ) ->

	{ ST , Type , CarCount , ListTrips , NameFile , Mode } = Car,
        { STInteger , _ } = string:to_integer( ST ),
	StartTime = case STInteger > 800 of
		true -> STInteger - 800 + class_RandomManager:get_uniform_value( 200 );
		false -> STInteger + class_RandomManager:get_uniform_value( 200 )
	end,
	
	ListTripsFinal = create_single_trip( ListTrips , [] , Graph ),

	{ StartTime , [ { NameFile , ListTripsFinal , Type , ok , Mode , element (1 , string:to_integer(CarCount)) } ] }.

create_single_trip( [] , ListTripsFinal , _Graph ) -> ListTripsFinal;
create_single_trip( [ Trip |  ListTrips ] , ListTripsFinal , Graph ) ->

	{ Origin , Destination , LinkOrigin , Mode , LinkDestination , Line } = Trip,
	
	case Mode of

		"metro" ->

			TripCreated = [ { Mode , Origin , LinkOrigin , Destination , LinkDestination } ],
			
			create_single_trip( ListTrips , ListTripsFinal ++  TripCreated , Graph );

		"bus" ->
		
			TripCreated = [ { Mode , Origin , Destination , Line , LinkOrigin , LinkDestination } ],

			create_single_trip( ListTrips , ListTripsFinal ++  TripCreated , Graph );

		_ -> % car and walk have the same behaviour.

			Path = digraph:get_short_path( Graph , list_to_atom(Origin) , list_to_atom(Destination) ),		

			TripCreated = case Mode of

				ok ->
				
					[ { "car" , Origin , LinkOrigin , Destination , Path , ok } ]; % if the mode is not set in the input file, "car" is the default value. Ok because doesn't have a park spot

				_ ->

					[ { Mode , Origin , LinkOrigin , Destination , Path , ok } ] % Otherwise, car or walk.

			end,
			
			create_single_trip( ListTrips ,  ListTripsFinal ++  TripCreated , Graph )
	

	end.
