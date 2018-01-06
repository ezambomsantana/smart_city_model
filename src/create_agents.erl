-module(create_agents).

-export([
         iterate_list/6
        ]).


% Init the XML processing

iterate_list( _ListCount , [] , _Graph , Name , MainPID , FinalList ) -> 

	class_Actor:create_initial_actor( class_CarManager,
		[ Name , FinalList ] ),

	MainPID ! { Name };

iterate_list( ListCount , [ Car | MoreCars] , Graph , Name , MainPID , FinalList ) ->

	Count = element ( 3 , Car ),

	Element = case size( Car ) == 9 of

		true ->
			create_person( element (1 , string:to_integer(Count)) , Car , Graph );

		false ->			
			create_person_multi_trip( element (1 , string:to_integer(Count)) , Car , Graph )

	end,

	iterate_list( ListCount + 1 , MoreCars , Graph , Name , MainPID , FinalList ++ Element ).


create_person( CarCount ,  Car , Graph ) ->

	Origin = element ( 1 , Car ),
	Destination = element ( 2 , Car ),
	
	ST = element( 1 , string:to_integer( element ( 4 , Car ) ) ),
	StartTime = case ST > 800 of
		true -> ST - 800 + class_RandomManager:get_uniform_value( 200 );
		false -> ST + class_RandomManager:get_uniform_value( 200 )
	end,

	LinkOrigin = element ( 5 , Car ),
	Type = element ( 6 , Car ),
	Mode = element ( 7 , Car ),
	NameFile = element ( 8 , Car ),
	Park = element ( 9 , Car ),

	ModeFinal = case Mode of
		ok ->
			"car"; % if the mode is not set in the input file, "car" is the default value.
		_ ->
			Mode % Otherwise, car or walk.
	end,

	NewPath = digraph:get_short_path( Graph , list_to_atom(Origin) , list_to_atom(Destination) ),

	ListTripsFinal = [ { ModeFinal , NewPath , LinkOrigin } ],

	% ListTripsFinal = [ { ModeFinal , NewPath } ],

	[ { StartTime , [ { NameFile , ListTripsFinal , Type , Park , ModeFinal , CarCount } ] } ].



create_person_multi_trip( CarCount ,  Car , Graph  ) ->

	io:format("teste ~w", [ element( 1 , Car ) ] ),
	ST = element( 1 , string:to_integer( element ( 1 , Car ) ) ),
	StartTime = case ST > 800 of
		true -> ST - 800 + class_RandomManager:get_uniform_value( 200 );
		false -> ST + class_RandomManager:get_uniform_value( 200 )
	end,
	Type = element ( 2 , Car ),
	ListTrips = element ( 4 , Car ),
	NameFile = element ( 5 , Car ),
	Mode = element ( 6 , Car ),
	
	ListTripsFinal = create_single_trip( ListTrips , [] , Graph ),

	[ { StartTime , [ { NameFile , ListTripsFinal , Type , ok , Mode , CarCount } ] } ].



create_single_trip( [] , ListTripsFinal , _Graph ) ->

	ListTripsFinal;

create_single_trip( [ Trip |  ListTrips ] , ListTripsFinal , Graph ) ->

	Origin = element ( 1 , Trip ),
	Destination = element ( 2 , Trip ),
	LinkOrigin = element ( 3 , Trip ),
	Mode = element ( 4 , Trip ),
	LinkDestination = element ( 5 , Trip ),
	Line = element ( 6 , Trip ),

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
