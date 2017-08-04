-module(create_agents).



% usage:
%
% l(osm_parser).
% osm_parser:show("map.osm").

-export([
         iterate_list/8,
	 get_path_nodes/3
        ]).


% Init the XML processing

iterate_list( _ListCount, _ListVertex , [] , _Graph , _LogPID , Name , _MetroActor , MainPID ) ->
	MainPID ! { Name };

iterate_list( ListCount, ListVertex , [ Car | MoreCars] , Graph , LogPID , Name , MetroActor , MainPID ) ->

	Count = element ( 3 , Car ),

	case size( Car ) == 8 of

		true ->
			create_person( ListCount , element (1 , string:to_integer(Count)) , ListVertex , Car , Graph , false , LogPID , MetroActor );

		false ->			
			create_person_multi_trip( ListCount , element (1 , string:to_integer(Count)) , ListVertex , Car , Graph , LogPID , MetroActor )

	end,
			

	iterate_list( ListCount + 1, ListVertex , MoreCars , Graph , LogPID , Name , MetroActor , MainPID ).



create_person( _ListCount , _CarCount = 0 , _ListVertex ,  _Car , _Graph , _Path , _LogPID , _MetroActor ) ->
	
	ok;


create_person( ListCount , CarCount , ListVertex ,  Car , Graph , Path , LogPID , MetroActor ) ->

	Origin = element ( 1 , Car ),
	Destination = element ( 2 , Car ),
	StartTime = element ( 4 , Car ),
	LinkOrigin = element ( 5 , Car ),
	Type = element ( 6 , Car ),
	Mode = element ( 7 , Car ),
	NameFile = element ( 8 , Car ),

	CarName = io_lib:format( "~s_~B",
		[ NameFile , CarCount ] ),

	case Path of

		false ->

			NewPath = digraph:get_short_path( Graph , list_to_atom(Origin) , list_to_atom(Destination) ),

			ListVertexPath = get_path_nodes( NewPath , ListVertex , [] ),

			ListTripsFinal = [ { Mode , Origin , LinkOrigin , Destination , NewPath } ],

			class_Actor:create_initial_actor( class_Person,
				[ CarName , ListVertexPath , ListTripsFinal , element( 1 , string:to_integer( StartTime )) , LogPID , Type , MetroActor ] ),

			create_person( ListCount , CarCount - 1 , ListVertex ,  Car , Graph , NewPath , LogPID , MetroActor );

		_ ->

			ListVertexPath = get_path_nodes( Path , ListVertex , [] ),

			ListTripsFinal = [ { Mode , Origin , LinkOrigin , Destination , Path } ],

			class_Actor:create_initial_actor( class_Person,
				[ CarName , ListVertexPath , ListTripsFinal , element( 1 , string:to_integer( StartTime )) , LogPID , Type , MetroActor ] ),

			create_person( ListCount , CarCount - 1 , ListVertex ,  Car , Graph , Path , LogPID , MetroActor  )

	end.

create_person_multi_trip( _ListCount , _CarCount = 0 , _ListVertex ,  _Car , _Graph , _LogPID , _MetroActor ) ->
	
	ok;

create_person_multi_trip( _ListCount , CarCount , ListVertex ,  Car , Graph , LogPID  , MetroActor ) ->

	StartTime = element ( 1 , Car ),
	Type = element ( 2 , Car ),
	ListTrips = element ( 4 , Car ),
	NameFile = element ( 5 , Car ),

	CarName = io_lib:format( "~s_~B",
		[ NameFile , CarCount ] ),
	
	{ ListTripsFinal , ListVertexPath } = create_single_trip( ListTrips , [] , Graph , [] , ListVertex ),

	class_Actor:create_initial_actor( class_Person,
		[ CarName , ListVertexPath , ListTripsFinal , element( 1 , string:to_integer( StartTime )) , LogPID , Type , MetroActor ] ).

create_single_trip( [] , ListTripsFinal , _Graph , ListVertexPath , _ListVertex ) ->

	{ ListTripsFinal , ListVertexPath };

create_single_trip( [ Trip |  ListTrips ] , ListTripsFinal , Graph , ListVertexPath , ListVertex ) ->

	Origin = element ( 1 , Trip ),
	Destination = element ( 2 , Trip ),
	LinkOrigin = element ( 3 , Trip ),
	Mode = element ( 4 , Trip ),
	LinkDestination = element ( 5 , Trip ),
	Line = element ( 6 , Trip ),

	case Mode of

		"metro" ->

			TripCreated = [ { Mode , Origin , LinkOrigin , Destination , LinkDestination } ],
			
			create_single_trip( ListTrips , ListTripsFinal ++  TripCreated , Graph , ListVertexPath , ListVertex );

		"bus" ->
		
			TripCreated = [ { Mode , Origin , Destination , Line } ],
			
			create_single_trip( ListTrips , ListTripsFinal ++  TripCreated , Graph , ListVertexPath , ListVertex );

		_ -> % car and walk have the same behavior.

			Path = digraph:get_short_path( Graph , list_to_atom(Origin) , list_to_atom(Destination) ),

			TripCreated = [ { Mode , Origin , LinkOrigin , Destination , Path } ],

			NewListVertexPath = ListVertexPath ++ get_path_nodes( Path , ListVertex , [] ),
			
			create_single_trip( ListTrips ,  ListTripsFinal ++  TripCreated , Graph , NewListVertexPath , ListVertex )
	

	end.

get_path_nodes( [] , _ListVertex , List ) ->
	
	List;

get_path_nodes( [ Node | MoreNodes] , ListVertex , List ) ->

	Element = dict:find( Node , ListVertex ),

	ElementList = [{ Node , element( 2 , Element) }],

	get_path_nodes( MoreNodes , ListVertex , List ++ ElementList ).	
