-module(create_agents).

-export([iterate_list/6]).

iterate_list( ListCount , Lista , Graph , Name , MainPID, DigitalRails ) -> 
	ListaFinal = verify_list( ListCount , Lista , Graph , Name , MainPID, DigitalRails ),
	class_Actor:create_initial_actor( class_CarManager, [ Name , ListaFinal ] ),
	MainPID ! { Name }.

verify_list( _ListCount , [ ] , _Graph , _Name , _MainPID, _DigitalRails ) -> [];
verify_list( ListCount , [ Car | MoreCars] , Graph , Name , MainPID, DigitalRails ) ->

	Element = create_person( Car , Graph, DigitalRails ),
	[ Element | verify_list( ListCount + 1 , MoreCars , Graph , Name , MainPID, DigitalRails ) ].

create_person( Car , Graph, DigitalRails ) ->
	{ Origin , Destination , CarCount , ST , LinkOrigin , Type , Mode , NameFile , Park, TrafficModel } = Car,
    { STInteger , _ } = string:to_integer( ST ),

	ModeFinal = case Mode of
		ok ->
			car; % if the mode is not set in the input file, "car" is the default value.
		_ ->
			list_to_atom( Mode ) % Otherwise, car or walk.
	end,

	DrEdges = create_dr_edge_list( DigitalRails ),
	NewPath = case Destination of 
		"random_walk" -> digital_rails_random_walk(Graph, list_to_atom(Origin), false, [], 5, DrEdges);
		_ -> digraph:get_short_path( Graph , list_to_atom(Origin) , list_to_atom(Destination) )
	end,

	% io:format("Path: ~p ~n", [NewPath]),

	ListTripsFinal = [ { ModeFinal , NewPath , LinkOrigin } ],

	TrafficModelAtom = case TrafficModel of
		ok -> ok;
		_ -> list_to_atom(TrafficModel)
	end,

	{ STInteger , [ { NameFile , ListTripsFinal , Type , Park , ModeFinal , element (1 , string:to_integer(CarCount)), TrafficModelAtom } ] }.

random_element(List) -> lists:nth(rand:uniform(length(List)), List).

digital_rails_random_walk(_Graph, Origin, _UsedDigitalRails, _RestrictedLinks, 0, _) ->
	[Origin];

digital_rails_random_walk(Graph, Origin, UsedDigitalRails, RestrictedLinks, RemainingLinks, DrEdges) ->
	AllOutboundEdges = lists:map(fun(E) -> digraph:edge(Graph, E) end, digraph:out_edges(Graph, Origin)),
	AllowedOutboundEdges = lists:filter(fun (E) -> not lists:member(element(1, element(4, E)), RestrictedLinks) end, AllOutboundEdges),

	DigitalRailsOutboundEdges = lists:filter(fun (E) -> is_edge_digital_rail(E, DrEdges) end, AllowedOutboundEdges),
	RegularOutboundEdges = lists:filter(fun (E) -> not is_edge_digital_rail(E, DrEdges) end, AllowedOutboundEdges),

	case length(DigitalRailsOutboundEdges) == 0 of
		true -> 
			{_, _, Destination, {_, _, _, _, _, RestrictedNextLinks}} = random_element(RegularOutboundEdges),
			case UsedDigitalRails of
				true ->
					% io:format("No outbound Digital rails and already used DR, ending trip (~p) ~n", [RegularOutboundEdges]),
					[Origin, Destination];
				false ->
					% io:format("No outbound Digital rails and have not used DR (~p) ~n", [RegularOutboundEdges]),
					[Origin] ++ digital_rails_random_walk(Graph, Destination, false, RestrictedNextLinks, RemainingLinks - 1, DrEdges)
				end;
		false -> 
			case length(RegularOutboundEdges) == 0 of
				true -> 
					% io:format("No choice but to stay in digital rails (~p) ~n", DigitalRailsOutboundEdges),
					{_, _, Destination, {_, _, _, _, _, RestrictedNextLinks}} = lists:nth(1, DigitalRailsOutboundEdges),
					[Origin] ++ digital_rails_random_walk(Graph, Destination, true, RestrictedNextLinks, RemainingLinks, DrEdges);
				false -> 
					StayStraightProbability = case UsedDigitalRails of
						true -> 0.9;
						false -> 0.70
					end,

					case rand:uniform() < StayStraightProbability of
						true -> 
							% io:format("Remaining in digital rails (~p) ~n", DigitalRailsOutboundEdges),
							{_, _, Destination, {_, _, _, _, _, RestrictedNextLinks}} = lists:nth(1, DigitalRailsOutboundEdges),
							[Origin] ++ digital_rails_random_walk(Graph, Destination, true, RestrictedNextLinks, RemainingLinks, DrEdges);
						false -> 
							% io:format("Leaving digital rails and ending trip (~p) ~n", [RegularOutboundEdges]),
							{_, _, Destination, _} = random_element(RegularOutboundEdges),
							[Origin, Destination]
					end
			end
	end.

create_dr_edge_list([]) -> [];

create_dr_edge_list([{rail, [{cycle, _}], [{links, Links}]} | Rails]) ->
	filter_dr_links(Links) ++ create_dr_edge_list(Rails);
	
create_dr_edge_list([_ | Rails]) ->
	create_dr_edge_list(Rails).

filter_dr_links([]) -> [];
filter_dr_links([{link, [{origin, Origin}, {destination, Destination}], _} | Links]) ->
	[{Origin, Destination}] ++  filter_dr_links(Links);

filter_dr_links([{link, [{origin, Origin}, {destination, Destination}, {signalized, _}, {offset, _}], _} | Links]) ->
	[{Origin, Destination}] ++  filter_dr_links(Links);

filter_dr_links([_ | Links]) ->
	filter_dr_links(Links).

is_edge_digital_rail({_, Origin, Destination, _}, DrEdges) ->
	lists:member({atom_to_list(Origin), atom_to_list(Destination)}, DrEdges).