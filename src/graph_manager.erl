-module(graph_manager).
-export([init/0]).

init() ->
    ets:new( graph, [public, set, named_table]),
    ets:insert( graph, { mypid, self() }),
    G = digraph:new(),
    ets:insert( graph, { mygraph, G }),
    loop(G).

add_edge(G, IdV1, IdV2) ->
    [ { _, V1 } ] = ets:lookup( graph, atom_to_list( IdV1 ) ), 
    [ { _, V2 } ] = ets:lookup( graph, atom_to_list( IdV2 ) ), 
    digraph:add_edge(G, V1, V2).

add_vertex(G, Id) ->
    V = digraph:add_vertex(G),
    Vertex = digraph:add_vertex( G, V, { Id } ),
    ets:insert( graph, { Id, Vertex } ),
    ets:insert( graph, { Vertex, Id } ).

find_edge( Graph, V1, V2 ) ->
    EdgesList = digraph:out_edges( Graph, V1 ),
    iterate_edges( Graph, EdgesList, V2 ).

iterate_edges( _Graph, [], _V ) ->
    ok;
iterate_edges( Graph, [Edge|EdgesList], V ) ->
    { Vertex, _ } = digraph:vertex( Graph, V ),
    EdgeToCheck = digraph:edge( Graph, Edge ),

    case EdgeToCheck of
        { Edge, _, Vertex, _ } -> EdgeToCheck;
        _ -> iterate_edges( Graph, EdgesList, V )
    end.

delete_edge( G, IdV1, IdV2 ) ->
    [ { _, V1 } ] = ets:lookup( graph, atom_to_list( IdV1 ) ), 
    [ { _, V2 } ] = ets:lookup( graph, atom_to_list( IdV2 ) ), 
    { Edge, _, _, _ } = find_edge( G, V1, V2 ),
    digraph:del_edge( G, Edge ).

print_graph_edges(G) ->
    io:format("Edges: ~p ~n", [digraph:edges(G)]).

print_graph_vertices(G) ->
    io:format("Vertices: ~p ~n", [digraph:vertices(G)]).

get_vertices_ids( [], Ids ) -> Ids;
get_vertices_ids( [ Vertex | List], Ids ) ->
    [ { _, Id } ] = ets:lookup( graph, Vertex ),
    NewIds = lists:append( [ list_to_atom(Id) ], Ids ),
    get_vertices_ids( List, NewIds ).

get_shortest_path( G, IdV1, IdV2, PID ) ->
    [ { _, V1 } ] = ets:lookup( graph, atom_to_list( IdV1 ) ), 
    [ { _, V2 } ] = ets:lookup( graph, atom_to_list( IdV2 ) ), 
    Path = digraph:get_short_path( G, V1, V2 ),
    Ids = get_vertices_ids( Path, [] ),
    PID ! { best_path, lists:reverse(Ids) }.

loop(G) ->
    receive
        { add_vertex, Id }                 -> add_vertex(G, Id);
        { add_edge, IdV1, IdV2 }           -> add_edge(G, IdV1, IdV2);
        { delete_edge, IdV1, IdV2}         -> delete_edge(G, IdV1, IdV2);
        { print_graph_edges }              -> print_graph_edges(G);
        { print_graph_vertices }           -> print_graph_vertices(G);
	{ get_best_path, IdV1, IdV2, PID } -> get_shortest_path( G, IdV1, IdV2, PID )
    end,
    loop(G).
