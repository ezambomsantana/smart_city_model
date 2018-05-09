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
  ets:insert( graph, { Id, V } ).

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

loop(G) ->
  receive
    { add_vertex, Id }         -> add_vertex(G, Id);
    { add_edge, IdV1, IdV2 }   -> add_edge(G, IdV1, IdV2);
    { delete_edge, IdV1, IdV2} -> delete_edge(G, IdV1, IdV2);
    { print_graph_edges }      -> print_graph_edges(G);
    { print_graph_vertices }   -> print_graph_vertices(G)
  end,
  loop(G).
