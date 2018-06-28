-module(map_parser).
-include_lib("xmerl/include/xmerl.hrl").

% usage:
%
% l(osm_parser).
% osm_parser:show("map.osm").

-export([
         show/3
        ]).

% Init the XML processing
show(Infilename , Print, GraphManagerPid) ->
    {Doc, _Misc} = xmerl_scan:file(Infilename),
    Graph = digraph:new(),
    init( Doc , Graph, GraphManagerPid ),
    case Print of 
	true ->
    		print_graph( Graph );
	_ ->
		ok
    end,
    Graph.

% read the OSM tag and extract all children
init( Node, Graph, GraphManagerPid ) ->
    case Node of
        #xmlElement{name=Name, content=Content} ->
            
	    case Name of
		
		network -> 

			nodes_links(Content , Graph, GraphManagerPid);
			
		_ -> ok

	    end;
            _ -> ok
    end.

nodes_links([] , _Graph, _GraphManagerPid) ->
    ok;

nodes_links([Node | MoreNodes], Graph, GraphManagerPid) ->
    extract_nodes( Node , Graph, GraphManagerPid ),
    nodes_links(MoreNodes , Graph, GraphManagerPid ).

%
% Show a node/element and then the children of that node.
extract_nodes(Node , Graph, GraphManagerPid ) ->

    case Node of
        #xmlElement{name=Name, content=Content} ->
            
	    case Name of
		
		nodes -> 
			
			get_nodes(Content , Graph, GraphManagerPid );

		links ->

			get_links(Content , Graph, GraphManagerPid );

		_ ->
			ok
	    end;

            _ -> ok
    end.

get_nodes([] , _Graph, _GraphManagerPid ) ->
    ok;

get_nodes([Node | MoreNodes] , Graph, GraphManagerPid ) ->
    extract_node(Node , Graph, GraphManagerPid ),
    get_nodes( MoreNodes , Graph, GraphManagerPid ).


get_links([] , _Graph, _GraphManagerPid ) ->
    ok;

get_links([ Link | MoreLinks ] , Graph, GraphManagerPid ) ->
    extract_link(Link , Graph, GraphManagerPid ),
    get_links( MoreLinks , Graph, GraphManagerPid ).

% Show a node/element and then the children of that node.
extract_node(Node , Graph, GraphManagerPid ) ->

    case Node of
        #xmlElement{name=Name, attributes=Attributes} ->
            
	    case Name of
		
		node -> 
			
			Id = children( Attributes , id ),
			Lat = children( Attributes , x ),
			Long = children( Attributes , y ),	
			GraphManagerPid ! { add_vertex, Id },
			digraph:add_vertex(Graph, list_to_atom(Id), { Lat , Long });	

		_ ->
			ok
	    end;

            _ -> ok
    end.

extract_link(Link , Graph, GraphManagerPid ) ->

    case Link of
        #xmlElement{name=Name, attributes=Attributes} ->
            
	    case Name of
		
		link -> 
			
			Id = children( Attributes , id ),	
			From = children( Attributes , from ),
			To = children( Attributes , to ),
			Length = children( Attributes , length ),
			Capacity = children ( Attributes , capacity ),
			Freespeed = children( Attributes , freespeed ),
			GraphManagerPid ! { add_edge, list_to_atom( From ), list_to_atom( To ) },
			digraph:add_edge(Graph, list_to_atom(From), list_to_atom(To), { Id , Length , Capacity , Freespeed });

		_ ->
			ok
	    end;

            _ -> ok
    end.


children( [] , _Type ) ->
    ok;

children( [Node | MoreNodes] , Type ) ->
    Element = extract_children( Node , Type ),
    case Element of

	ok ->
    		
		children( MoreNodes , Type );

	_ ->
		Element

    end.


extract_children( Node , Type ) ->

    case Node of
        #xmlAttribute{name=Name, value=Value} ->
            
	    case Name of
		
		Type -> 

			Value;

		_ -> ok

	    end;
            _ -> ok
    end.

print_graph( Graph ) ->
	
	Vertices = digraph:vertices( Graph ),
	Edges = digraph:edges( Graph ),
	print_vertices( Vertices  ),
	print_edges( Graph , Edges ).



print_vertices([]) ->
	ok;

print_vertices([Element | MoreElements]) ->
	io:format("vertice: ~s~n", [ Element ]),
	print_vertices(MoreElements).



print_edges(_Graph , []) ->
	ok;

print_edges(Graph, [Element | MoreElements]) ->
	Edge = digraph:edge(Graph, Element),
	io:format("vInicio: ~s~n", [ element( 2 , Edge ) ]),
	io:format("vFim: ~s~n", [ element( 3 , Edge) ]),
	io:format("id: ~s~n", [ element( 4 , Edge) ]),
	print_edges( Graph , MoreElements ).
