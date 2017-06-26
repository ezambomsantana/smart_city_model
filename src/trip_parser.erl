-module(trip_parser).
-include_lib("xmerl/include/xmerl.hrl").

% usage:
%
% l(osm_parser).
% osm_parser:show("map.osm").

-export([
         show/1
        ]).

% Init the XML processing
show(Infilename) ->
    {Doc, _Misc} = xmerl_scan:file(Infilename),
    init(Doc).

% read the OSM tag and extract all children
init(Node) ->
    case Node of
        #xmlElement{name=Name, content=Content} ->
            
	    case Name of
		
		scsimulator_matrix -> 

			List = trips(Content , [] , false),
			List;

		_ -> ok

	    end;
            _ -> ok
    end.

trips([], List , _Multi ) ->
    List;

trips([Node | MoreNodes] , List , Multi ) ->
    Element = extract_node( Node , Multi ),
    case Element of

	ok ->
    		
		trips( MoreNodes , List , Multi);

	_ ->
		NewList = List ++ Element,
		trips( MoreNodes , NewList , Multi)

    end.

%
% Show a node/element and then the children of that node.
extract_node( Node , Multi ) ->

    case Node of
        #xmlElement{ name=Name , content=Content , attributes=Attributes } ->
            
	    case Name of
		
		trip -> 
			
			case Multi of 

				false ->
			
					Origin = children( Attributes , origin ),
					Destination = children( Attributes , destination ),
					Count = children( Attributes , count ),
					StartTime = children( Attributes , start ),
					LinkOrigin = children( Attributes , link_origin ),
					Type = children( Attributes , type ),
					Mode = children( Attributes , mode ),
					[ { Origin , Destination , Count , StartTime , LinkOrigin , Type , Mode } ];

				true ->

					Origin = children( Attributes , origin ),
					Destination = children( Attributes , destination ),
					LinkOrigin = children( Attributes , link_origin ),
					LinkDestination = children( Attributes , link_destination ),
					Mode = children( Attributes , mode ),
					Line = children( Attributes , line ),
					[ { Origin , Destination , LinkOrigin , Mode , LinkDestination , Line } ]
					
			end;
			
		multi_trip ->

			List = trips( Content , [] , true),
			Count = children( Attributes , count ),
			StartTime = children( Attributes , start ),
			Type = children( Attributes , type ),
			[ { StartTime , Type , Count , List } ];
			

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
