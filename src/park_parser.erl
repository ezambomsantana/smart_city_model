-module(park_parser).
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
		
		park -> 

			List = spots(Content , [] ),
			List;

		_ -> ok

	    end;
            _ -> ok
    end.

spots( [] , List ) ->
    List;

spots([Node | MoreNodes] , List ) ->

    Element = extract_node( Node ),

    case Element of

	ok ->
    		
		spots( MoreNodes , List );

	_ ->
		NewList = List ++ Element,
		spots( MoreNodes , NewList )

    end.

extract_node( Node ) ->

    case Node of
        #xmlElement{ name=Name , attributes=Attributes } ->
            
	    case Name of
					
		spot ->

			Uuid = children( Attributes , uuid ),
			NodeId = children( Attributes , node ),
			Coordinates = { children( Attributes , lat ) , children( Attributes , lon ) },
			[ { Uuid , { NodeId , Coordinates } } ];	

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
