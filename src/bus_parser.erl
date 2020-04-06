-module(bus_parser).
-include_lib("xmerl/include/xmerl.hrl").

-export([
         show/1
        ]).

% Init the XML processing
show(Infilename) ->
    {Doc, _Misc} = xmerl_scan:file(Infilename),
    Return = init(Doc),
    case Return of
		ok -> [ ];
		_-> Return
    end.

% read the OSM tag and extract all children
init(Node) ->
    case Node of
        #xmlElement{name=Name, content=Content} ->
            
	    case Name of
		
		scsimulator_buses -> 

			List = buses(Content , [] ),
			List;

		_ -> ok

	    end;
            _ -> ok
    end.

buses([], List ) ->
    List;

buses([Node | MoreNodes] , List ) ->
    Element = extract_node( Node ),
    case Element of

	ok ->
    		
		buses( MoreNodes , List );

	_ ->
		NewList = List ++ Element,
		buses( MoreNodes , NewList )

    end.

%
% Show a node/element and then the children of that node.
extract_node( Node ) ->

    case Node of
        #xmlElement{ name=Name , content=_Content , attributes=Attributes } ->
            
	    case Name of
		
		bus -> 
			
				Id = children( Attributes , id ),
				Interval = children( Attributes , interval ),
				Stops = string:tokens( children( Attributes , stops ) , "," ),
				StartTime = children( Attributes , start_time ),
				[ { Id , element( 1 , string:to_integer( Interval ) ) , Stops , StartTime } ]; % method to_integer returns two elements.
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
