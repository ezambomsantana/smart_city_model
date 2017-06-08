-module(config_parser).
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
		
		scsimulator_config -> 

			List = config(Content , []),
			List;

		_ -> ok

	    end;
            _ -> ok
    end.

config([], List) ->
    List;

config([Node | MoreNodes], List) ->
    Element = extract_node(Node),
    case Element of

	ok ->
    		
		config(MoreNodes , List);

	_ ->
		Element

    end.

%
% Show a node/element and then the children of that node.
extract_node(Node) ->

    case Node of
        #xmlElement{name=Name, attributes=Attributes} ->
            
	    case Name of
		
		config -> 
			
			OutputFile = children( Attributes , output_file ),
			SimulationTime = children( Attributes , simulation_time ),
			MapFile = children( Attributes , map_file ),
			TripFile = children( Attributes , trip_file ),
			MetroFile = children( Attributes , metro_file ),
			{ OutputFile , SimulationTime , MapFile , TripFile , MetroFile };

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
