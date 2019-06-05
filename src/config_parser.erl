-module(config_parser).
-include_lib("xmerl/include/xmerl.hrl").

% usage:
%
% l(osm_parser).
% osm_parser:show("map.osm").

-export([show/1]).

% Init the XML processing
show(Infilename) ->
  {Doc, _Misc} = xmerl_scan:file(Infilename),
  init(Doc).

% read the OSM tag and extract all children
init(#xmlElement{name=scsimulator_config, content=Content}) -> config(Content, []);
init(_) -> ok.

config([], List) -> List;
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
extract_node(#xmlElement{name=config, attributes=Attributes}) ->
	OutputFile = children( Attributes , output_file ),
	SimulationTime = children( Attributes , simulation_time ),
	MapFile = children( Attributes , map_file ),
	TripFile = children( Attributes , trip_file ),
	MetroFile = children( Attributes , metro_file ),
	BusFile = children( Attributes , bus_file ),
	ParkFile = children( Attributes , park_file ),
	TrafficSignalsFile = children( Attributes , traffic_signals_file ),
	DigitalRailsFile = children( Attributes , digital_rails_file ),
	GenerateGraph = children( Attributes , generate_graph ),
	{ OutputFile , SimulationTime , MapFile , TripFile , MetroFile , BusFile , ParkFile , TrafficSignalsFile, DigitalRailsFile, GenerateGraph };
extract_node(_) -> ok.

children( [Node | MoreNodes] , Type ) ->
	Element = extract_children( Node , Type ),
	case Element of
		ok -> children( MoreNodes , Type );
		_ -> Element
	end;
children( [] , _Type ) -> ok.

extract_children(#xmlAttribute{name=Type, value=Value}, Type) -> Value;
extract_children(_, _) -> ok.