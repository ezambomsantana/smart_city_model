-module(trip_parser).
-include_lib("xmerl/include/xmerl.hrl").

% usage:
%
% l(trip_parser).
% trip_parser:show("trips.xml").

-export([
         show/1
        ]).

show( FileName ) ->
    {Doc, _Misc} = xmerl_scan:file(FileName),
    init(Doc).

% read the XML and extract all trip elements
init(Node) ->
    case Node of
        #xmlElement{name=Name, content=Content} ->
            case Name of
                scsimulator_matrix -> 
                    List = extract_trips(Content, []),
                    List;
                _ -> 
                    []
            end;
        _ -> 
            []
    end.

extract_trips([], List) ->
    List;
extract_trips([Node | MoreNodes], List) ->
    Element = extract_trip(Node),
    case Element of
        ok ->
            extract_trips(MoreNodes, List);
        _ ->
            extract_trips(MoreNodes, [Element | List])
    end.

extract_trip(Node) ->
    case Node of
        #xmlElement{name=Name, attributes=Attributes} ->
            case Name of
                trip ->
                    Name_attr = get_attribute(Attributes, name),
                    Origin = get_attribute(Attributes, origin),
                    Destination = get_attribute(Attributes, destination),
                    LinkOrigin = get_attribute(Attributes, link_origin),
                    CarCount = get_attribute(Attributes, count),
                    Start = get_attribute(Attributes, start),
                    Mode = get_attribute(Attributes, mode),
                    Type = ok,
                    Park = ok,
                    Uuid = get_attribute(Attributes, uuid),
                    {Origin, Destination, CarCount, Start, LinkOrigin, Type, Mode, Name_attr, Park, Uuid};
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

get_attribute([], _Name) ->
    "";
get_attribute([#xmlAttribute{name=Name, value=Value} | _Rest], Name) ->
    Value;
get_attribute([_ | Rest], Name) ->
    get_attribute(Rest, Name).
