-module(platform_request).

-export([
         call_parking_service/1
        ]).
		
call_parking_service( Coordinates ) ->


    URL = "http://172.19.66.212:8000/discovery/resources?capability=parking_monitoring;lat=" ++ element( 1 , Coordinates ) ++ ";lon=" ++ element( 2 , Coordinates ) ++ ";radius=5000;available.eq=true",

    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
      httpc:request(get, {URL, []}, [], []),

    string:sub_string(Body, 24, 59).
