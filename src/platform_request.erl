-module(platform_request).

-include_lib("../deps/amqp_client/include/amqp_client.hrl").

-export([
         start_server/0, make_call/2, receive_park/2, verify_park_by_actor_name/2, call_parking_service/3
        ]).

start_server( ) ->

	receive
		{ make_call , ActorName , Coordinates } -> make_call( ActorName , Coordinates );
		{ receive_park, ActorName , Park } -> receive_park( ActorName , Park );
		{ verify_park_by_actor_name , ActorName , PID } -> verify_park_by_actor_name( ActorName , PID )
	end,
	start_server().

make_call( ActorName , Coordinates ) ->
	spawn(platform_request, call_parking_service , [ ActorName , Coordinates , self() ]).

receive_park( ActorName , Park ) ->
        put (ActorName , Park ).

verify_park_by_actor_name( ActorName , PID ) ->
       Park = get( ActorName ),
       case Park of
	    undefined -> PID ! { nok };
	    _ -> erase( ActorName ), PID !  { ok , ActorName , Park }
       end.

		
call_parking_service( ActorName , _Coordinates , PID ) ->
	PID ! { receive_park , ActorName , "teste" }.
%    URL = "http://172.19.66.212:8000/discovery/resources?capability=parking_monitoring;lat=" ++ element( 1 , Coordinates ) ++ ";lon=" ++ element( 2 , Coordinates ) ++ ";radius=5000;available.eq=true",

 %   {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
  %    httpc:request(get, {URL, []}, [], []),

   %string:sub_string(Body, 24, 59).
