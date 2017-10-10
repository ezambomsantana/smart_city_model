-module(platform_request).

-include_lib("../deps/amqp_client/include/amqp_client.hrl").

-export([
         start_server/0, make_call/2, receive_park/2, verify_park_by_actor_name/2, call_parking_service/4
        ]).

start_server( ) ->
	
	receive
		{ make_call , ActorName , Coordinates } -> make_call( ActorName , Coordinates );
		{ receive_park, ActorName , Park } -> receive_park( ActorName , Park );
		{ verify_park_by_actor_name , ActorName , PID } -> verify_park_by_actor_name( ActorName , PID )
	end,
	start_server().

make_call( ActorName , Coordinates ) ->
	spawn(platform_request, call_parking_service , [ ActorName , Coordinates , self() , 500 ]).

receive_park( ActorName , Park ) ->
        put (ActorName , Park ).

verify_park_by_actor_name( ActorName , PID ) ->

       Park = get( ActorName ),       case Park of
	    undefined -> PID ! { nok };
	    _ -> erase( ActorName ), PID !  { ok , ActorName , Park }
       end.

		
call_parking_service( ActorName , Coordinates , PID , Radius ) ->
    inets:start(),


    URL = "http://172.19.66.212:8000/discovery/resources?capability=parking_monitoring;lat=" ++ element( 1 , Coordinates ) ++ ";lon=" ++ element( 2 , Coordinates ) ++ ";radius=" ++ integer_to_list( Radius ) ++ ";available.eq=true",


   {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
    httpc:request(get, {URL, []}, [], []),

  case length(Body) < 30 of
	true -> call_parking_service( ActorName , Coordinates , PID , Radius * 2 );
	false -> 
		Park = string:sub_string(Body, 24, 59),
		PID ! { receive_park , ActorName , Park }
   end.

  
