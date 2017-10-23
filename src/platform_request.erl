-module(platform_request).

-include_lib("../deps/amqp_client/include/amqp_client.hrl").

-export([
         start_server/0, make_call/3, receive_park/2, verify_park_by_actor_name/2, call_parking_service/5, init_file_service/0, save_timestamp/3
        ]).

start_server( ) ->
	
	FileServicePID = spawn(platform_request, init_file_service , [ ]),
	run_server( FileServicePID ).

run_server( FileServicePID ) ->

	receive
% estacionamento
		{ make_call , ActorName , Coordinates } -> make_call( ActorName , Coordinates , FileServicePID );
		{ receive_park, ActorName , Park } -> receive_park( ActorName , Park );
		{ verify_park_by_actor_name , ActorName , PID } -> verify_park_by_actor_name( ActorName , PID )
% saude
	end,
	run_server( FileServicePID ).

make_call( ActorName , Coordinates , FileServicePID ) ->
	spawn(platform_request, call_parking_service , [ ActorName , Coordinates , self() , 500 , FileServicePID ]).

receive_park( ActorName , Park ) ->
        put (ActorName , Park ).

verify_park_by_actor_name( ActorName , PID ) ->

       Park = get( ActorName ),
       case Park of
	    undefined -> PID ! { nok };
	    _ -> erase( ActorName ), PID !  { ok , ActorName , Park }
       end.

		
call_parking_service( ActorName , Coordinates , PID , Radius , FileServicePID ) ->

    inets:start(),

    { { Year, Month, Day }, { Hour, Minute, Second } } = calendar:local_time(),
    FirstTimestamp = lists:flatten( io_lib:format( "~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
                                          [ Year, Month, Day, Hour, Minute, Second ] ) ),

    URL = "http://kong-proxy:8000/discovery/resources?capability=parking_monitoring;lat=" ++ element( 1 , Coordinates ) ++ ";lon=" ++ element( 2 , Coordinates ) ++ ";radius=" ++ integer_to_list( Radius ) ++ ";available.eq=true",

  case httpc:request(get, {URL, []}, [], []) of
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
   { { Year2, Month2, Day2 }, { Hour2, Minute2, Second2 } } = calendar:local_time(),
    SecondTimestamp = lists:flatten( io_lib:format( "~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
                                          [ Year2, Month2, Day2, Hour2, Minute2, Second2 ] ) ),
 

		  case length(Body) < 30 of
			true -> 
				FileServicePID ! { save_timestamp , FirstTimestamp , SecondTimestamp },
				call_parking_service( ActorName , Coordinates , PID , Radius * 2 , FileServicePID );
			false -> 
				FileServicePID ! { save_timestamp , FirstTimestamp , SecondTimestamp },
				Park = string:sub_string(Body, 24, 59),
				PID ! { receive_park , ActorName , Park }
		   end;

	_ ->
   { { Year2, Month2, Day2 }, { Hour2, Minute2, Second2 } } = calendar:local_time(),
    SecondTimestamp = lists:flatten( io_lib:format( "~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
                                          [ Year2, Month2, Day2, Hour2, Minute2, Second2 ] ) ),
  		
FileServicePID ! { save_error , FirstTimestamp , SecondTimestamp }
  end.


  

init_file_service() ->

	File = file_utils:open( "../platform/response_time.csv" , _Opts=[ append, delayed_write ] ),
        put ( file , File ),
	run_file_service().

run_file_service() ->
	receive
		{ save_timestamp , FirstTimestamp , SecondTimestamp } -> save_timestamp( "success" , FirstTimestamp , SecondTimestamp );
		{ save_error , FirstTimestamp , SecondTimestamp } -> save_timestamp( "error" , FirstTimestamp , SecondTimestamp )
	end,
	run_file_service( ).


save_timestamp( Result , FirstTimestamp , SecondTimestamp ) ->

	File = get( file ),	

	file_utils:write( File, "~s,~s,~s\n" , [ Result , FirstTimestamp , SecondTimestamp ] ).

