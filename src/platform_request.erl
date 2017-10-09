-module(platform_request).

-include_lib("../deps/amqp_client/include/amqp_client.hrl").

-export([
         start_service/3, get_data_park/0
        ]).


start_service ( ActorName , CarCoordinates , Channel ) ->

	inets:start(),

	{ _ , Pwd } = file:get_cwd(),
	AmqpClientPath = string:concat( Pwd, "/../deps/amqp_client"),

    	code:add_pathsa( [ AmqpClientPath , string:concat( AmqpClientPath, "/ebin" ), 
				string:concat( AmqpClientPath, "/include/rabbit_common/ebin" ) ] ),
	Park = call_parking_service( CarCoordinates , 5000 ),


	publish_data( ActorName , Park , "data_stream" , Channel ).

publish_data( ActorName , Park , _Topic , Channel ) ->

	Exchange = #'exchange.declare'{ exchange = <<"simulator_exchange">>,
                                    type = <<"topic">> },

	#'exchange.declare_ok'{} = amqp_channel:call( Channel, Exchange ),

	Publish = #'basic.publish'{ exchange = <<"simulator_exchange">>,
                                routing_key = list_to_binary( ActorName ) },

	amqp_channel:cast( Channel,
					   Publish,
					   #amqp_msg{ payload = list_to_binary( Park ) }).


		
call_parking_service( Coordinates , Radius ) ->
    URL = "http://172.19.66.212:8000/discovery/resources?capability=parking_monitoring;lat=" ++ element( 1 , Coordinates ) ++ ";lon=" ++ element( 2 , Coordinates ) ++ ";radius=" ++ integer_to_list( Radius ) ++ ";available.eq=true",

    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
      httpc:request(get, {URL, []}, [], []),

   case length( Body ) < 30 of
	true -> call_parking_service( Coordinates , Radius * 2 );
	false -> string:sub_string(Body, 24, 59)
   end.





get_data_park( ) ->

    receive
        {#'basic.deliver'{routing_key = _RoutingKey}, #amqp_msg{payload = Body}} ->
            binary_to_list( Body );
	_ ->
            nok
    end.

