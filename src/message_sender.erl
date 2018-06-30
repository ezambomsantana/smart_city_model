-module(message_sender).

-include_lib("../deps/amqp_client/include/amqp_client.hrl").

-export([ publish_data/0 ]).

setup() ->
	{ _ , Pwd } = file:get_cwd(),
	AmqpClientPath = string:concat( Pwd, "/../deps/amqp_client"),

	Paths = [ AmqpClientPath,
			  string:concat( AmqpClientPath, "/ebin" ),
			  string:concat( AmqpClientPath, "/include/rabbit_common/ebin" )
			],

	code:add_pathsa( Paths ).

publish_data() ->
	setup(),

	timer:sleep(1000),

	Hostname = os:getenv("RABBITMQ_HOST", "localhost"),
	{ok, Connection} = amqp_connection:start(#amqp_params_network{host=Hostname}),

	loop( Connection ).

send( Connection, Topic, RoutingKey, Message ) ->
	{ ok, Channel } = amqp_connection:open_channel( Connection ),

	Exchange = #'exchange.declare'{ exchange = list_to_binary( Topic ),
									type = <<"topic">> },
	#'exchange.declare_ok'{} = amqp_channel:call( Channel, Exchange ),

	Publish = #'basic.publish'{ exchange = list_to_binary( Topic ),
								routing_key = list_to_binary( RoutingKey ) },

	amqp_channel:cast( Channel,
					   Publish,
					   #amqp_msg{ payload = list_to_binary( Message ) }).

loop( Connection ) ->
	receive
		{ send_data, Topic, RoutingKey, Message } -> send( Connection, Topic, RoutingKey, Message )
	end,
	loop( Connection ).
