-module(events_handler).

-include_lib("../deps/amqp_client/include/amqp_client.hrl").

-export([listen_for_events/0]).

setup() ->
	{ _ , Pwd } = file:get_cwd(),
	AmqpClientPath = string:concat( Pwd, "/../deps/amqp_client"),

	Paths = [ AmqpClientPath,
			string:concat( AmqpClientPath, "/ebin" ),
			string:concat( AmqpClientPath, "/include/rabbit_common/ebin" )
		],

    code:add_pathsa( Paths ).


listen_for_events() ->
	setup(),
	timer:sleep(1000),

	Hostname = os:getenv( "RABBITMQ_HOST", "localhost" ),
	{ok, Connection} = amqp_connection:start(#amqp_params_network{host=Hostname}),
	{ok, Channel} = amqp_connection:open_channel(Connection),

	X = <<"traffic_sign">>,
	BindKey = <<"#">>,

	#'queue.declare_ok'{queue = Q} = amqp_channel:call(Channel, #'queue.declare'{}),

	ExchangeDeclare = #'exchange.declare'{exchange = X, type = <<"topic">>},
	#'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

	QueueBind = #'queue.bind'{queue = Q,
				  exchange = X,
				  routing_key = BindKey},
	#'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),

	Sub = #'basic.consume'{queue = Q, no_ack = true},
	#'basic.consume_ok'{} = amqp_channel:call(Channel, Sub),

	loop(Channel).


loop(Channel) ->
	receive
		{#'basic.deliver'{ exchange=_Exchange, routing_key=_RoutingKey}, Content} ->
			#amqp_msg{payload = Payload} = Content,
			[ CurrentNode, FromNode, ToNode ] = string:tokens( binary_to_list(Payload), "." ),
			io:format("Current Node: ~p~nFrom Node: ~p~nTo Node: ~p~n", [CurrentNode, FromNode, ToNode]),
			ets:insert(traffic_events, { CurrentNode, { FromNode, ToNode } }),
			loop(Channel)
	end.
