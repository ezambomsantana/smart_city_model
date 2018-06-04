-module(events_handler).

-include_lib("../deps/amqp_client/include/amqp_client.hrl").

-export([listen_for_events/0, test/0]).

setup() ->
	{ _ , Pwd } = file:get_cwd(),
	AmqpClientPath = string:concat( Pwd, "/../deps/amqp_client"),

	Paths = [ AmqpClientPath,
			string:concat( AmqpClientPath, "/ebin" ),
			string:concat( AmqpClientPath, "/include/rabbit_common/ebin" )
		],

    code:add_pathsa( Paths ).

test() ->
    setup(),

    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    X = <<"traffic_sign">>,
    BindKey = <<"#">>,

    #'queue.declare_ok'{queue = Q}
        = amqp_channel:call(Channel, #'queue.declare'{queue = <<"my_queue">>}),

    ExchangeDeclare = #'exchange.declare'{exchange = X, type = <<"topic">>},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

    QueueBind = #'queue.bind'{queue = Q,
                              exchange = X,
                              routing_key = BindKey},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, QueueBind),

    Payload = <<"2.3.5">>,
    RoutingKey = <<"test.traffic.event">>,
    Publish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload}),
    io:format("ENVIADO A PRIMEIRA MSG\n"),

    Payload1 = <<"2.3.5">>,
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Payload1}),
    io:format("ENVIADO A SEGUNDA MSG\n"),
    
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),

    ok.

listen_for_events() ->
	setup(),

	Hostname = os:getenv("RABBITMQ_HOST", "localhost"),
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
        {#'basic.deliver'{ exchange=_Exchange, routing_key=RoutingKey}, Content} ->
            io:format("RoutingKey received: ~p~n", [RoutingKey]),
            #amqp_msg{payload = Payload} = Content,
            [ CurrentNode, FromNode, ToNode ] = string:tokens( binary_to_list(Payload), "." ),
            io:format("Payload received: ~p~n", [Payload]),
            io:format("Current Node: ~p~nFrom Node: ~p~nTo Node: ~p~n", [CurrentNode, FromNode, ToNode]),
            ets:insert(traffic_events, { list_to_atom(CurrentNode), { list_to_atom(FromNode), list_to_atom(ToNode) } }),
            loop(Channel)
    end.
